# Methodology & Project Walkthrough
---

## 1. Objective and logic of the project

The project builds a **compliance risk score for individual Italian firms** based on
how exposed they are — through their sector's trade flows — to *geopolitically risky
countries*. The motivating use case is **sanctions and trade-compliance due
diligence**: a firm operating in a sector that sources heavily from sanctioned or
"sanctions-evasion" jurisdictions carries higher compliance risk, even if the firm
itself has done nothing wrong, because its supply chain requires enhanced scrutiny.

The pipeline is deliberately split into two layers so that **country-level risk is
estimated independently of firms**, and only afterwards mapped onto sectors and firms:

- **Layer 1 — Country risk.** Turn many heterogeneous country indicators
  (governance, logistics, sovereign credit, sanctions, evasion) into a single,
  interpretable risk score per country, plus a small number of country *clusters*.
- **Layer 2 — Firm compliance risk.** Use Italian trade-flow data to measure, for
  each economic sector, how much of its trade is with high-risk countries; combine
  this into a sector compliance score; assign that score to firms via their industry
  code.

This separation is a standard modelling choice: it keeps the country-risk estimation
**data-driven and reusable** (it does not depend on the firm sample), and it makes the
firm score transparent — every firm's score can be traced back to its sector and to
the specific countries driving that sector's exposure.

---

## 2. The data (what each economic source actually is)

All raw inputs live in `data/raw/`. The table below explains each one in plain terms.

| File | Source | What it measures | Scale / unit |
|------|--------|------------------|--------------|
| `wgi.csv` | World Bank **Worldwide Governance Indicators** | Institutional quality on five dimensions: voice & accountability, government effectiveness, regulatory quality, rule of law, control of corruption | roughly −2.5 (weak) to +2.5 (strong) |
| `lpi.csv` | World Bank **Logistics Performance Index** | How reliable and efficient a country's logistics are (customs, infrastructure, shipment reliability) | 1 (poor) to 5 (excellent) |
| `oecd_crc.csv` | **OECD Country Risk Classification** | Sovereign credit risk — the assessed likelihood a country cannot service its external debt | 0 (minimum risk, e.g. Germany) to 7 (maximum risk, e.g. Russia, Iran) |
| `opensanctions_by_country.csv` | **OpenSanctions** aggregate | Number of individuals, companies and vessels from each country appearing on international sanctions lists (EU, US OFAC, UK HMT) | count; plus a 0/1 `sanctioned_flag` |
| `circumvention_hubs.csv` | Compiled from trade-anomaly research (e.g. Kyiv School of Economics) | Whether a country is a documented transit hub for **sanctions evasion** toward Russia after 2022 | level 0 / 1 / 2 (none / moderate / systematic) |
| `istat_trade.csv` | **ISTAT** (Italian statistics office) | Italian import (`IV`) and export (`EV`) values by **CPA sector** and **partner country**, year 2022 | EUR |
| `db_polimi_final.csv` | Firm financials (built from **AIDA / Bureau van Dijk**) | One row per Italian firm: revenue, employees, profitability, cash flow, leverage, industry code | EUR (thousands) + counts |
| `iso3_reference.csv` | Standard ISO list | Maps country codes between ISO-2 and ISO-3 (needed because sources use different code formats) | — |

> **Not included in the public repository:** `db_polimi_final.csv`, `aida_all_batches.csv`,
> `db_polimi.xlsx` and the firm-level output `layer2_firms.csv`. They come from AIDA
> (Bureau van Dijk), a licensed database, and contain firm identifiers, so they cannot be
> redistributed. See `data/README.md`.

**Auxiliary data (*not* read as a scored input by the pipeline):**
`aida_all_batches.csv` (the broader AIDA extraction the firm sample is drawn from, not public),
`db_polimi.xlsx` (spreadsheet version of the firm data, not public), `eu_crm_list.csv` (20
**Critical Raw Materials** identified by HS6 customs code — cobalt, lithium, rare
earths, gallium, etc.), and `eurostat_trade_sitc_paese.csv`. These are kept for **possible extensions and
cross-validation** (e.g. flagging firms exposed to critical raw materials, or checking
Italian flows against EU-level Eurostat figures). *Note:* the other Eurostat file,
`eurostat_ext_tec03.csv`, **is** used by `03_Layer2.r` to recover the Sector G (trade)
score — see §4.

### A note on the firm financial variables

`db_polimi_final.csv`'s columns mean:

- `ricavi` = revenue, `dipendenti` = employees, `utile_netto` = net income, `ebit`
- `oneri_fin` = financial (interest) charges; `tax_shield` = tax saving from
  deductible interest; `max_detraibile` = the cap on deductible interest under
  Italian corporate-tax rules
- `ccn` = net working capital (*capitale circolante netto*); `fcf` = free cash flow;
  `imposte` = taxes
- `leverage` = debt ratio; `indice_allerta` = a **crisis early-warning indicator**
  derived from the Italian *Codice della Crisi d'Impresa* (the reference text is in
  `docs/crecrccurrentenglish.pdf`)
- `ateco_codice` / `ateco2` = the firm's industry classification (ATECO, the Italian
  version of NACE); `ateco2` is the 2-digit division used for the sector crosswalk.

These financial variables are the firm's **economic baseline**; Layer 2 adds the
geopolitical-compliance dimension on top of them.

---

## 3. Layer 1 — Country risk (PCA + clustering)

Scripts: `scripts/01_build_layer1.py` (assembles the matrix) → `scripts/02_layer1_analysis.r`
(the statistical analysis).

### 3.1 Building the country matrix (`01_build_layer1.py`)

The Python script merges the six country sources on the ISO-3 country code into one
table (`layer1_input.csv`), one row per country. Missing sanctions / evasion values
are filled with 0 (interpreted as "no recorded sanctions / no recorded evasion role",
which is the correct default for those binary/count variables). Missing WGI and LPI
values are **left as NA here** and handled later in R.

### 3.3 Transformations

1. **Log transform of the sanctions count** (`log1p`). The number of sanctioned
   entities is extremely right-skewed: Russia has ~21,000 while the median country has
   ~35. Without a transform, that single variable would dominate the analysis purely
   because of its scale. `log1p` (log of 1 + x) compresses the range while handling
   zeros safely.
2. **Standardisation** (`scale`, mean 0 / variance 1). The variables live on
   completely different scales (WGI ≈ −2.5…+2.5, LPI 1–5, CRC 0–7, log-counts, 0–2
   levels). PCA is variance-based, so **unstandardised variables would be ranked by
   their units, not their information.** Standardisation puts every variable on equal
   footing — the standard and necessary choice here.

**Component interpretation (from the loadings):**

- **PC1 — "Institutional quality" (~63%).** All five WGI dimensions and LPI load
  positively (≈ 0.33–0.41); CRC loads negatively (≈ −0.36). PC1 runs from
  well-governed, logistically efficient, low-credit-risk countries (Germany,
  Switzerland, Finland, Norway, Singapore) to weakly governed, high-credit-risk ones
  (Syria, North Korea, Yemen, Venezuela). This is the classic *structural* country-risk
  axis.
- **PC2 — "Sanctions & evasion exposure" (~17%).** The sanctions and circumvention
  variables dominate this axis. PC2 separates countries with heavy sanctions exposure
  or documented evasion-hub behaviour (Russia, Turkey, Kazakhstan, UAE, Armenia,
  China) from those with none. Crucially, **this is information PC1 cannot capture** —
  a country can have decent institutions *and* be a sanctions-evasion hub (the UAE is
  the textbook case). That is exactly why two components are kept.

### 3.5 Clustering: Ward on PC1–PC2, k = 3

Countries are grouped with **hierarchical clustering, Ward's method (`ward.D2`)**, on
the Euclidean distances in the PC1–PC2 plane. Ward is chosen because it produces
compact, roughly equal-variance clusters, which suits the goal of identifying
coherent country "types".

**Choice of k — an honest tradeoff.** Silhouette analysis statistically *prefers*
**k = 2** (average silhouette ≈ 0.44) over **k = 3** (≈ 0.367). We nevertheless use
**k = 3** because:

- k = 3 is still in the acceptable silhouette range (> 0.25), so it is not
  statistically unreasonable; and
- k = 3 maps directly onto the **three substantive categories** the project is built
  around, which a 2-cluster solution would hide.

This is a deliberate, **theory-driven** choice, and it is flagged as such rather than
presented as the statistically optimal one. The three clusters are:

| Cluster | Name | n | Profile | Examples |
|--------|------|---|---------|----------|
| 1 | Stable markets | 64 | Strong institutions, high LPI, near-zero credit risk, no evasion role | DEU, FIN, FRA, NOR, CHE, USA, GBR |
| 2 | High structural risk | 90 | Weak institutions, low LPI, high sovereign credit risk | SYR, PRK, IRN, YEM, AFG, VEN, BLR, RUS |
| 3 | Grey zone / evasion hubs | 61 | Mixed institutions but **high circumvention level** — the compliance-critical group | ARE, TUR, KAZ, ARM, CHN, SGP, IND, SRB |

Cluster 3 is the analytically interesting one: these countries can look "fine" on
standard governance metrics yet act as sanctions-transit hubs, which is precisely the
risk that enhanced due diligence is designed to catch.

### 3.6 Continuous risk score
Instead of assigning each country a flat value per cluster (0 / 0.5 / 1), each country
gets a **continuous score in [0, 1]** based on its geometric position relative to two
anchor centroids in PC space:

```
raw_score = distance(country, stable_centroid) − distance(country, high-risk_centroid)
risk_score = min-max normalisation of raw_score to [0, 1]
```

A country far from the "stable" centroid and close to the "high-risk" centroid scores
near 1. This is preferred over discrete scoring for three reasons: it **preserves
within-cluster heterogeneity** (Russia and Belarus are both Cluster 2 but score
differently), it **places grey-zone hubs naturally at intermediate values** instead of
forcing an arbitrary 0.5, and it is **fully data-driven**.

## 4. Layer 2 — Firm compliance risk

Script: `scripts/03_Layer2.r`. Reference year **2022** (chosen because it is the first
full year after the major Russia sanctions package, with consolidated data).

Beyond the sector exposure score described below, `03_Layer2.r` also (a) recovers the
Sector G *trade* score from Eurostat `ext_tec03`, (b) assigns theory-based proxy scores
to domestic sectors with no measurable trade flows (construction, finance, hospitality,
…), (c) computes a **financial-fragility score** for each firm via PCA on its
balance-sheet ratios, and (d) crosses the two scores into four **risk quadrants**
(CRITICAL / WATCH / VULNERABLE / LOW). The companion script `04_Layer2_analysis.r`
then validates the country clusters with a **MANOVA** (Pillai test) and runs the
**weight sensitivity analysis** of §6.3; it must be run in the *same R session*, right
after `03_Layer2.r`, because it reuses the in-memory objects. The exact run order is in
the repository `README.md`.

### 4.1 From countries to sectors
ISTAT trade is reported by **CPA section** (Classification of Products by Activity:
A = agriculture, B = mining, C = manufacturing, etc.). The script reads each trade
record, identifies the partner country and the flow direction (import / export), and
maps the partner to its Layer 1 cluster.

> *Technical aside:* the ISTAT file has an irregular CSV layout (country names contain
> commas that shift column positions). The script handles this with field-count
> offsets (`CPA_OFFSETS`) determined by inspecting the raw file. This is a parsing
> workaround, not a modelling step.

### 4.2 Three exposure components per sector

For each CPA section the script computes three shares:

1. **`import_highrisk_share`** — fraction of the sector's *imports* coming from
   high-risk countries (Clusters 2 + 3). Captures supply-chain dependence.
2. **`export_highrisk_share`** — fraction of the sector's *exports* going to high-risk
   countries. Captures market-of-destination exposure.
3. **`circumvention_share`** — fraction of the sector's *total* trade (import + export)
   with evasion-hub countries (Cluster 3). Captures the specific sanctions-evasion
   risk.

### 4.3 The compliance score and its weights

```
compliance_score = 0.40 · import_highrisk_share
                 + 0.30 · export_highrisk_share
                 + 0.30 · circumvention_share
```

The rationale for weighting imports highest (0.40) is that **supply-chain dependence is the hardest
exposure to substitute**: a firm can redirect exports more easily than it can replace
a critical input sourced from a high-risk country. Exports and circumvention are
weighted equally (0.30 each). Because these weights are a choice, a **sensitivity
analysis** (re-running the scores under alternative weightings) is the natural
robustness check and is noted in the code as a planned step.

### 4.4 Assigning scores to firms

Each firm's 2-digit ATECO code is mapped to a CPA section via the `ATECO2_TO_CPA`
crosswalk, and the firm inherits its sector's compliance score and is binned into a
**risk tier**:

| Tier | compliance_score |
|------|------------------|
| LOW | 0.00 – 0.15 |
| MEDIUM | 0.15 – 0.30 |
| HIGH | 0.30 – 0.50 |
| CRITICAL | > 0.50 |

**Declared limitation — imputation.** Some sectors (construction, finance, hospitality)
have no internationally classifiable trade flows under CPA, so firms in those sectors
have no sector score. They are assigned the **median** compliance score across firms.
This is a transparent, conservative fallback, but it means those firms' scores reflect
a default rather than measured trade exposure — a limitation to state explicitly in any
write-up.

### 4.5 What the numbers say (economic reading)

From `layer2_sector_components.csv`:

- **Manufacturing (C)** dominates the Italian trade economy (≈ €478bn imports,
  €582bn exports) with moderate exposure (import-highrisk 30%, score **0.247**).
  Because of its sheer volume, it carries most of the *absolute* compliance exposure
  even at a moderate score.
- **Mining & quarrying (B)** has the highest exposure among economically material
  sectors: **80%** of its imports come from high-risk countries (score **0.395**).
  This is the energy-and-raw-materials dependence story — the sector most reliant on
  sourcing from Cluster 2/3 jurisdictions.
- **Agriculture (A)** sits at **0.244** (import-highrisk 35%).
- **Other services (S)** shows an extreme score of **0.976**, but on **negligible
  trade volume** (≈ €6m). This is a **statistical artifact of a tiny denominator**, not
  a meaningful economic signal, and should be footnoted rather than highlighted.
- Energy/utilities (D), professional services (M), arts (R) and the others score low.

The headline economic insight: **compliance risk concentrates where Italy depends on
imported raw materials and energy (mining), while the largest *absolute* exposure lives
in manufacturing simply because of its scale.**

---

## 5. Summary of methodological choices (and why)

| Choice | What we did | Why |
|--------|-------------|-----|
| Two-layer design | Estimate country risk first, then map to firms | Keeps country risk reusable and the firm score traceable |
| Drop redundant flags | Remove `sanctioned_flag`, `circumvention_flag` | Avoid double-counting before PCA |
| Log-transform sanctions | `log1p(n_sanctioned_entities)` | Tame extreme right skew (Russia ≫ median) |
| Standardise variables | `scale()` to mean 0 / var 1 | PCA is scale-sensitive; equalise variable influence |
| PCA, keep 2 components | ~80% variance in PC1+PC2 | Reduce dimensionality, gain interpretable axes |
| Ward clustering, k = 3 | Hierarchical clustering on PC1–PC2 | Compact clusters; k = 3 chosen for interpretability over the silhouette-optimal k = 2 (stated openly) |
| Continuous risk score | Distance-to-centroid, min-max normalised | Preserve within-cluster variation; avoid arbitrary discrete values |
| Compliance weights 0.4/0.3/0.3 | Weighted average of 3 shares | Import dependence is hardest to substitute; weights are a parameter pending sensitivity analysis |
| Median imputation for firms without CPA | Fill missing sector scores with the median | Transparent fallback; flagged as a limitation |
| Reference year 2022 | Filter trade to 2022 | First consolidated year after the major Russia sanctions |

---

## 6. Next steps — the planned statistical analysis

This is where the project moves from *building a score* to *doing statistics with it*
— the part that turns the data pipeline into an Applied Statistics thesis. Five
analyses are planned, roughly in order of analytical weight.

> *Implementation status.* The financial-fragility PCA, the 2D risk quadrants, the
> cluster **MANOVA** and the **weight sensitivity analysis** (§6.3) are now implemented
> in `03_Layer2.r` / `04_Layer2_analysis.r`. The remaining items — the financial
> regression (§6.1), the rule-based RQ5 score (§6.2) and the event validation (§6.4) —
> are still planned.

### 6.1 Financial regression (the analytical core)

The central hypothesis: **does a firm's geopolitical exposure explain variation in its
financial performance?** The baseline model (OLS to start):

```
EBIT_margin ~ import_exposure + export_exposure + leverage + log(revenue) + province
```

- **Outcomes to test:** EBIT margin, leverage, and the crisis early-warning index
  (`indice_allerta`) as a financial-distress proxy.
- **Controls:** firm size (`log(revenue)`), province (geography), leverage.
- **Why it is statistically interesting:** it tests a genuine hypothesis with controls,
  diagnostics and interpretation, instead of merely reporting a score.

> *Recommended statistical care (suggestions, to confirm):* because `compliance_score`
> is assigned at the **sector** level, regression errors are likely correlated within
> sector. The clean treatments are **cluster-robust standard errors by CPA** or a
> **multilevel model** with firms nested in sectors; either avoids overstating
> significance. Worth also checking heteroskedasticity-robust SEs, multicollinearity
> among the exposure measures (VIF), and influential observations.

### 6.2 RQ5 — combined rule-based compliance score

Extend the Layer 2 sector score into a fuller firm–product–country score:

```
compliance_score = w1·cluster_risk + w2·circumvention_flag + w3·dual_use_flag + w4·CRM_flag
```

- **`CRM_flag` — available now.** `eu_crm_list.csv` already provides 20 Critical Raw
  Materials by HS6 code, so exposure to strategically critical inputs can be flagged
  immediately.
- **`dual_use_flag` — the missing input.** Requires the EU dual-use goods list
  (Regulation **2021/821**), which is not yet in the repo. This is the one piece still
  to obtain before RQ5 can be completed.
- The weights `w1…w4` are again parameters, which ties this step directly to the
  sensitivity analysis below.

### 6.3 Sensitivity / robustness analysis

Because several modelling choices are parameters rather than estimates, robustness is
what gives the framework credibility:

- **Number of components:** re-run the clustering on **3 principal components instead
  of 2** (PC1+PC2 already capture ~80% of variance) and check whether cluster
  membership is stable — e.g. with the adjusted Rand index between the two solutions.
- **Number of clusters:** report **k = 2 and k = 3 side by side** (the silhouette
  favours k = 2), so the substantive choice is transparent.
- **Compliance weights:** vary the weights over a grid and report how firms' risk-tier
  assignments shift. Stable tiers across reasonable weightings = a robust score;
  unstable tiers are themselves a finding worth reporting.

### 6.4 Validation against known geopolitical events

An event-study-style face-validity check: do firms with high **pre-2022 exposure to
Russia** show measurable financial deterioration in **2022–2023** (margin compression,
higher distress index)? Further narrative cases: Iran/China and Taiwan tensions.
Linking the score to observable outcomes is the strongest argument that it measures
something real, not just an internal construct.

### 6.5 Descriptive cluster characterisation

Before the regression, characterise each country cluster rigorously: which variables
discriminate the clusters (e.g. ANOVA or Kruskal–Wallis per variable across clusters),
how stable the clusters are (bootstrap resampling of the clustering), and how they
distribute geographically. This is the descriptive backbone that justifies treating the
clusters as statistically meaningful groups before using them downstream.

---

## 7. Known limitations

- **k = 3 is a substantive, not statistically optimal, choice** — the silhouette
  prefers k = 2 (addressed by reporting both, see §6.3).
- **The compliance weights are not estimated** but chosen (addressed by the
  sensitivity analysis, §6.3).
- **Imputed firms** (construction, finance, hospitality) carry a default median score,
  not a measured exposure.
- **The firm financial layer** (`fcf`, `tax_shield`, `indice_allerta`) is provided as
  input, not recomputed by this pipeline; the regression in §6.1 is what bridges it to
  the compliance score.

---

*This document describes the reasoning behind the code as implemented. Where it
interprets an unstated choice (e.g. the rationale for the import weight), that is
analysis, not a claim from the original authors — corrections welcome.*
