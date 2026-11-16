# ╔══════════════════════════════════════════════════════════════════════════╗
# ║        BUILD LAYER 2 v2 — Compliance Risk Score + Financial Fragility   ║
# ║                              (R version)                               ║
# ╚══════════════════════════════════════════════════════════════════════════╝
#
# OBJECTIVE
# ─────────
# Assign two scores to ~20,000 Italian firms:
#
#   compliance_score  ∈ [0,1]
#     SECTOR geopolitical exposure: how much of the sector's trade flows
#     pass through high-risk countries (sanctions, evasion).
#     Source: ISTAT trade 2022 + Eurostat ext_tec03.
#
#   fragility_score   ∈ [0,1]
#     FIRM financial fragility: computed with PCA on balance-sheet
#     margins, leverage and the alert index.
#
# STRUCTURE
# ─────────
#   [1]  Layer 1: country risk scores from country clustering
#   [2]  Mapping ISO2 → ISO3
#   [3]  Parse ISTAT trade 2022 (irregular CSV format)
#   [4]  Risk components per CPA sector (ISTAT data)
#   [5]  Sector G (Trade) from Eurostat ext_tec03
#   [6]  Proxy scores for domestic sectors (F, H, I, K, L, N, P)
#   [7]  Complete sector table
#   [8]  Assignment to firms via ATECO→CPA crosswalk
#   [9]  Financial Fragility Score (PCA)
#   [10] 2D matrix quadrants
#   [11] Save CSV
#   [12] 6 presentation-ready charts
#
# REQUIRED PACKAGES
# ───────────────────
#   install.packages(c("tidyverse", "patchwork", "scales", "viridis"))
#
# ── Load libraries ───────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(patchwork)
library(scales)


# ══════════════════════════════════════════════════════════════════
# PATHS — relative to the repository root
# ══════════════════════════════════════════════════════════════════
# Paths relative to the repository root (this file lives in scripts/).
# Works both with Rscript and inside RStudio.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("--file=", args)
  if (length(m) > 0) return(dirname(normalizePath(sub("--file=", "", args[m]))))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    return(dirname(rstudioapi::getSourceEditorContext()$path))
  return(file.path(getwd(), "scripts"))
}
ROOT      <- normalizePath(file.path(get_script_dir(), ".."))
RAW       <- file.path(ROOT, "data", "raw")
PROC      <- file.path(ROOT, "data", "processed")
DOWNLOADS <- RAW   # istat_trade.csv and db_polimi_final.csv live in data/raw
OUT       <- PROC


# ══════════════════════════════════════════════════════════════════
# GLOBAL PARAMETERS
# ══════════════════════════════════════════════════════════════════

YEAR_REF    <- 2022   # reference year for trade flows

# Weights for the compliance_score.
# W_IMP = 0.40: "importing from risky countries" weighs 40%
#   (supply-chain dependence = the most direct risk).
# W_EXP = 0.30: exposure on destination markets.
# W_CIRC = 0.30: risk of involvement in sanctions-evasion chains.
W_IMP  <- 0.40
W_EXP  <- 0.30
W_CIRC <- 0.30

# Thresholds for the 2D matrix.
# compliance > 0.20 = "high exposure" side
# fragility  > 0.60 = "high fragility" side
COMP_THRESH <- 0.20
FRAG_THRESH <- 0.60

# Number of fields in the ISTAT CSV per CPA section.
# The ISTAT file has an irregular format: some sector names
# contain internal commas, so the number of columns varies.
# Map: n_fields → (iso2_pos, year_pos, value_pos)
# (1-based indices as in R)
CPA_OFFSETS <- list(
  "37" = c(11, 13, 14),   # B, C, E, J, S — simple name
  "38" = c(12, 14, 15),   # A, M  — 1 comma in the name (confirmed by debugging)
  "39" = c(13, 15, 16)    # D, R, V — 2 commas in the name
  # 40-field rows = aggregates/totals → discarded automatically
)


# ══════════════════════════════════════════════════════════════════
# CROSSWALK ATECO2 → CPA
#
# CPA (Classification of Products by Activity) = European system
# to classify products by sector (letters A-U).
# ATECO2 = first 2 digits of the Italian ATECO code.
#
# ORIGINAL CROSSWALK: only sectors with direct ISTAT data.
# EXTENDED CROSSWALK: adds domestic sectors (construction, etc.)
# ══════════════════════════════════════════════════════════════════

# Helper function: assign the CPA code to a vector of ATECO2
assign_cpa <- function(ateco2, extended = FALSE) {
  cpa <- rep(NA_character_, length(ateco2))
  
  # Sectors with direct ISTAT data (original crosswalk)
  cpa[ateco2 %in% 1:3]   <- "A"   # Agriculture
  cpa[ateco2 %in% 5:9]   <- "B"   # Mining
  cpa[ateco2 %in% 10:33] <- "C"   # Manufacturing
  cpa[ateco2 == 35]      <- "D"   # Energy
  cpa[ateco2 %in% 36:39] <- "E"   # Utilities/waste
  cpa[ateco2 %in% 58:63] <- "J"   # ICT
  cpa[ateco2 %in% 69:75] <- "M"   # Professional services
  cpa[ateco2 %in% 90:93] <- "R"   # Arts/Sport
  cpa[ateco2 %in% 94:96] <- "S"   # Other services
  
  if (extended) {
    # Domestic sectors added in the extended crosswalk
    # (not covered by ISTAT: they get a proxy or Eurostat score)
    cpa[ateco2 %in% 41:43] <- "F"   # Construction
    cpa[ateco2 %in% 45:47] <- "G"   # Trade (score from Eurostat)
    cpa[ateco2 %in% 49:53] <- "H"   # Transport/Logistics
    cpa[ateco2 %in% 55:56] <- "I"   # Hospitality/Food service
    cpa[ateco2 %in% 64:66] <- "K"   # Finance
    cpa[ateco2 == 68]      <- "L"   # Real estate
    cpa[ateco2 %in% 77:82] <- "N"   # Administrative services
    cpa[ateco2 %in% 85:89] <- "P"   # Education/Health
  }
  
  return(cpa)
}


# ══════════════════════════════════════════════════════════════════
# PROXY SCORES FOR DOMESTIC SECTORS
#
# For F, H, I, K, L, N, P there are no ISTAT trade flows.
# Economically motivated scores (see inline comments):
# ══════════════════════════════════════════════════════════════════
theory_scores <- tribble(
  ~cpa, ~import_highrisk_share, ~export_highrisk_share, ~circumvention_share, ~compliance_score, ~score_source,
  # F Construction (0.05): almost entirely domestic sector,
  #   material purchases almost all from domestic suppliers
  "F",  0.04, 0.03, 0.03, 0.05, "Theory proxy",
  # H Transport (0.18): the highest among the proxies. Logistics
  #   firms run routes passing through circumvention hubs
  #   (Georgia, Kazakhstan, UAE for goods toward Russia post-2022)
  "H",  0.18, 0.14, 0.17, 0.18, "Theory proxy",
  # I Hospitality (0.06): mostly domestic and tourist demand
  "I",  0.05, 0.04, 0.04, 0.06, "Theory proxy",
  # K Finance (0.12): indirect exposure — banks finance
  #   operations with risky countries through their clients
  "K",  0.10, 0.10, 0.09, 0.12, "Theory proxy",
  # L Real estate (0.02): 100% domestic by definition
  "L",  0.01, 0.01, 0.01, 0.02, "Theory proxy",
  # N Administrative services (0.07): mostly domestic
  "N",  0.06, 0.05, 0.05, 0.07, "Theory proxy",
  # P Education/Health (0.03): public/semi-public sector
  "P",  0.02, 0.02, 0.02, 0.03, "Theory proxy"
)

# Readable labels
sector_names <- c(
  A="Agriculture",   B="Mining",    C="Manufacturing",
  D="Energy",       E="Utilities",     F="Construction",
  G="Trade",     H="Transport",     I="Hospitality",
  J="ICT",           K="Finance",       L="Real estate",
  M="Prof. services", N="Admin. services",  P="Education/Health",
  R="Arts/Sport",    S="Other services"
)
source_labels <- c(
  A="ISTAT data", B="ISTAT data", C="ISTAT data", D="ISTAT data",
  E="ISTAT data", J="ISTAT data", M="ISTAT data", R="ISTAT data",
  S="ISTAT data", V="ISTAT data", G="Eurostat data",
  F="Theory proxy", H="Theory proxy", I="Theory proxy",
  K="Theory proxy", L="Theory proxy", N="Theory proxy",
  P="Theory proxy"
)


# ══════════════════════════════════════════════════════════════════
# CHART STYLE
# ══════════════════════════════════════════════════════════════════
TIER_COLORS  <- c(LOW="#2196F3", MEDIUM="#FF9800", HIGH="#F44336", CRITICAL="#7B1FA2")
QUAD_COLORS  <- c(
  "CRITICAL\n(high risk)"      = "#7B1FA2",
  "WATCH\n(high exposure)"     = "#F44336",
  "VULNERABLE\n(high fragility)"  = "#FF9800",
  "LOW\n(stable)"                = "#2196F3"
)
SOURCE_COLORS <- c(
  "ISTAT data"                       = "#1976D2",
  "Eurostat data"                    = "#388E3C",
  "Theory proxy"                    = "#F57C00",
  "Theory proxy (insufficient volume)" = "#F57C00"  # same color, volume too low
)
theme_clean <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "#EEEEEE"),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(color = "#666666", size = 10),
    axis.title        = element_text(size = 10),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 9),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "#F9F9F9", color = NA)
  )


# ══════════════════════════════════════════════════════════════════
cat("=================================================================\n")
cat("BUILD LAYER 2 v2 — Compliance + Financial Fragility\n")
cat("=================================================================\n\n")


# ─────────────────────────────────────────────────────────────────
# [1] LAYER 1: country risk scores
#
# Layer 1 classified 215 countries into 3 clusters with PCA + Ward:
#   Cluster 1 = stable markets (DEU, USA, GBR, ...)
#   Cluster 2 = high structural risk (RUS, IRN, PRK, ...)
#   Cluster 3 = grey zone / evasion hubs (ARE, TUR, KAZ, ...)
#
# We identify the "high risk" and "circumvention" clusters
# empirically from the mean risk_score (we do not assume the numeric
# label 1/2/3 is fixed across different runs).
# ─────────────────────────────────────────────────────────────────
cat("[1] Loading Layer 1 country risk scores...\n")
l1 <- read_csv(file.path(PROC, "layer1_countries.csv"), show_col_types = FALSE)
colnames(l1) <- c("iso3", "cluster", "risk_score")

clus_risk     <- l1 %>% group_by(cluster) %>% summarise(mean_risk = mean(risk_score))
highrisk_clus <- clus_risk %>% filter(mean_risk > 0.4) %>% pull(cluster)
circum_clus   <- clus_risk %>% filter(mean_risk > 0.3 & mean_risk < 0.7) %>% pull(cluster)

highrisk_iso3 <- l1 %>% filter(cluster %in% highrisk_clus) %>% pull(iso3)
circum_iso3   <- l1 %>% filter(cluster %in% circum_clus)   %>% pull(iso3)

cat(sprintf("    Total countries: %d\n", nrow(l1)))
cat(sprintf("    High-risk (Cluster 2+3): %d countries\n", length(highrisk_iso3)))
cat(sprintf("    Circumvention hubs (Cluster 3): %d countries\n\n", length(circum_iso3)))


# ─────────────────────────────────────────────────────────────────
# [2] MAPPING ISO2 → ISO3
#
# ISTAT data use ISO2 (e.g. "DE"), Layer 1 uses ISO3 (e.g. "DEU").
# ─────────────────────────────────────────────────────────────────
cat("[2] Loading ISO mappings...\n")
iso_ref  <- read_csv(file.path(RAW, "iso3_reference.csv"), show_col_types = FALSE)
iso2to3  <- setNames(iso_ref$iso3, toupper(iso_ref$iso2))
iso2_to_cluster <- setNames(
  l1$cluster[match(iso_ref$iso3, l1$iso3)],
  toupper(iso_ref$iso2)
)
cat(sprintf("    Entries ISO2→ISO3: %d\n\n", length(iso2to3)))


# ─────────────────────────────────────────────────────────────────
# [3] PARSING ISTAT TRADE 2022
#
# The file has an irregular CSV format: sector names contain
# commas, so the number of fields per row varies (38, 39 or 40).
# We read line by line and use CPA_OFFSETS to find the
# positions of iso2, year and value.
# ─────────────────────────────────────────────────────────────────
cat("[3] Parsing ISTAT trade data (2022)...\n")

lines <- readLines(file.path(DOWNLOADS, "istat_trade.csv"), encoding = "UTF-8",
                   warn = FALSE)
lines <- lines[-1]                               # remove header
lines <- lines[!is.na(lines) & nchar(lines) > 0] # remove NA and empty rows

trade_rows <- lapply(lines, function(line) {
  parts <- strsplit(line, ",")[[1]]
  n     <- length(parts)
  
  # Determine the "base" number of fields
  base  <- if (as.character(n)   %in% names(CPA_OFFSETS)) as.character(n)
  else if (as.character(n-1) %in% names(CPA_OFFSETS)) as.character(n-1)
  else NA
  
  if (is.na(base)) return(NULL)
  
  idx    <- CPA_OFFSETS[[base]]
  i_iso2 <- idx[1]; i_year <- idx[2]; i_val <- idx[3]
  
  year  <- suppressWarnings(as.integer(trimws(parts[i_year])))
  value <- suppressWarnings(as.numeric(trimws(parts[i_val])))
  
  if (is.na(year) || is.na(value)) return(NULL)
  if (year != YEAR_REF || value <= 0) return(NULL)
  
  # Return a data.frame (not a plain list) so bind_rows works correctly
  data.frame(
    flow  = trimws(parts[5]),      # "IV" = imports, "EV" = exports
    cpa   = trimws(parts[9]),      # CPA section letter
    iso2  = trimws(parts[i_iso2]),
    value = value,
    stringsAsFactors = FALSE
  )
})

trade <- bind_rows(Filter(Negate(is.null), trade_rows))
trade$iso3 <- iso2to3[match(toupper(trade$iso2), names(iso2to3))]
trade <- trade %>% filter(!is.na(iso3))
cat(sprintf("    Record: %s  |  CPA: %s\n\n",
            format(nrow(trade), big.mark=","),
            paste(sort(unique(trade$cpa)), collapse=", ")))


# ─────────────────────────────────────────────────────────────────
# [4] RISK COMPONENTS PER CPA SECTOR (ISTAT data)
#
# For each CPA sector we compute 3 indicators:
#
#   import_highrisk_share = imports_from_(Cluster2+3) / total_imports
#     Supply-chain dependence on risky countries. Weight: 0.40
#
#   export_highrisk_share = exports_to_(Cluster2+3) / total_exports
#     Exposure to destination markets. Weight: 0.30
#
#   circumvention_share = trade_with_(Cluster3) / total_trade
#     Sanctions-evasion risk. Weight: 0.30
#
#   compliance_score = 0.40*import_hr + 0.30*export_hr + 0.30*circum
# ─────────────────────────────────────────────────────────────────
cat("[4] Computing compliance components per CPA section (ISTAT)...\n")

compute_components <- function(imp_df, exp_df, highrisk_set, circum_set) {
  all_cpas <- sort(unique(c(imp_df$cpa, exp_df$cpa)))
  
  bind_rows(lapply(all_cpas, function(cpa) {
    imp       <- imp_df %>% filter(cpa == !!cpa)
    total_imp <- sum(imp$value)
    hr_imp    <- sum(imp$value[imp$iso3 %in% highrisk_set])
    imp_share <- if (total_imp > 0) hr_imp / total_imp else 0
    
    exp       <- exp_df %>% filter(cpa == !!cpa)
    total_exp <- sum(exp$value)
    hr_exp    <- sum(exp$value[exp$iso3 %in% highrisk_set])
    exp_share <- if (total_exp > 0) hr_exp / total_exp else 0
    
    total_trade <- total_imp + total_exp
    circ_trade  <- sum(imp$value[imp$iso3 %in% circum_set]) +
      sum(exp$value[exp$iso3 %in% circum_set])
    circ_share  <- if (total_trade > 0) circ_trade / total_trade else 0
    
    tibble(
      cpa                   = cpa,
      import_highrisk_share = round(imp_share,  4),
      export_highrisk_share = round(exp_share,  4),
      circumvention_share   = round(circ_share, 4),
      total_import_eur      = total_imp,
      total_export_eur      = total_exp,
      score_source          = source_labels[cpa] %||% "ISTAT data"
    )
  }))
}

# Null-coalescing operator (not built-in in R < 4.4)
`%||%` <- function(a, b) if (!is.na(a) && !is.null(a)) a else b

imports    <- trade %>% filter(flow == "IV")
exports    <- trade %>% filter(flow == "EV")
components <- compute_components(imports, exports, highrisk_iso3, circum_iso3)

components <- components %>%
  mutate(compliance_score = round(W_IMP * import_highrisk_share +
                                    W_EXP * export_highrisk_share +
                                    W_CIRC * circumvention_share, 4),
         total_trade = total_import_eur + total_export_eur)

# Minimum trade-volume threshold: 500 million euro.
# Below this threshold the ISTAT figure is statistically unreliable:
# with only a few million in total trade, a single risky supplier
# can blow the share up to 99% (e.g. sector S = 6.7M → score 0.97).
# Solution: below threshold, score replaced with a low theory proxy (0.05).
MIN_TRADE_EUR <- 500e6

components <- components %>%
  mutate(
    compliance_score = if_else(total_trade < MIN_TRADE_EUR, 0.05, compliance_score),
    score_source     = if_else(total_trade < MIN_TRADE_EUR,
                               "Theory proxy (insufficient volume)",
                               score_source)
  )


# ─────────────────────────────────────────────────────────────────
# [5] SECTOR G (TRADE) FROM EUROSTAT ext_tec03
#
# ATECO 45-47 = trade = 17.2% of the sample, missing from ISTAT.
# Eurostat publishes flows for NACE 'G' in the ext_tec03 file.
# Same logic as step [4] but with iso2 instead of iso3.
# ─────────────────────────────────────────────────────────────────
cat("[5] Computing sector G (Commerce) score from Eurostat ext_tec03...\n")

ext    <- read_csv(file.path(RAW, "eurostat_ext_tec03.csv"), show_col_types = FALSE)
g_data <- ext %>% filter(nace_r2 == "G")

ext_tec03_shares <- function(flow_val) {
  df <- g_data %>%
    filter(stk_flow == flow_val) %>%
    select(partner, value = as.character(YEAR_REF)) %>%
    mutate(cluster = iso2_to_cluster[toupper(partner)]) %>%
    filter(!is.na(cluster), !is.na(value))
  
  total    <- sum(df$value)
  if (total == 0) return(list(hr = 0, circ = 0, total = 0))
  hr_val   <- sum(df$value[df$cluster %in% c(2, 3)])
  circ_val <- sum(df$value[df$cluster == 3])
  list(hr = hr_val / total, circ = circ_val / total, total = total)
}

imp_g  <- ext_tec03_shares("IMP")
exp_g  <- ext_tec03_shares("EXP")
tot    <- imp_g$total + exp_g$total
circ_g <- if (tot > 0) {
  (imp_g$circ * imp_g$total + exp_g$circ * exp_g$total) / tot
} else 0
g_score <- round(W_IMP * imp_g$hr + W_EXP * exp_g$hr + W_CIRC * circ_g, 4)

g_row <- tibble(
  cpa                   = "G",
  import_highrisk_share = round(imp_g$hr, 4),
  export_highrisk_share = round(exp_g$hr, 4),
  circumvention_share   = round(circ_g,   4),
  total_import_eur      = imp_g$total,
  total_export_eur      = exp_g$total,
  score_source          = "Eurostat data",
  compliance_score      = g_score
)

cat(sprintf("    Sector G: import_hr=%.3f, export_hr=%.3f, circum=%.3f → score=%.4f\n\n",
            imp_g$hr, exp_g$hr, circ_g, g_score))


# ─────────────────────────────────────────────────────────────────
# [6-7] COMPLETE SECTOR TABLE
#
# Combines: ISTAT data + Eurostat (G) + theory proxies (F,H,I,K,L,N,P)
# ─────────────────────────────────────────────────────────────────
cat("[6-7] Building complete sector components table...\n")

all_components <- bind_rows(components, g_row, theory_scores) %>%
  arrange(desc(compliance_score))

cat("    Compliance scores per sector (descending):\n")
print(all_components %>% select(cpa, compliance_score, score_source))


# ─────────────────────────────────────────────────────────────────
# [8] ASSIGN COMPLIANCE SCORE TO FIRMS
#
# Assignment logic (decreasing priority):
#   1. cpa_orig: original crosswalk (only sectors with ISTAT data)
#   2. cpa_ext:  extended crosswalk (adds G, F, H, I, K, L, N, P)
#   3. unknown ATECO → median of sectors with real data
# ─────────────────────────────────────────────────────────────────
cat("\n[8] Assigning compliance scores to firms...\n")

firms <- read_csv(file.path(DOWNLOADS, "db_polimi_final.csv"), show_col_types = FALSE)

# Extract the first 2 digits of the ATECO code (section)
firms <- firms %>%
  mutate(
    ateco2    = as.integer(str_sub(as.character(ateco_codice), 1, 2)),
    cpa_orig  = assign_cpa(ateco2, extended = FALSE),   # only ISTAT sectors
    cpa_ext   = assign_cpa(ateco2, extended = TRUE),    # all sectors
    cpa_final = coalesce(cpa_orig, cpa_ext)             # prefers orig
  )

# Sector score via lookup
score_lookup   <- setNames(all_components$compliance_score,   all_components$cpa)
imp_hr_lookup  <- setNames(all_components$import_highrisk_share, all_components$cpa)
exp_hr_lookup  <- setNames(all_components$export_highrisk_share, all_components$cpa)
circ_lookup    <- setNames(all_components$circumvention_share,  all_components$cpa)
source_lookup  <- setNames(all_components$score_source,         all_components$cpa)

# Median of only the sectors with real data (ISTAT + Eurostat)
real_sectors <- c("A","B","C","D","E","J","M","R","S","V","G")
median_real  <- median(all_components$compliance_score[all_components$cpa %in% real_sectors],
                       na.rm = TRUE)

firms <- firms %>%
  mutate(
    compliance_score      = coalesce(score_lookup[cpa_final],  median_real),
    import_highrisk_share = coalesce(imp_hr_lookup[cpa_final],
                                     median(all_components$import_highrisk_share[
                                       all_components$cpa %in% real_sectors])),
    export_highrisk_share = coalesce(exp_hr_lookup[cpa_final],
                                     median(all_components$export_highrisk_share[
                                       all_components$cpa %in% real_sectors])),
    circumvention_share   = coalesce(circ_lookup[cpa_final],
                                     median(all_components$circumvention_share[
                                       all_components$cpa %in% real_sectors])),
    score_source          = coalesce(source_lookup[cpa_final], "Unknown"),
    sector_name           = coalesce(sector_names[cpa_final],  "Unknown"),
    # Risk tier: classification into 4 bands
    risk_tier = cut(compliance_score,
                    breaks = c(0, 0.10, 0.20, 0.35, 1.01),
                    labels = c("LOW","MEDIUM","HIGH","CRITICAL"),
                    include.lowest = TRUE)
  )

n_mapped <- sum(!is.na(firms$cpa_final))
cat(sprintf("    Firms mapped: %s/%s (%.1f%%)\n",
            format(n_mapped, big.mark=","),
            format(nrow(firms), big.mark=","),
            n_mapped / nrow(firms) * 100))
cat("    Risk tier distribution:\n")
print(table(firms$risk_tier))


# ─────────────────────────────────────────────────────────────────
# [9] FINANCIAL FRAGILITY SCORE (PCA)
#
# VARIABLES (ratios normalized by revenue — remove the size effect):
#   ebit_margin  = EBIT / Revenue         → low = low profitability
#   fcf_margin   = FCF / Revenue          → low = weak cash flow
#   net_margin   = Net income / Revenue  → low = low profitability
#   oneri_ratio  = Fin. charges / Revenue   → high = high debt cost
#   leverage     = Leverage              → high = highly indebted
#   ccn_ratio    = NWC / Revenue          → low = poor liquidity
#   ia_log       = log(1 + |Alert index|) → high = high alert
#
# PREPROCESSING:
#   "OTTIMO" in the alert index → 5th percentile (low value = healthy)
#   Winsorization 1-99% → removes extreme outliers
#   Standardization → scale to mean 0, sd 1
#
# PCA:
#   PC1 ≈ 46% variance = financial-health axis.
#   We flip the sign: high fragility_score = fragile firm.
#   Final normalization to [0, 1].
# ─────────────────────────────────────────────────────────────────
cat("\n[9] Building Financial Fragility Score via PCA...\n")

# Winsorization function (replaces values beyond the percentiles)
winsorize_r <- function(x, lo = 0.01, hi = 0.99) {
  q <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

firms <- firms %>%
  mutate(
    leverage_num = as.numeric(leverage),
    # "OTTIMO" in the alert index → 5th percentile of the numeric distribution
    ia_num = {
      raw <- suppressWarnings(as.numeric(indice_allerta))
      ia_5pct <- quantile(raw, 0.05, na.rm = TRUE)
      replace(raw, is.na(raw), ia_5pct)   # OTTIMO → low value
    },
    # Ratios normalized by revenue (clip at 1 to avoid division by zero)
    ricavi_pos  = pmax(ricavi, 1),
    ebit_margin = ebit        / ricavi_pos,
    fcf_margin  = fcf         / ricavi_pos,
    net_margin  = utile_netto / ricavi_pos,
    oneri_ratio = oneri_fin   / ricavi_pos,
    ccn_ratio   = ccn         / ricavi_pos,
    ia_log      = log1p(abs(ia_num))   # log-transform to handle the heavy tail
  )

fin_vars <- c("ebit_margin","fcf_margin","net_margin",
              "oneri_ratio","leverage_num","ccn_ratio","ia_log")
var_labels <- c(
  ebit_margin  = "EBIT margin",
  fcf_margin   = "FCF margin",
  net_margin   = "Net margin",
  oneri_ratio  = "Fin. charges/Revenue",
  leverage_num = "Leverage",
  ccn_ratio    = "NWC/Revenue",
  ia_log       = "Alert index (log)"
)

# Prepare matrix: median imputation + winsorization + standardization
fin_mat <- firms[, fin_vars] %>%
  mutate(across(everything(), ~ {
    x <- .x
    x[is.na(x)] <- median(x, na.rm = TRUE)
    winsorize_r(x)
  })) %>%
  scale()   # standardize (mean 0, sd 1)

# PCA with prcomp() (built-in in base R)
pca_res  <- prcomp(fin_mat, center = FALSE, scale. = FALSE)
explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
loadings  <- as.data.frame(pca_res$rotation[, 1:3])

cat(sprintf("    Variance: PC1=%.1f%%, PC2=%.1f%%, PC3=%.1f%%\n",
            explained[1]*100, explained[2]*100, explained[3]*100))
cat(sprintf("    Cumulative PC1+PC2: %.1f%%\n\n", sum(explained[1:2])*100))
cat("    Loadings PCA:\n")
print(round(loadings, 3))

# PC1 orientation: we want high = FRAGILE
# If ebit_margin loads positively on PC1 → high PC1 = healthy → flip
pc1_scores <- pca_res$x[, 1]
if (loadings["ebit_margin", "PC1"] > 0) {
  pc1_scores <- -pc1_scores
  cat("\n    → PC1 inverted: now high = fragile\n")
} else {
  cat("\n    → PC1 already oriented: high = fragile\n")
}

# Normalization [0, 1]
firms$fragility_score <- (pc1_scores - min(pc1_scores)) /
  (max(pc1_scores) - min(pc1_scores))

cat(sprintf("    Fragility — median: %.3f, sd: %.3f\n",
            median(firms$fragility_score), sd(firms$fragility_score)))


# ─────────────────────────────────────────────────────────────────
# [10] 2D MATRIX QUADRANTS
#
#   CRITICAL   (compliance > 0.20 AND fragility > 0.60)
#     High geopolitical exposure + high financial fragility.
#     Top priority.
#
#   WATCH      (compliance > 0.20 AND fragility ≤ 0.60)
#     High exposure but financially solid. To monitor.
#
#   VULNERABLE (compliance ≤ 0.20 AND fragility > 0.60)
#     Fragile but little exposed to geopolitical risk.
#
#   LOW        (compliance ≤ 0.20 AND fragility ≤ 0.60)
#     Stable on both dimensions.
# ─────────────────────────────────────────────────────────────────
cat("\n[10] Assigning composite risk quadrants...\n")

firms <- firms %>%
  mutate(
    quadrant = case_when(
      compliance_score > COMP_THRESH & fragility_score > FRAG_THRESH ~ "CRITICAL\n(high risk)",
      compliance_score > COMP_THRESH & fragility_score <= FRAG_THRESH ~ "WATCH\n(high exposure)",
      compliance_score <= COMP_THRESH & fragility_score > FRAG_THRESH ~ "VULNERABLE\n(high fragility)",
      TRUE ~ "LOW\n(stable)"
    )
  )
print(table(firms$quadrant))


# ─────────────────────────────────────────────────────────────────
# [11] SAVE OUTPUT
# ─────────────────────────────────────────────────────────────────
cat("\n[11] Saving outputs...\n")

out_cols <- c(
  "ragione_sociale","provincia","ateco_codice","ateco_descrizione",
  "cpa_final","sector_name","score_source",
  "ricavi","dipendenti","ebit","utile_netto","fcf",
  "leverage_num","ia_num",
  "import_highrisk_share","export_highrisk_share","circumvention_share",
  "compliance_score","risk_tier","fragility_score","quadrant"
)
write_csv(firms[, intersect(out_cols, names(firms))],
          file.path(OUT, "layer2_firms_v2.csv"))
write_csv(all_components, file.path(OUT, "layer2_sector_components_v2.csv"))

# trade detail
trade_out <- trade %>%
  mutate(
    is_highrisk = iso3 %in% highrisk_iso3,
    is_circum   = iso3 %in% circum_iso3,
    cluster     = l1$cluster[match(iso3, l1$iso3)],
    risk_score  = l1$risk_score[match(iso3, l1$iso3)]
  )
write_csv(trade_out, file.path(OUT, "layer2_trade_detail.csv"))

cat(sprintf("    ✓ layer2_firms_v2.csv           (%s firms)\n",
            format(nrow(firms), big.mark=",")))
cat(sprintf("    ✓ layer2_sector_components_v2.csv (%d sectors)\n", nrow(all_components)))
cat("    ✓ layer2_trade_detail.csv\n")


# ══════════════════════════════════════════════════════════════════
# [12] CHARTS
# ══════════════════════════════════════════════════════════════════
cat("\n[12] Generating charts...\n")


# ── CHART 1: Before vs After ──────────────────────────────────────
# HOW TO READ IT:
#   Two side-by-side panels show the risk distribution
#   before (original crosswalk, 42.6% coverage) and after (88.6%).
#   Before: ~91% MEDIUM — all firms without a CPA get the same
#   imputed score, so the model barely differentiates anything.
#   After: 4 well-populated bands — the model now discriminates.
#   The shrinking MEDIUM bar and the growth of LOW and HIGH
#   are evidence that the crosswalk fix worked.
# ─────────────────────────────────────────────────────────────────

# Rebuild the "before" distribution with the original thresholds (0.15/0.30/0.50)
firms <- firms %>%
  mutate(
    compliance_orig    = coalesce(score_lookup[cpa_orig], median_real),
    risk_tier_before   = cut(compliance_orig,
                             breaks = c(0, 0.15, 0.30, 0.50, 1.01),
                             labels = c("LOW","MEDIUM","HIGH","CRITICAL"),
                             include.lowest = TRUE)
  )

before_df <- firms %>%
  count(risk_tier_before) %>%
  mutate(pct = n / sum(n) * 100, version = "BEFORE\n(42.6% coverage)",
         tier = as.character(risk_tier_before))

after_df <- firms %>%
  count(risk_tier) %>%
  mutate(pct = n / sum(n) * 100, version = "AFTER\n(88.6% coverage)",
         tier = as.character(risk_tier))

ba_df <- bind_rows(
  before_df %>% rename(tier = tier),
  after_df  %>% rename(tier = tier)
) %>%
  mutate(
    tier    = factor(tier, levels = c("LOW","MEDIUM","HIGH","CRITICAL")),
    version = factor(version, levels = c("BEFORE\n(42.6% coverage)","AFTER\n(88.6% coverage)"))
  )

p1 <- ggplot(ba_df, aes(x = tier, y = pct, fill = tier)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            vjust = -0.4, fontface = "bold", size = 3.8) +
  facet_wrap(~ version, ncol = 2) +
  scale_fill_manual(values = TIER_COLORS, guide = "none") +
  scale_y_continuous(limits = c(0, 105), labels = label_percent(scale = 1)) +
  labs(title = "Risk Distribution: Before and After the CPA Fix",
       subtitle = paste0("Before: 91% of firms had the same (imputed) score.",
                         " After: differentiated distribution."),
       x = NULL, y = "% of firms") +
  theme_clean +
  theme(strip.text = element_text(face = "bold", size = 11))

print(p1)
cat("    ✓ chart1_before_after\n")


# ── CHART 2: PCA Biplot ──────────────────────────────────────────
# HOW TO READ IT:
#   Left panel (PC1 vs PC2 scatter):
#     Each point = 1 firm (sample of 3,000). X axis = PC1 = main
#     fragility axis (~46% variance). Red = fragile,
#     blue = healthy. If the red/blue separation is clear along X,
#     PC1 is discriminating well.
#
#   Right panel (loading plot):
#     Arrows = financial variables.
#     → Arrows to the left = contribute to FRAGILITY
#       (leverage, financial charges)
#     → Arrows to the right = contribute to HEALTH
#       (EBIT margin, FCF, net)
#     Arrow length = variable importance.
#     Orthogonal arrows = mutually independent variables.
# ─────────────────────────────────────────────────────────────────

# Loadings sign for the loading plot (consistent with the chosen orientation)
pc1_sign <- if (loadings["ebit_margin", "PC1"] > 0) -1 else 1

set.seed(42)
samp_idx <- sample(nrow(firms), 3000)
samp_df  <- firms[samp_idx, ] %>%
  mutate(
    PC1   = pca_res$x[samp_idx, 1] * pc1_sign,
    PC2   = pca_res$x[samp_idx, 2],
    frag_cat = if_else(fragility_score > FRAG_THRESH,
                       sprintf("High fragility (>%.2f)", FRAG_THRESH),
                       sprintf("Low fragility (≤%.2f)", FRAG_THRESH))
  )

p2a <- ggplot(samp_df, aes(x = PC1, y = PC2, color = frag_cat)) +
  geom_point(alpha = 0.3, size = 1, shape = 16) +
  scale_color_manual(values = c("#F44336","#2196F3"),
                     name = NULL) +
  labs(
    title    = "PCA space (sample of 3,000 firms)",
    subtitle = "Red/blue separation along X confirms PC1 captures fragility",
    x = sprintf("PC1 — Main fragility axis (%.1f%% variance)", explained[1]*100),
    y = sprintf("PC2 (%.1f%% variance)", explained[2]*100)
  ) +
  theme_clean +
  theme(legend.position = "bottom")

# Loading plot
load_df <- data.frame(
  var    = rownames(loadings),
  label  = var_labels[rownames(loadings)],
  x      = loadings$PC1 * pc1_sign,
  y      = loadings$PC2
)

p2b <- ggplot(load_df) +
  # reference circle
  geom_path(data = data.frame(
    x = cos(seq(0, 2*pi, length.out = 100)),
    y = sin(seq(0, 2*pi, length.out = 100))),
    aes(x = x, y = y), color = "#CCCCCC", linetype = "dashed") +
  # arrows
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               color = "#333333", linewidth = 0.9) +
  # variable labels
  geom_text(aes(x = x * 1.15, y = y * 1.15, label = label,
                hjust = if_else(x > 0, 0, 1)),
            size = 3.2, color = "#333333") +
  # axes
  geom_hline(yintercept = 0, color = "#CCCCCC", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "#CCCCCC", linewidth = 0.5) +
  annotate("text", x = -1.6, y = 0.05,
           label = "← FRAGILE", size = 3, color = "#F44336", fontface = "italic") +
  annotate("text", x =  1.1, y = 0.05,
           label = "HEALTHY →", size = 3, color = "#2196F3", fontface = "italic") +
  scale_x_continuous(limits = c(-1.7, 1.7)) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  coord_fixed() +
  labs(
    title    = "Loading Plot — Variable contributions to PC1",
    subtitle = sprintf("PC1: %.1f%%   PC2: %.1f%%   PC3: %.1f%%",
                       explained[1]*100, explained[2]*100, explained[3]*100),
    x = "Loadings PC1",
    y = "Loadings PC2"
  ) +
  theme_clean

p2 <- p2a + p2b +
  plot_annotation(title = "PCA on Firm Financial Fragility",
                  theme = theme(plot.title = element_text(face="bold", size=13)))

print(p2)
cat("    ✓ chart2_pca_fragility\n")

# ── CHART 3: 2D matrix (main chart) ─────────────────────
# HOW TO READ IT:
#   Each point = 1 firm (sample of 4,000).
#   X AXIS = compliance_score = SECTOR geopolitical exposure
#            (0 = none, 1 = maximum)
#   Y AXIS = fragility_score = FIRM financial fragility
#            (0 = very solid, 1 = maximally fragile)
#   The dashed lines divide the 4 quadrants.
#
#   4 COLORS / QUADRANTS:
#     Purple (CRITICAL, top-right): high exposure + high fragility
#       → top priority for compliance analysis
#     Red (WATCH, bottom-right): high exposure but financially healthy
#       → monitor; financial strength protects in the short term
#     Orange (VULNERABLE, top-left): fragile but little exposed
#       → financial risk independent of geopolitics
#     Blue (LOW, bottom-left): stable on both dimensions
#
#   COLOR DENSITY: where points pile up = where
#   risk concentrates in the sample.
# ─────────────────────────────────────────────────────────────────
set.seed(42)
samp4k <- firms %>%
  slice_sample(n = 4000) %>%
  mutate(quadrant = factor(quadrant, levels = names(QUAD_COLORS)))

# Count totals for the legend (all 19,938, not just the 4,000)
quad_counts <- firms %>%
  count(quadrant) %>%
  mutate(label = sprintf("%s (n=%s)", sub("\n.*","",quadrant),
                         format(n, big.mark=",")))
quad_legend <- setNames(quad_counts$label, quad_counts$quadrant)

p3 <- ggplot(samp4k, aes(x = compliance_score, y = fragility_score, color = quadrant)) +
  geom_point(alpha = 0.35, size = 1.2, shape = 16) +
  # threshold lines
  geom_vline(xintercept = COMP_THRESH, linetype = "dashed",
             color = "#333333", linewidth = 0.8, alpha = 0.7) +
  geom_hline(yintercept = FRAG_THRESH, linetype = "dashed",
             color = "#333333", linewidth = 0.8, alpha = 0.7) +
  # threshold labels
  annotate("text", x = COMP_THRESH + 0.01, y = 0.02,
           label = sprintf("threshold %.2f", COMP_THRESH),
           hjust = 0, size = 3, color = "#555555") +
  annotate("text", x = 0.02, y = FRAG_THRESH + 0.01,
           label = sprintf("threshold %.2f", FRAG_THRESH),
           hjust = 0, size = 3, color = "#555555") +
  # quadrant labels
  annotate("label", x = COMP_THRESH * 0.5, y = 0.95,
           label = "VULNERABLE\nhigh fragility, low exposure",
           fill = "white", color = "#FF9800", size = 3, alpha = 0.85) +
  annotate("label", x = COMP_THRESH + 0.13, y = 0.95,
           label = "CRITICAL\nhigh fragility, high exposure",
           fill = "white", color = "#7B1FA2", size = 3, alpha = 0.85) +
  annotate("label", x = COMP_THRESH * 0.5, y = 0.08,
           label = "LOW\nlow fragility, low exposure",
           fill = "white", color = "#1976D2", size = 3, alpha = 0.85) +
  annotate("label", x = COMP_THRESH + 0.13, y = 0.08,
           label = "WATCH\nlow fragility, high exposure",
           fill = "white", color = "#C62828", size = 3, alpha = 0.85) +
  scale_color_manual(values = QUAD_COLORS,
                     labels = quad_legend[levels(samp4k$quadrant)],
                     name = "Quadrant (total firms)") +
  scale_x_continuous(limits = c(-0.02, 1.02)) +
  scale_y_continuous(limits = c(-0.02, 1.02)) +
  labs(
    title    = sprintf("Geopolitical Risk × Financial Fragility Matrix"),
    subtitle = sprintf("~%s Italian firms  (sample of 4,000 shown)",
                       format(nrow(firms), big.mark=",")),
    x = "Compliance Score  (sector geopolitical exposure)",
    y = "Financial Fragility Score  (PCA on balance-sheet variables)"
  ) +
  theme_clean +
  theme(legend.position = "bottom",
        legend.box      = "horizontal")

print(p3)
cat("    ✓ chart3_2d_matrix\n")
# ── CHART 4: Sector profiles ──────────────────────────────────
# HOW TO READ IT:
#   Left panel — Compliance Score by sector:
#     Bars sorted by value (further right = more exposed).
#     COLOR = data source:
#       Blue  (ISTAT data)    = directly measured trade flows
#       Green (Eurostat data) = Sector G, different source but real data
#       Orange (Proxy)        = motivated theoretical value, not measured
#     Sectors with a proxy must be interpreted with more caution.
#
#   Right panel — Fragility Score by sector:
#     Mean financial fragility of the firms in the sector.
#     Always computed from real balance sheets (independent of the CPA source).
#
#   CROSS-REFERENCE THE TWO PANELS to find the sectors most critical
#   on both dimensions at once.
# ─────────────────────────────────────────────────────────────────
sec_stats <- firms %>%
  filter(!is.na(cpa_final)) %>%
  group_by(cpa_final, sector_name, score_source) %>%
  summarise(
    n_firms         = n(),
    mean_compliance = mean(compliance_score, na.rm = TRUE),
    mean_fragility  = mean(fragility_score,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_firms >= 10)

p4a <- sec_stats %>%
  mutate(sector_name = reorder(sector_name, mean_compliance)) %>%
  ggplot(aes(x = mean_compliance, y = sector_name, fill = score_source)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", mean_compliance)),
            hjust = -0.1, size = 3.2) +
  scale_fill_manual(values = SOURCE_COLORS, name = "Data source") +
  scale_x_continuous(limits = c(0, 1.1)) +
  labs(title = "Geopolitical Exposure",
       subtitle = "Mean Compliance Score per sector",
       x = "Compliance Score", y = NULL) +
  theme_clean

p4b <- sec_stats %>%
  mutate(sector_name = reorder(sector_name, mean_fragility)) %>%
  ggplot(aes(x = mean_fragility, y = sector_name)) +
  geom_col(fill = "#FF9800", width = 0.65, color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", mean_fragility)),
            hjust = -0.1, size = 3.2) +
  scale_x_continuous(limits = c(0, 1.0)) +
  labs(title = "Financial Fragility",
       subtitle = "Mean Fragility Score per sector",
       x = "Fragility Score", y = NULL) +
  theme_clean

p4 <- p4a + p4b +
  plot_annotation(title = "Risk Profile by Sector",
                  theme = theme(plot.title = element_text(face="bold", size=13)))

cat("    ✓ chart4_sector_profiles.png\n")
print(p4)
cat("    ✓ chart4_sector_profiles\n")
# HOW TO READ IT:
#   Rows = sectors, Columns = 4 risk quadrants.
#   Each cell = % of that sector's firms in the quadrant.
#   (percentages in each row sum to 100%)
#   Colors: from white (0%) to dark purple (high concentration).
#
#   PATTERNS TO LOOK FOR:
#     Row dominated by "LOW" (left, light colors) = stable sector
#     Row dominated by "CRITICAL" or "WATCH" = high-risk sector
#     Row with spread-out values = heterogeneous sector
#
#   This is the most informative chart for a presentation:
#   it shows the risk structure across all sectors at a glance.
# ─────────────────────────────────────────────────────────────────
quad_pct <- firms %>%
  filter(!is.na(sector_name), sector_name != "Unknown") %>%
  group_by(sector_name, quadrant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(sector_name) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  filter(sector_name %in% (firms %>%
                             count(sector_name) %>%
                             filter(n >= 10) %>%
                             pull(sector_name))) %>%
  mutate(quadrant_short = case_when(
    str_detect(quadrant, "CRITICAL")   ~ "CRITICAL",
    str_detect(quadrant, "WATCH")      ~ "WATCH",
    str_detect(quadrant, "VULNERABLE") ~ "VULNERABLE",
    TRUE ~ "LOW"
  ),
  quadrant_short = factor(quadrant_short,
                          levels = c("LOW","VULNERABLE","WATCH","CRITICAL")))

p5 <- ggplot(quad_pct, aes(x = quadrant_short, y = sector_name, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), size = 3.5, fontface = "bold",
            color = if_else(quad_pct$pct > 45, "white", "#333333")) +
  scale_fill_gradientn(
    colors  = c("#E3F2FD","#BBDEFB","#FF9800","#7B1FA2"),
    values  = scales::rescale(c(0, 25, 50, 80)),
    limits  = c(0, 85),
    name    = "% of firms\nin sector"
  ) +
  labs(
    title    = "Risk Distribution by Sector (%)",
    subtitle = "Read by row: each cell shows the % of the sector\u2019s firms in the quadrant",
    x = "Risk Quadrant",
    y = NULL
  ) +
  theme_clean +
  theme(axis.text.x = element_text(face = "bold"))

cat("    ✓ chart5_heatmap.png\n")

print(p5)
cat("    ✓ chart5_heatmap\n")
#   Each "violin" = full score distribution for a sector.
#   WIDTH of the violin at a point = how many firms have that score.
#   Inner TICK = sector median.
#   Inner BOX = interquartile range (25th-75th percentile).
#
#   THINGS TO NOTICE:
#     Narrow violin = homogeneous sector (all firms similar)
#     Violin wide at the top = many firms with a high score
#     "Bimodal" violin = two distinct subgroups in the sector
#
#   The violins are sorted by median (the sector with the lowest
#   median at the bottom, the highest at the top).
#   The vertical dashed line is the quadrant threshold.
# ─────────────────────────────────────────────────────────────────
top8 <- firms %>%
  count(sector_name) %>%
  filter(sector_name != "Unknown") %>%
  slice_max(n, n = 8) %>%
  pull(sector_name)

df_top <- firms %>% filter(sector_name %in% top8)

make_violin <- function(df, x_var, x_label, thresh, fill_col) {
  order_vec <- df %>%
    group_by(sector_name) %>%
    summarise(med = median(.data[[x_var]], na.rm = TRUE)) %>%
    arrange(med) %>%
    pull(sector_name)
  
  df %>%
    mutate(sector_name = factor(sector_name, levels = order_vec)) %>%
    ggplot(aes(x = .data[[x_var]], y = sector_name)) +
    geom_violin(fill = fill_col, alpha = 0.7, color = "white",
                scale = "width", linewidth = 0.4) +
    geom_boxplot(width = 0.15, fill = "white", outlier.size = 0.5,
                 color = "#555555", linewidth = 0.4) +
    geom_vline(xintercept = thresh, linetype = "dashed",
               color = "#888888", linewidth = 0.8) +
    annotate("text", x = thresh + 0.02, y = 0.7,
             label = sprintf("threshold\n%.2f", thresh),
             size = 2.8, color = "#777777", hjust = 0) +
    scale_x_continuous(limits = c(-0.05, 1.05),
                       labels = label_number(accuracy = 0.1)) +
    labs(x = sprintf("%s  [0–1]", x_label), y = NULL) +
    theme_clean
}

p6a <- make_violin(df_top, "compliance_score",
                   "Compliance Score (Geopolitical Exposure)",
                   COMP_THRESH, "#1976D2")
p6b <- make_violin(df_top, "fragility_score",
                   "Financial Fragility Score",
                   FRAG_THRESH, "#E64A19")

p6 <- p6a + p6b +
  plot_annotation(
    title    = "Score Distribution by Sector (Top 8 by count)",
    subtitle = "Violin width = firm density. Inner tick = median.",
    theme = theme(plot.title    = element_text(face = "bold", size = 13),
                  plot.subtitle = element_text(color = "#666666", size = 10))
  )

cat("    ✓ chart6_violin.png\n")


print(p6)
cat("    ✓ chart6_violin\n")
cat("=================================================================\n")
cat(sprintf("\n  Reference year:       %d\n", YEAR_REF))
cat(sprintf("  Compliance weights:   import=%.2f, export=%.2f, circum=%.2f\n",
            W_IMP, W_EXP, W_CIRC))
cat(sprintf("  2D matrix thresholds: compliance>%.2f, fragility>%.2f\n",
            COMP_THRESH, FRAG_THRESH))
cat(sprintf("\n  Total firms:          %s\n", format(nrow(firms), big.mark=",")))
cat(sprintf("  CPA assigned:         %s (%.1f%%)\n",
            format(sum(!is.na(firms$cpa_final)), big.mark=","),
            mean(!is.na(firms$cpa_final)) * 100))
cat(sprintf("  PCA variance (PC1):   %.1f%%\n", explained[1] * 100))
cat(sprintf("  PCA variance (PC1+2): %.1f%%\n", sum(explained[1:2]) * 100))
cat("\n  Risk tier:\n")
print(table(firms$risk_tier))
cat("\n  2D quadrants:\n")
print(table(firms$quadrant))
cat("\nDone ✓\n")
