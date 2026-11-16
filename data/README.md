# Data

## Included (public sources)

`data/raw/`

| File | Source |
|------|--------|
| `wgi.csv` | World Bank, Worldwide Governance Indicators |
| `lpi.csv` | World Bank, Logistics Performance Index |
| `oecd_crc.csv` | OECD Country Risk Classification |
| `opensanctions_by_country.csv` | OpenSanctions, aggregated by country |
| `circumvention_hubs.csv` | Compiled from public trade-anomaly research |
| `istat_trade.csv` | ISTAT, Italian trade by CPA sector and partner country, 2022 |
| `eurostat_ext_tec03.csv`, `eurostat_trade_sitc_paese.csv` | Eurostat |
| `eu_crm_list.csv` | EU Critical Raw Materials list |
| `iso3_reference.csv` | ISO country codes |

`data/processed/` contains aggregated outputs at country and sector level only.

## Not included (licensed)

The firm-level files come from **AIDA (Bureau van Dijk)**, accessed under a university
licence, and contain firm names and tax identifiers, so they are not published:

- `db_polimi_final.csv`: firm financials (input to Layer 2)
- `aida_all_batches.csv`, `db_polimi.xlsx`: raw extractions
- `layer2_firms.csv`: firm-level scores (output of Layer 2)

To run Layer 2, place a file named `db_polimi_final.csv` in `data/raw/` with these columns:

```
ragione_sociale, provincia, chiusura_bilancio, ricavi, dipendenti, utile_netto, ebit,
max_detraibile, oneri_fin, tax_shield, ccn, fcf, imposte, indice_allerta, leverage,
codice_fiscale, ateco_codice, ateco_descrizione, ateco2
```

Monetary values are in thousands of EUR. Column meanings are described in
`docs/METHODOLOGY.md`.
