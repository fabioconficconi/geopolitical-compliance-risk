import pandas as pd
import numpy as np
from pathlib import Path

# Paths relative to the repository root (this file lives in scripts/)
ROOT = Path(__file__).resolve().parent.parent
RAW  = ROOT / 'data' / 'raw'
PROC = ROOT / 'data' / 'processed'
PROC.mkdir(parents=True, exist_ok=True)

print("=" * 50)
print("BUILD LAYER 1 - Country Risk Matrix")
print("=" * 50)

# Load ISO reference for iso2 -> iso3 conversion
iso_ref = pd.read_csv(RAW / 'iso3_reference.csv')
iso_ref.columns = [c.strip().lower() for c in iso_ref.columns]
print("ISO ref columns:", list(iso_ref.columns))

# Find iso2 and iso3 columns
iso2_col = [c for c in iso_ref.columns if '2' in c and ('iso' in c or 'alpha' in c)][0]
iso3_col = [c for c in iso_ref.columns if '3' in c and ('iso' in c or 'alpha' in c)][0]
iso2to3 = dict(zip(iso_ref[iso2_col].str.strip().str.upper(),
                   iso_ref[iso3_col].str.strip().str.upper()))
print(f"ISO2→ISO3 mapping: {len(iso2to3)} entries")

# Load all sources
wgi       = pd.read_csv(RAW / 'wgi.csv')
lpi       = pd.read_csv(RAW / 'lpi.csv')
crc       = pd.read_csv(RAW / 'oecd_crc.csv')
sanctions = pd.read_csv(RAW / 'opensanctions_by_country.csv')
hubs      = pd.read_csv(RAW / 'circumvention_hubs.csv')

for df in [wgi, lpi, crc, sanctions, hubs]:
    df.columns = [c.strip().lower() for c in df.columns]

# Convert sanctions iso2 -> iso3
sanctions['iso3'] = sanctions['iso2'].str.strip().str.upper().map(iso2to3)
sanctions = sanctions.dropna(subset=['iso3'])

# Keep only needed columns
wgi       = wgi[['iso3', 'wgi_voice_accountability', 'wgi_gov_effectiveness',
                  'wgi_regulatory_quality', 'wgi_rule_of_law', 'wgi_control_corruption']]
lpi       = lpi[['iso3', 'lpi_score']]
crc       = crc[['iso3', 'crc_rating']]
sanctions = sanctions[['iso3', 'n_sanctioned_entities', 'sanctioned_flag']]
hubs      = hubs[['iso3', 'circumvention_flag', 'circumvention_level']]

# Build matrix
print("\nBuilding country matrix...")
layer1 = wgi.copy()
layer1 = layer1.merge(lpi,       on='iso3', how='left')
layer1 = layer1.merge(crc,       on='iso3', how='left')
layer1 = layer1.merge(sanctions, on='iso3', how='left')
layer1 = layer1.merge(hubs,      on='iso3', how='left')

# Fill missing values
layer1['crc_rating']            = layer1['crc_rating'].fillna(0)
layer1['n_sanctioned_entities'] = layer1['n_sanctioned_entities'].fillna(0)
layer1['sanctioned_flag']       = layer1['sanctioned_flag'].fillna(0)
layer1['circumvention_flag']    = layer1['circumvention_flag'].fillna(0)
layer1['circumvention_level']   = layer1['circumvention_level'].fillna(0)

print(f"Countries: {len(layer1)}")
print(f"Missing values:\n{layer1.isnull().sum()}")

# Save
out_path = PROC / 'layer1_input.csv'
layer1.to_csv(out_path, index=False)
print(f"\n✓ Saved: {out_path}")
print(f"  Shape: {layer1.shape}")
print(f"\nSample (Russia, Germany, UAE):")
print(layer1[layer1['iso3'].isin(['RUS','DEU','ARE'])].to_string())
print("\nDone.")
