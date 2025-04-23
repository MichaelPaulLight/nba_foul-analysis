This folder contains subfolders:
    1. 02-1_data_raw [raw data files]
    2. 02-2_data_clean [data files after any transformation]
    3. 02-3_data_modeling [data files after any pre-processing]

## Data Pipeline Overview

The NBA foul analysis project follows a structured data pipeline:

1. **Data Collection** (`02-1_data_raw`):
   - Play-by-play (PBP) data is collected using the hoopR package
   - NBA Last Two Minutes (L2M) reports are imported
   - Raw data is stored in Parquet format
   - Key files include player rosters, defender dashboards, and shooting data

2. **Data Cleaning** (`02-2_data_clean`):
   - Raw PBP data undergoes quality checks and corrections
   - Foul count inconsistencies are detected and fixed using a sequential correction algorithm
   - Shot-level data is organized from offensive and defensive perspectives
   - Summary statistics about data quality issues are generated

3. **Data Modeling** (`02-3_data_modeling`):
   - Various sample sizes of the dataset are created for modeling
   - Data is transformed into different formats (raw, scaled, factor) for analysis
   - Samples are structured to support different modeling approaches

## Key Data Processing Scripts

- `update-pbp.R`: Updates play-by-play data from NBA API
- `clean_pbp.qmd`: Performs data cleaning and transformation
- `import_l2m.qmd`: Imports Last Two Minutes reports
- `investigation_foul-count-inflation.qmd`: Analyzes data quality issues related to foul counts

## Data Quality Notes

The NBA play-by-play data contains some inconsistencies, particularly in personal foul counts, which are addressed in clean_pbp.qmd.

