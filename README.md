# Urban Climate Health Resilience

> **Note:** The underlying data are not shared in the public version of this repository. They are available from the authors upon reasonable request.

This repository contains the R code needed to run the Urban Climate Health Resilience modelling workflow and reproduce the final outputs used for the manuscript tables and figures.

## How to run the model

1. Sync the full shared folder to your local computer, or copy the full folder to a local directory.
   - Technically, only the `01_data/01_raw` subfolder must be synced/stored locally, but the folder structure for `01_data/02_interim` and `01_data/03_final` must still exist or the code will fail to run. Syncing/storing the entire folder is recommended.
2. Open the R project file: `urban_climate_health_resilience.Rproj`.
3. Open the main workflow script `02_codes/00_full_run.R` and run from start to finish.

## Outputs

Running the model will overwrite files in:

- `01_data/02_interim`
- `01_data/03_final`

The final outputs needed for the manuscript tables and figures are saved in `01_data/03_final`. These files are named according to the table or figure they support, for example:

- `table_1.csv`, `table_1.rds`
- `table_2.csv`, `table_2.rds`
- `maps_mortality.gpkg`, `maps_mortality.csv`
- `table_s1.csv`, `table_s1.rds`
- `table_s2.csv`, `table_s2.rds`

Additional intermediate files required for the workflow are saved in `01_data/03_final/supplementary`.

## Runtime

The full workflow takes approximately 2 hours to run on a standard desktop PC with normal computing power.
