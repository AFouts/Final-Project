# Restore renv environment (should happen automatically)
renv::restore()

## Data Cleaning
source("2_analysis/1_clean-data.r")

## Analysis
source("2_analysis/2_run-regressions.r")

## Figures & Tables
source("2_analysis/3_tables.r")
source("2_analysis/4_figures.r")

## Run Tests
testthat::local_edition(3)
testthat::test_dir("2_analysis/tests")