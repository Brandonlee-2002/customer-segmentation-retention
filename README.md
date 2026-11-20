# Project 3: Customer Segmentation, Retention Modeling, and Marketing Allocation

This project uses the **dunnhumby Complete Journey** data to identify customer groups, predict whether households return during a holdout period, and recommend how to allocate a $1,000,000 marketing budget.

## Project structure

- `analysis/project_3_analysis.R` — reproducible analysis pipeline.
- `report/project_3_report.qmd` — completed Quarto report and required narrative.
- `data/` — local data exports, if needed. The default script loads the `completejourney` R package directly.
- `output/` — generated tables and figures.
- `background/` — original project brief supplied for this assignment.

## Reproduce the analysis

1. Install R packages once:

   ```r
   install.packages(c("completejourney", "tidyverse", "factoextra", "broom", "knitr", "scales"))
   ```

2. Run the analysis:

   ```sh
   Rscript analysis/project_3_analysis.R
   ```

3. Render the report:

   ```sh
   quarto render report/project_3_report.qmd
   ```

The analysis uses January 1–September 30 as the feature period and October 1–December 31 as the holdout period. The exact dates are inferred from the transaction data so the script remains reproducible if the package data are refreshed.

## Current status

The completed analysis includes customer-level feature construction, PCA variance and loading interpretation, seeded k-means diagnostics, four business segments, held-out logistic-regression evaluation, calibration and segment-level performance tables, and a non-equal $1,000,000 budget recommendation. The report documents the allocation as a planning framework and proposes randomized treatment/control testing because the observational data do not identify causal marketing lift.

Additional generated model outputs include `calibration.csv`, `model_comparison.csv`, `segment_model_performance.csv`, and `test_model_metrics.csv`.

## Git structure

The `main` branch contains the reproducible analysis, report source, rendered report, generated outputs, and the GitHub Pages site under `docs/`. Analysis code belongs in `analysis/`, report sources and rendered artifacts belong in `report/`, and reproducible tables belong in `output/`. The `.github/workflows/pages.yml` workflow publishes the contents of `docs/` whenever changes are pushed to `main`.
# customer-segmentation-retention
