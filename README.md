# Project 3: Customer Segmentation, Retention Modeling, and Marketing Allocation

This project uses the **dunnhumby Complete Journey** data to identify customer groups, predict whether households return during a holdout period, and recommend how to allocate a $1,000,000 marketing budget.

## Project structure

- `analysis/project_3_analysis.R` — reproducible analysis pipeline.
- `report/project_3_report.qmd` — Quarto report scaffold and required narrative.
- `data/` — local data exports, if needed. The default script loads the `completejourney` R package directly.
- `output/` — generated tables and figures.
- `background/` — original project brief supplied for this assignment.

## Reproduce the analysis

1. Install R packages once:

   ```r
   install.packages(c("completejourney", "tidyverse", "factoextra", "quarto"))
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

The repository is initialized with the customer-level feature construction, PCA, k-means diagnostics, segmentation profile, logistic regression evaluation, and a transparent budget-allocation framework. Final segment names and spending levels should be reviewed after inspecting the generated diagnostics and model results.
# customer-segmentation-retention
