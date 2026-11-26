# Customer Segmentation and Retention Strategy: Allocating a $1M Marketing Budget

### Project 3 · Customer analytics, retention modeling, and marketing allocation

[![Report](https://img.shields.io/badge/read-report-0f766e?style=flat-square)](https://brandonlee-2002.github.io/customer-segmentation-retention/)
[![Source](https://img.shields.io/badge/source-GitHub-14213d?style=flat-square&logo=github)](https://github.com/Brandonlee-2002/customer-segmentation-retention)

This project turns household purchase history into an actionable retention strategy using the dunnhumby Complete Journey data. It combines customer-level feature engineering, PCA, k-means segmentation, held-out logistic regression, and a $1,000,000 marketing allocation framework.

## Start here

Read the [published report](https://brandonlee-2002.github.io/customer-segmentation-retention/) for the executive summary, segment profiles, model evaluation, budget recommendation, and experiment design.

## Decision snapshot

| Decision area | Result |
|---|---|
| Customer groups | 4 actionable behavioral segments |
| Prediction target | Whether a household purchases during the October–December holdout period |
| Model evaluation | Stratified 80/20 holdout with ROC-AUC, calibration, and segment-level performance |
| Budget | Non-equal allocation totaling $1,000,000 |
| Core recommendation | Invest most in growth-potential regulars while protecting high-value loyal shoppers efficiently |

The allocation is a planning recommendation, not a causal estimate of marketing lift. A randomized treatment/control test is required before scaling campaign spend.

## Project structure

```text
analysis/       Reproducible R analysis pipeline
report/         Quarto source and rendered report artifacts
output/         Generated customer, PCA, cluster, model, and budget tables
docs/           Static Pages copy
background/     Assignment brief and supplied reference materials
AI_LOG.md       Record of AI assistance and validation
```

## Reproduce the analysis

Install the required R packages once:

```r
install.packages(c(
  "completejourney", "tidyverse", "factoextra",
  "broom", "knitr", "scales"
))
```

Run the analysis and render the report:

```sh
Rscript analysis/project_3_analysis.R
quarto render report/project_3_report.qmd
```

The feature period is January 1–September 30, and the holdout period is October 1–December 31. The script loads the Complete Journey data through the `completejourney` R package.

## Generated outputs

The pipeline writes customer-level data, PCA variance and loadings, cluster diagnostics, segment profiles, confusion matrices, held-out model comparisons, calibration tables, segment-level model performance, budget allocation and break-even sensitivity tables, plus six report figures to `output/`.

## Validation and limitations

- Behavioral variables are computed before the holdout period to avoid using future purchases as predictors.
- Clustering uses PCA scores and compares candidate values of `k` using within-cluster error, silhouette width, cluster size, and random-start diagnostics.
- Model results are evaluated on held-out data rather than only in-sample accuracy.
- The cluster-augmented logistic model has a documented convergence warning caused by near-separation in the smallest high-return segment.
- The observational data do not identify incremental marketing lift, margin, contact cost, or profitability.

## Published project

- [Open the published report](https://brandonlee-2002.github.io/customer-segmentation-retention/)
- [View the source repository](https://github.com/Brandonlee-2002/customer-segmentation-retention)
- [Read the AI use log](AI_LOG.md)

## Git structure

The `main` branch contains the analysis, report source, generated outputs, and the published Pages artifacts. Changes to the analysis are made in `analysis/`; report writing belongs in `report/project_3_report.qmd`; reproducible tables belong in `output/`; and the root `index.html` is the Pages entry point. The current project state is captured in the Git history.
