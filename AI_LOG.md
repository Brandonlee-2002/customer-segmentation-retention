# AI Use Log

## Project

Customer Segmentation, Retention Modeling, and Marketing Allocation

## Purpose

This log records how AI assistance was used during project development. The analysis results, business assumptions, and final decisions remain the responsibility of the project author.

## Assistance provided

| Area | AI assistance | Author responsibility / verification |
|---|---|---|
| Repository audit | Reviewed the assignment brief, repository structure, TODOs, generated outputs, and report scaffold. | Confirmed the assignment requirements and selected the final scope. |
| Analysis code | Suggested held-out evaluation, ROC-AUC, calibration, segment-level performance, seeded clustering diagnostics, and budget-table generation. | Ran the R script, inspected outputs, and reviewed model limitations. |
| PCA and clustering narrative | Drafted interpretations of PCA loadings, cluster profiles, segment labels, and the `k = 4` decision. | Checked interpretations against the generated loading, profile, and diagnostic tables. |
| Marketing recommendation | Drafted a non-equal $1,000,000 allocation and campaign actions by segment. | Reviewed the business logic and documented that allocation is not a causal estimate of marketing lift. |
| Report writing | Filled the Quarto report with the executive summary, methods, results, limitations, and experiment design. | Reviewed the final QMD and rendered HTML for completeness. |
| GitHub Pages | Added a static project landing page and a GitHub Actions Pages deployment workflow. | Confirmed the repository configuration and remains responsible for enabling Pages and reviewing the published site. |

## Validation performed

- Ran `Rscript analysis/project_3_analysis.R` successfully.
- Ran `quarto render report/project_3_report.qmd` successfully.
- Checked that the generated budget, PCA, clustering, model, calibration, and segment-performance tables were populated.
- Used held-out evaluation rather than relying only on in-sample accuracy.
- Documented the convergence warning caused by near-separation in the cluster-augmented logistic model.

## Limitations of AI assistance

AI did not independently verify causal marketing lift, profitability, contact costs, or campaign response. The budget allocation is a planning recommendation based on observational data and must be tested with randomized treatment and control groups. All numerical claims should be regenerated from the R script before submission.
