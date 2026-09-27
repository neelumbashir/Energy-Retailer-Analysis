# Energy Retailer Performance: Customers in Financial Difficulty

An end-to-end data analysis project in **R**, modelled on how an energy regulator uses retailer data to identify consumer harm and target compliance activity.

📄 **[Read the full report](https://neelumbashir.github.io/Energy-Retailer-Analysis/report.html)**

> **Note:** All data in this project is **synthetic**, created for training and portfolio purposes. It is modelled on the kinds of indicators energy regulators collect. All retailer names are fictional, and any resemblance to real businesses is coincidental.

## Business questions

1. Are customers in debt receiving help?
2. Are retailers disconnecting customers who should have been supported first?
3. Are eligible customers receiving concessions?
4. Are customers paying more than they need to?
5. Which retailers provide the poorest service?

## Data

Five linked sources, July 2020 to June 2026:

- Quarterly retailer submissions: 24 quarters, 20 retailers, in six Excel workbooks with two template versions
- 151,041 complaint records
- 111,278 disconnection records
- Demographic data for 640 Victorian postcodes
- A randomised trial of two outreach letters (6,000 customers)

## Workflow

| Stage | Script | What it does |
|---|---|---|
| Load | `R/01_load.R` | Reads CSV files and 24 Excel sheets; harmonises two template versions |
| Quality checks | `R/02_quality.R` | Missing values, duplicates, inconsistent codes, impossible values, totals, dates, postcodes; 28 issues logged |
| Cleaning | `R/03_clean.R` | Applies every documented decision from the issues log |
| Indicators | `R/04_indicators.R` | Rates per 100 customers and industry averages |
| Exploration | `R/05_eda.R` | Retailer comparisons, trends, record-level and demographic analysis |
| Statistics | `R/06_statistics.R` | Tests every key finding |
| Report | `report.qmd` | Quarto report for a non-technical audience |

## Statistical methods

- **A/B test** (two-proportion test) for a randomised letter trial
- **Proportion tests** and **Wilcoxon rank-sum test** for retailer comparisons
- **Funnel plots** to identify outliers fairly, allowing for retailer size
- **Kruskal–Wallis test** with Holm-adjusted post-hoc comparisons
- **Poisson regression** with a population offset
- **Survival analysis** (Kaplan–Meier, log-rank test), treating open complaints as censored
- **Change-point analysis** to date a billing problem

## Key findings

- **Brightwave Energy** disconnected customers at nearly twice the industry rate, over smaller debts, and without offering assistance in 48% of cases (vs 8% elsewhere).
- **Nexa Energy** left about three times as many customers in arrears without assistance as the industry, and was slowest to resolve complaints.
- **Summit Energy** had less than half the industry's rate of customers receiving concessions.
- **Coreline Power** had a billing problem from October 2023 to September 2024, when billing complaints tripled.
- Disconnections were about **4.6 times higher** in the most disadvantaged areas.
- A redesigned letter increased help-seeking by **4.4 percentage points** (95% CI: 2.3 to 6.4).

![Disconnection funnel plot](outputs/figures/stats4a_funnel_disconnections.png)

![Disconnections by disadvantage](outputs/figures/eda8_disconnections_decile.png)

## Data quality

28 issues were identified and documented in [`outputs/issues_log.xlsx`](outputs/issues_log.xlsx), including 604 duplicate records, retailer name variants, mixed date formats, values reported in thousands, a decimal error and a revised submission. Each was resolved with a recorded decision.

## Tools

R (tidyverse, readxl, janitor, lubridate, survival, changepoint), Quarto

## How to run

1. Open the `.Rproj` file in RStudio.
2. Run the scripts in `R/` in order, from `01_load.R` to `06_statistics.R`.
3. Render `report.qmd` to produce the report.

## Author

**Neelum Bashir**: PhD in Applied Mathematics, transitioning into data analytics.
