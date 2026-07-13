# Is France at Risk? A Bond Market-Based Assessment of Fiscal Sustainability

Replication files accompanying the research paper:

> **Is France at Risk? A Bond Market-Based Assessment of Fiscal Sustainability**

---
## Replication Package

**DOI:** https://doi.org/10.5281/zenodo.21342625

## Overview

This repository contains the data, code, figures, and replication materials used in the paper:

**Is France at Risk? A Bond Market-Based Assessment of Fiscal Sustainability**

The paper investigates whether financial markets perceive France's current fiscal trajectory as sustainable by examining the determinants of French 10-year government bond yields using quarterly data from **2010Q1–2025Q1**.

Rather than relying solely on debt and deficit levels, the study emphasizes **debt dynamics**, particularly the **growth–interest rate differential (g − r)**, within a dynamic econometric framework.

---

## Abstract

France has experienced persistent fiscal deficits, rising public debt, sovereign credit downgrades, and increasing political uncertainty following the COVID-19 pandemic.

Using quarterly macroeconomic and financial data, this study estimates an **Autoregressive Distributed Lag (ARDL)** model to examine both the short-run and long-run determinants of French sovereign bond yields.

The analysis finds that:

- debt dynamics matter more than debt levels alone;
- the growth–interest rate differential is a significant long-run determinant of borrowing costs;
- fiscal deficits exhibit mixed effects depending on macroeconomic conditions;
- inflation significantly raises sovereign yields; and
- financial markets appear to price fiscal sustainability dynamically rather than mechanically.

---

## Repository Structure

```
.
├── data/
│   ├── raw/
│   └── processed/
│
├── code/
│   ├── 01_data_cleaning.R
│   ├── 02_descriptive_statistics.R
│   ├── 03_ardl_models.R
│   ├── 04_cointegration_tests.R
│   ├── 05_diagnostics.R
│   ├── 06_rolling_models.R
│   └── 07_figures_tables.R
│
├── figures/
│
├── LICENSE
└── README.md
```

---

## Research Question

**Do financial markets perceive France's current fiscal position as fiscally sustainable?**

The paper examines whether movements in French 10-year government bond yields are explained by:

- Debt-to-GDP ratio
- Deficit-to-GDP ratio
- Growth–interest rate differential (g − r)
- Inflation
- Economic Policy Uncertainty (EPU)
- EUR/USD exchange rate

---

## Methodology

The empirical analysis employs:

- Autoregressive Distributed Lag (ARDL) model
- Error Correction Model (ECM)
- Bounds test for cointegration
- Bai–Perron structural break tests
- Rolling-window ARDL / OLS estimation
- FMOLS and DOLS robustness checks
- Newey–West HAC standard errors
- Durbin–Watson and Breusch–Godfrey serial correlation tests
- White heteroskedasticity test
- Generalized Variance Inflation Factors (GVIF)

---

## Main Findings

The study finds:

- Strong persistence in French sovereign bond yields.
- Evidence of a stable long-run relationship between fiscal fundamentals and yields.
- The growth–interest rate differential (g − r) is the strongest long-run determinant of sovereign borrowing costs.
- Inflation significantly increases long-term bond yields.
- Debt levels alone provide limited information once debt dynamics are incorporated.
- Economic Policy Uncertainty is not statistically significant after controlling for macroeconomic fundamentals.
- Fiscal sustainability is better explained through dynamic debt conditions than through static debt thresholds.

---

## Data Sources

Publicly available data were obtained from:

- Banque de France
- European Central Bank (ECB)
- Eurostat
- Federal Reserve Economic Data (FRED)
- Economic Policy Uncertainty (EPU) Database

---

## Software

Analysis performed in:

- R (version 4.x)

Primary packages include:

- ARDL
- dynlm
- sandwich
- lmtest
- strucchange
- urca
- ggplot2
- dplyr
- zoo
- car

---

## Reproducing the Results

1. Clone this repository.
2. Open the project in RStudio.
3. Install required packages.
4. Run the R scripts in numerical order.

The scripts reproduce:

- descriptive statistics
- ARDL estimates
- long-run coefficients
- cointegration tests
- robustness checks
- diagnostic tests
- figures
- tables

---

## Citation

If you use these replication materials, please cite:

> Your Name.
> *Is France at Risk? A Bond Market-Based Assessment of Fiscal Sustainability.*

---

## License

MIT License

