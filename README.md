# Brain and central nervous system cancer burden analysis

This repository contains the R code used for an independent secondary analysis of publicly available GBD 2023 estimates, socioeconomic inequality, welfare loss, and overall health expenditure context.

## Directory structure

```text
.
├── code/
│   ├── CNS_cancers_GBD2023.R
│   └── CNS_cancers_GBD2023_scenario_analysis.R
├── data/
└── results/
```

Run both scripts from the repository root. The main script must be completed before the scenario-analysis script:

```sh
Rscript code/CNS_cancers_GBD2023.R
Rscript code/CNS_cancers_GBD2023_scenario_analysis.R
```

The main script performs the epidemiological trend, inequality, primary welfare-loss, income-elasticity sensitivity, and exploratory mismatch analyses. The second script performs the 22 structural scenarios and the scenario-specific, fixed-threshold, median, and tertile classification sensitivity analyses.

## Required input files

Place the following files in `data/`:

- `SDI_data.csv`
- `Country_data.csv`
- `SDI_Number.csv`
- `Country_Number.csv`
- `Country_disease_burden.csv`
- `Population.csv`
- `DALYs-Number-Both-20-age.csv`
- `WPP2024_remaining_life_expectancy_2023.csv`
- `perGDP-2023PPP.csv`
- `totalGDP-2023PPP.csv`
- `HE-of-GDP.csv`

The scripts validate the expected years, measures, country coverage, age groups, economic indicators, and join keys before analysis.

## Software environment

The analysis was tested with R 4.5.2 and the following package versions:

- tidyverse 2.0.0
- dplyr 1.1.4
- tidyr 1.3.1
- purrr 1.2.0
- tibble 3.3.0
- ggplot2 4.0.0
- countrycode 1.8.0
- maps 3.4.3
- scales 1.4.0
- patchwork 1.3.2

## Interpretation of aggregate welfare loss

Welfare-loss totals are aggregated across the 188 countries and territories with the required 2023 economic inputs. They should not be interpreted as estimates covering all 204 GBD locations. The mismatch analysis uses the 184 locations with complete VLW/GDP and health-expenditure data and is intended as an exploratory comparison rather than a measure of disease-specific spending adequacy.

The lower and upper VLW totals are arithmetic sums obtained by applying the same valuation parameters to the reported lower and upper DALY bounds. They are not statistical 95% uncertainty intervals.
