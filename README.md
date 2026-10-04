# Global Burden, Socioeconomic Inequalities, and Welfare Losses of Brain and Other CNS Cancers, 1990–2023

This repository contains the R analysis scripts used in the study: **“Global burden, socioeconomic inequalities, and welfare losses of brain and other CNS cancers, 1990–2023: an integrated analysis of GBD 2023”**, submitted for publication.

This independent secondary analysis integrates epidemiological burden, cross-country socioeconomic inequality, welfare-based valuation, and overall health expenditure context using data from the Global Burden of Disease (GBD) Study 2023, the United Nations World Population Prospects 2024, and the World Bank.

## 1. Methodology Overview

The analysis includes the following components:

- **Epidemiological trends**: Changes in incidence, prevalence, mortality, and disability-adjusted life years (DALYs) from 1990 to 2023 were assessed using counts, age-standardized rates, and estimated annual percentage changes.

- **Socioeconomic inequality**: Cross-country inequalities were quantified using the Slope Index of Inequality (SII) and Concentration Index (CIX). Approximate Monte Carlo intervals were constructed from the publicly reported GBD uncertainty bounds. These intervals are not equivalent to intervals derived from joint GBD posterior draws.

- **Welfare-based valuation**: The value of lost welfare (VLW) was estimated by applying age-specific values of a statistical life year (VSLY) to age-specific DALYs. The primary value of a statistical life was transferred across countries using purchasing-power-parity-adjusted GDP per capita and an income elasticity of 1.0.

- **Valuation sensitivity analyses**: Alternative income elasticities and 22 structural scenarios were used to examine the effects of age adjustment, annualization horizon, discounting, and age coverage on absolute welfare estimates and cross-country patterns.

- **Health expenditure context**: VLW as a percentage of GDP was compared with current health expenditure as a percentage of GDP using exploratory quartile classifications. Scenario-specific thresholds, fixed primary thresholds, and alternative median and tertile cutoffs were examined to assess classification sensitivity.

Welfare-loss totals were aggregated across the 188 countries and territories with the required 2023 economic inputs. The exploratory mismatch analysis included 184 locations with complete VLW/GDP and health expenditure data.

The mismatch classifications indicate relative discordance between welfare burden and overall health expenditure. They do not measure CNS cancer-specific spending, spending adequacy, investment efficiency, or health-system performance.

## 2. Repository Structure

The repository contains the following files:

- **`CNS_cancers_GBD2023.R`**: Main script for epidemiological trends, inequality analyses, primary welfare-loss estimation, income-elasticity sensitivity analyses, figures, and exploratory mismatch classification.

- **`CNS_cancers_GBD2023_scenario_analysis.R`**: Script for the 22 structural scenarios and the scenario-specific, fixed-threshold, median, and tertile sensitivity analyses.

- **`README.md`**: Repository documentation.

- **`LICENSE`**: MIT License.

- **`.gitignore`**: Files and directories excluded from version control.

The input datasets and generated results are not included in this repository. To run the analysis locally, users should create a folder named `data/` in the repository root. The scripts automatically create the `results/` and `results/scenario_analysis/` folders.

## 3. Data Availability

This repository contains the R analysis scripts only. Source and processed input datasets, generated tables, and generated figures are not included.

The epidemiological inputs were obtained from the GBD 2023 Results Tool. Economic indicators were obtained from World Bank Open Data. Age-specific remaining life expectancy was derived from the United Nations World Population Prospects 2024.

To run the analysis locally, create a folder named `data/` in the repository root and place the following prepared input files inside it:

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

Users should obtain the source data from the respective data providers and prepare the required input files in accordance with the providers’ data-use requirements.

The scripts validate the expected years, measures, country coverage, age groups, economic indicators, and join keys before running the analyses.

## 4. Requirements

The analysis was tested using:

- **R version**: 4.5.2
- **tidyverse**: 2.0.0
- **dplyr**: 1.1.4
- **tidyr**: 1.3.1
- **purrr**: 1.2.0
- **tibble**: 3.3.0
- **ggplot2**: 4.0.0
- **countrycode**: 1.8.0
- **maps**: 3.4.3
- **scales**: 1.4.0
- **patchwork**: 1.3.2

Required packages can be installed in R using:

`install.packages(c("tidyverse", "countrycode", "maps", "scales", "patchwork"))`

## 5. Usage

1. Download the required data from the original data providers.

2. Prepare the input files listed in the Data Availability section.

3. Create a folder named `data/` in the repository root and place all prepared input files inside it.

4. Run the main analysis from the repository root using:

`Rscript CNS_cancers_GBD2023.R`

5. After the main analysis has completed, run the structural scenario analysis using:

`Rscript CNS_cancers_GBD2023_scenario_analysis.R`

6. Review the generated outputs in the following folders:

- `results/`
- `results/scenario_analysis/`

The scenario-analysis script must be run after the main script because it uses primary-analysis outputs stored in the `results/` folder.

## 6. Interpretation Notes

The lower and upper VLW totals are arithmetic sums obtained by applying the same valuation parameters to the reported lower and upper DALY bounds. They are not statistical 95% uncertainty intervals.

Absolute welfare-loss estimates are sensitive to valuation assumptions. Exploratory burden–expenditure classifications also depend on structural assumptions and threshold definitions and should not be interpreted as fixed country labels.

## 7. License

This project is licensed under the MIT License.
