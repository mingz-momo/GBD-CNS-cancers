# Global Burden, Socioeconomic Inequalities, and Welfare Losses of Brain and Other CNS Cancers, 1990–2023

This repository contains the source code for the study: **“Global burden, socioeconomic inequalities, and welfare losses of brain and other CNS cancers, 1990–2023: an integrated analysis of GBD 2023”**, submitted for publication.

This independent secondary analysis integrates epidemiological burden, cross-country socioeconomic inequality, welfare-based valuation, and overall health expenditure context using data from the Global Burden of Disease (GBD) Study 2023, the United Nations World Population Prospects 2024, and the World Bank.

## 1. Methodology Overview

The code implements the following analytical components:

- **Epidemiological trends**: Describes changes in incidence, prevalence, mortality, and disability-adjusted life years (DALYs) from 1990 to 2023 using counts, age-standardized rates, and estimated annual percentage changes.
- **Socioeconomic inequality**: Quantifies cross-country inequality using the Slope Index of Inequality (SII) and Concentration Index (CIX). Approximate Monte Carlo intervals are constructed from the publicly reported GBD uncertainty bounds and are not equivalent to intervals derived from joint GBD posterior draws.
- **Welfare-based valuation**: Estimates the value of lost welfare (VLW) by applying age-specific values of a statistical life year (VSLY) to age-specific DALYs. The primary value of a statistical life is transferred across countries using purchasing-power-parity-adjusted GDP per capita and an income elasticity of 1.0.
- **Valuation sensitivity analyses**: Examines alternative income elasticities and 22 structural scenarios varying age adjustment, annualization horizon, discounting, and age coverage.
- **Health expenditure context**: Compares VLW as a percentage of GDP with current health expenditure as a percentage of GDP using exploratory quartile classifications. Scenario-specific, fixed-quartile, median, and tertile analyses assess sensitivity to valuation assumptions and threshold definitions.

Welfare-loss totals are aggregated across the 188 countries and territories with the required 2023 economic inputs. The mismatch analysis includes 184 locations with complete VLW/GDP and health expenditure data. These classifications indicate relative discordance between welfare burden and overall health expenditure; they do not measure CNS cancer-specific spending or its adequacy.

## 2. Repository Structure

The repository currently contains the following files:

- **`CNS_cancers_GBD2023.R`**: Main script for epidemiological trends, inequality analyses, primary welfare-loss estimation, income-elasticity sensitivity analyses, figures, and exploratory mismatch classification.
- **`CNS_cancers_GBD2023_scenario_analysis.R`**: Script for the 22 structural scenarios and the scenario-specific, fixed-threshold, median, and tertile sensitivity analyses.
- **`README.md`**: Repository documentation.
- **`LICENSE`**: MIT License.
- **`.gitignore`**: Files and directories excluded from version control.

The input datasets and generated results are not included in this repository. To run the analysis locally, users should create a `data/` folder in the repository root. The scripts create the `results/` folder automatically when the analysis is run.

## 3. Data Availability

This repository contains the R analysis scripts only. Source and processed input datasets, as well as generated tables and figures, are not included in the repository.

The epidemiological inputs were obtained from the Global Burden of Disease Study 2023 Results Tool. Economic indicators were obtained from World Bank Open Data. Age-specific remaining life expectancy was derived from the United Nations World Population Prospects 2024.

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

Users should obtain the source data from the respective data providers and prepare the required input files in accordance with their data-use requirements. The scripts validate the expected years, measures, country coverage, age groups, economic indicators, and join keys before running the analyses.

## 4. Requirements

- **R version**: 4.5.2
- **Dependencies**: tidyverse 2.0.0, dplyr 1.1.4, tidyr 1.3.1, purrr 1.2.0, tibble 3.3.0, ggplot2 4.0.0, countrycode 1.8.0, maps 3.4.3, scales 1.4.0, and patchwork 1.3.2.

## 5. Usage

1. **Download the required data** from the original data providers.

2. **Prepare the input files** listed in the Data Availability section.

3. **Create the input directory** and place all prepared input files in it:

   ```text
   data/
