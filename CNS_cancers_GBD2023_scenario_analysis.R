# GBD 2023 CNS cancers: scenario analysis
#
# Run this module from the repository root after the main analysis.
# It produces the parameter-sensitivity and 22 structural-scenario tables
# reported in the manuscript and supplement.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(countrycode)
})

scenario_results_dir <- file.path("results", "scenario_analysis")
dir.create(scenario_results_dir, recursive = TRUE, showWarnings = FALSE)
data_dir <- "data"

write_scenario_csv <- function(x, file) {
  utils::write.csv(
    x,
    file.path(scenario_results_dir, file),
    row.names = FALSE,
    na = ""
  )
}

required_files <- c(
  file.path(data_dir, "DALYs-Number-Both-20-age.csv"),
  file.path(data_dir, "WPP2024_remaining_life_expectancy_2023.csv"),
  file.path(data_dir, "perGDP-2023PPP.csv"),
  file.path(data_dir, "totalGDP-2023PPP.csv"),
  file.path(data_dir, "HE-of-GDP.csv"),
  file.path("results", "VLW_US_benchmark_bridge.csv"),
  file.path("results", "VLW_mismatch_classification.csv")
)
if (any(!file.exists(required_files))) {
  stop(
    "Run CNS_cancers_GBD2023.R first. Missing: ",
    paste(required_files[!file.exists(required_files)], collapse = ", "),
    call. = FALSE
  )
}

age_raw <- read.csv(
  file.path(data_dir, "DALYs-Number-Both-20-age.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_age_columns <- c(
  "location_id", "location_name", "sex_id", "sex_name", "age_name",
  "measure_name", "metric_name", "year", "val", "lower", "upper"
)
if (!all(required_age_columns %in% names(age_raw))) {
  stop("The new 20-age-group DALY input is missing required columns.", call. = FALSE)
}

expected_age_names <- c(
  "<5 years", "5-9 years", "10-14 years", "15-19 years",
  "20-24 years", "25-29 years", "30-34 years", "35-39 years",
  "40-44 years", "45-49 years", "50-54 years", "55-59 years",
  "60-64 years", "65-69 years", "70-74 years", "75-79 years",
  "80-84 years", "85-89 years", "90-94 years", "95+ years"
)
aggregate_ids <- c(1L, 44634L, 44635L, 44636L, 44637L, 44639L)

if (
  !setequal(unique(age_raw$year), c(1990L, 2023L)) ||
  !identical(unique(age_raw$sex_name), "Both") ||
  !identical(unique(age_raw$measure_name),
             "DALYs (Disability-Adjusted Life Years)") ||
  !identical(unique(age_raw$metric_name), "Number") ||
  n_distinct(age_raw$location_id) != 210L ||
  !setequal(unique(age_raw$age_name), expected_age_names)
) {
  stop("The new DALY file does not have the expected GBD 2023 dimensions.", call. = FALSE)
}

age_country <- age_raw %>%
  filter(
    year == 2023,
    sex_id == 3,
    !location_id %in% aggregate_ids
  )

if (
  n_distinct(age_country$location_id) != 204L ||
  nrow(age_country) != 204L * 20L ||
  anyDuplicated(age_country[c("location_id", "age_name")])
) {
  stop("Expected 204 countries x 20 unique age groups in 2023.", call. = FALSE)
}

# Harmonise the GBD country labels and create ISO3 codes.
age_country$location_name[grepl(
  "^T.*kiye$", age_country$location_name, useBytes = TRUE
)] <- "Turkey"
age_country <- age_country %>%
  mutate(
    location_name_iso = recode(
      location_name,
      "Taiwan (Province of China)" = "Taiwan",
      "Palestine" = "State of Palestine",
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "Iran (Islamic Republic of)" = "Iran",
      "Bolivia (Plurinational State of)" = "Bolivia",
      "Democratic People's Republic of Korea" = "North Korea",
      "Republic of Korea" = "South Korea",
      "Russian Federation" = "Russia",
      "Republic of Moldova" = "Moldova",
      "Syrian Arab Republic" = "Syria",
      "United Republic of Tanzania" = "Tanzania",
      "Lao People's Democratic Republic" = "Laos",
      "Micronesia (Federated States of)" = "Micronesia",
      "Brunei Darussalam" = "Brunei",
      "Viet Nam" = "Vietnam",
      "Democratic Republic of the Congo" = "DR Congo",
      "Congo" = "Republic of the Congo"
    ),
    ISO3 = suppressWarnings(
      countrycode(location_name_iso, "country.name", "iso3c")
    ),
    ISO3 = if_else(location_name_iso == "Micronesia", "FSM", ISO3),
    age = case_when(
      age_name == "<5 years" ~ 3,
      age_name == "5-9 years" ~ 7,
      age_name == "10-14 years" ~ 12,
      age_name == "15-19 years" ~ 17,
      age_name == "20-24 years" ~ 22,
      age_name == "25-29 years" ~ 27,
      age_name == "30-34 years" ~ 32,
      age_name == "35-39 years" ~ 37,
      age_name == "40-44 years" ~ 42,
      age_name == "45-49 years" ~ 47,
      age_name == "50-54 years" ~ 52,
      age_name == "55-59 years" ~ 57,
      age_name == "60-64 years" ~ 62,
      age_name == "65-69 years" ~ 67,
      age_name == "70-74 years" ~ 72,
      age_name == "75-79 years" ~ 77,
      age_name == "80-84 years" ~ 82,
      age_name == "85-89 years" ~ 87,
      age_name == "90-94 years" ~ 92,
      age_name == "95+ years" ~ 97,
      TRUE ~ NA_real_
    )
  )

if (anyNA(age_country$ISO3) || anyNA(age_country$age)) {
  stop("One or more country names or age groups could not be mapped.", call. = FALSE)
}

country_lookup <- age_country %>%
  distinct(ISO3, country = location_name) %>%
  mutate(country = if_else(ISO3 == "TUR", "Türkiye", country))

life_table <- read.csv(
  file.path(data_dir, "WPP2024_remaining_life_expectancy_2023.csv"),
  stringsAsFactors = FALSE
) %>%
  filter(year == 2023) %>%
  select(ISO3, age, life_expectancy_at_birth, remaining_life_expectancy)

read_economic <- function(file, indicator, value_name) {
  d <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  if (
    !all(c("Country Code", "Indicator Code", "2023") %in% names(d)) ||
    any(d[["Indicator Code"]] != indicator)
  ) {
    stop("Unexpected economic input: ", file, call. = FALSE)
  }
  d %>%
    transmute(
      ISO3 = `Country Code`,
      value = suppressWarnings(as.numeric(`2023`))
    ) %>%
    rename(!!value_name := value)
}

perGDP <- read_economic(
  file.path(data_dir, "perGDP-2023PPP.csv"),
  "NY.GDP.PCAP.PP.CD", "perGDP"
)
totalGDP <- read_economic(
  file.path(data_dir, "totalGDP-2023PPP.csv"),
  "NY.GDP.MKTP.PP.CD", "totalGDP"
)
HE_GDP <- read_economic(
  file.path(data_dir, "HE-of-GDP.csv"),
  "SH.XPD.CHEX.GD.ZS", "HE_GDP"
)

age_input_20 <- age_country %>%
  transmute(
    ISO3, age_name, age,
    DALYs = val, lower, upper
  ) %>%
  left_join(life_table, by = c("ISO3", "age"), relationship = "many-to-one") %>%
  left_join(perGDP, by = "ISO3", relationship = "many-to-one") %>%
  left_join(totalGDP, by = "ISO3", relationship = "many-to-one") %>%
  left_join(HE_GDP, by = "ISO3", relationship = "many-to-one") %>%
  arrange(ISO3, age)

if (
  anyNA(age_input_20$life_expectancy_at_birth) ||
  anyNA(age_input_20$remaining_life_expectancy)
) {
  stop("The new DALY age groups did not fully match the WPP life table.", call. = FALSE)
}

age_coverage_check <- age_input_20 %>%
  count(ISO3, name = "n_age_groups")
if (
  nrow(age_coverage_check) != 204L ||
  any(age_coverage_check$n_age_groups != 20L) ||
  anyDuplicated(age_input_20[c("ISO3", "age")])
) {
  stop("The new 20-age-group input failed validation.", call. = FALSE)
}

VSL_peak_USA <- 13200000
GDP_USA_2023 <- age_input_20 %>%
  filter(ISO3 == "USA") %>%
  distinct(perGDP) %>%
  pull(perGDP)
if (length(GDP_USA_2023) != 1L || !is.finite(GDP_USA_2023)) {
  stop("A unique 2023 US PPP GDP per capita value is required.", call. = FALSE)
}

age_function <- function(ratio) {
  fa <- 19.41 * 0.236^4 -
    43.17 * 0.236^3 +
    27.65 * 0.236^2 -
    4.33 * 0.236 +
    0.44
  ifelse(
    ratio <= 0.236,
    fa,
    19.41 * ratio^4 -
      43.17 * ratio^3 +
      27.65 * ratio^2 -
      4.33 * ratio +
      0.44
  )
}

annuity_factor <- function(years, discount_rate) {
  if (discount_rate == 0) years else -expm1(-discount_rate * years) / discount_rate
}

calculate_scenario <- function(
    data,
    income_elasticity,
    age_ratio,
    annualisation_horizon,
    discount_rate,
    age_coverage
) {
  if (!age_ratio %in% c("a/(a+RLE)", "a/LE")) {
    stop("Unsupported age-ratio specification.", call. = FALSE)
  }
  if (!annualisation_horizon %in% c("RLE", "LE-a", "LE", "a+RLE")) {
    stop("Unsupported annualisation horizon.", call. = FALSE)
  }
  if (!age_coverage %in% c("All 20 age groups", "Only a<=LE")) {
    stop("Unsupported age-coverage rule.", call. = FALSE)
  }
  if (age_ratio == "a/LE" && age_coverage != "Only a<=LE") {
    stop("a/LE scenarios must be restricted to a<=LE.", call. = FALSE)
  }
  if (annualisation_horizon == "LE-a" && age_coverage != "Only a<=LE") {
    stop("LE-a scenarios require the restricted age coverage.", call. = FALSE)
  }
  
  out <- data %>%
    mutate(
      eligible_by_age = if (age_coverage == "Only a<=LE") {
        age <= life_expectancy_at_birth
      } else {
        rep(TRUE, n())
      },
      conversion_years = case_when(
        annualisation_horizon == "RLE" ~ remaining_life_expectancy,
        annualisation_horizon == "LE-a" ~ life_expectancy_at_birth - age,
        annualisation_horizon == "LE" ~ life_expectancy_at_birth,
        annualisation_horizon == "a+RLE" ~ age + remaining_life_expectancy
      ),
      eligible = eligible_by_age &
        is.finite(conversion_years) &
        conversion_years > 0,
      ratio = case_when(
        !eligible ~ NA_real_,
        age_ratio == "a/(a+RLE)" ~ age / (age + remaining_life_expectancy),
        age_ratio == "a/LE" ~ age / life_expectancy_at_birth
      ),
      f_age = if_else(eligible, age_function(ratio), NA_real_),
      VSL_peak = VSL_peak_USA * (perGDP / GDP_USA_2023)^income_elasticity,
      VSL_age = if_else(eligible, VSL_peak * f_age, 0),
      annualisation_factor = if_else(
        eligible,
        annuity_factor(conversion_years, discount_rate),
        NA_real_
      ),
      VSLY = if_else(eligible, VSL_age / annualisation_factor, 0),
      VLW = VSLY * DALYs,
      VLW_lower = VSLY * lower,
      VLW_upper = VSLY * upper
    )
  
  invalid <- out %>%
    filter(
      eligible & !is.na(perGDP) &
        (!is.finite(ratio) | ratio < 0 | ratio > 1 |
           !is.finite(f_age) | f_age <= 0 |
           !is.finite(annualisation_factor) | annualisation_factor <= 0 |
           !is.finite(VSLY) | VSLY <= 0)
    )
  if (nrow(invalid)) {
    stop("A scenario produced invalid eligible age rows.", call. = FALSE)
  }
  
  out
}

sum_if_complete <- function(x) {
  if (any(is.na(x)) || any(!is.finite(x))) NA_real_ else sum(x)
}

summarise_country <- function(age_result) {
  result <- age_result %>%
    group_by(ISO3) %>%
    summarise(
      n_age_groups = n(),
      n_age_groups_included = sum(eligible),
      included_DALYs = sum(DALYs[eligible]),
      perGDP = first(perGDP),
      totalGDP = first(totalGDP),
      HE_GDP = first(HE_GDP),
      VLW = sum_if_complete(VLW),
      VLW_lower = sum_if_complete(VLW_lower),
      VLW_upper = sum_if_complete(VLW_upper),
      .groups = "drop"
    ) %>%
    mutate(
      VLW_available = is.finite(perGDP) & is.finite(VLW),
      VLW_GDP_available = VLW_available & is.finite(totalGDP) & totalGDP > 0,
      classification_eligible = VLW_GDP_available & is.finite(HE_GDP),
      VLW_GDP = if_else(VLW_GDP_available, VLW / totalGDP * 100, NA_real_),
      VLW = VLW / 1e6,
      VLW_lower = VLW_lower / 1e6,
      VLW_upper = VLW_upper / 1e6,
      VLW_bound_sum_range = if_else(
        VLW_available,
        sprintf("%.2f (%.2f-%.2f)", VLW, VLW_lower, VLW_upper),
        NA_character_
      )
    )
  
  if (
    nrow(result) != 204L ||
    any(result$n_age_groups != 20L) ||
    any(result$VLW_available & !is.finite(result$VLW))
  ) {
    stop("Country scenario aggregation failed validation.", call. = FALSE)
  }
  result
}

classify_country <- function(country_result) {
  complete <- country_result %>% filter(classification_eligible)
  if (!nrow(complete)) stop("No complete classification cases.", call. = FALSE)
  
  he_q <- quantile(complete$HE_GDP, c(0.25, 0.75), names = FALSE)
  vlw_q <- quantile(complete$VLW_GDP, c(0.25, 0.75), names = FALSE)
  
  complete %>%
    mutate(
      HE_flag = case_when(
        HE_GDP <= he_q[[1]] ~ "Low",
        HE_GDP >= he_q[[2]] ~ "High",
        TRUE ~ "Middle"
      ),
      VLW_flag = case_when(
        VLW_GDP <= vlw_q[[1]] ~ "Low",
        VLW_GDP >= vlw_q[[2]] ~ "High",
        TRUE ~ "Middle"
      ),
      classification = case_when(
        HE_flag == "Low" & VLW_flag == "High" ~ "Low HE / High VLW",
        HE_flag == "High" & VLW_flag == "Low" ~ "High HE / Low VLW",
        TRUE ~ "Other combinations"
      ),
      HE_GDP_Q1 = he_q[[1]],
      HE_GDP_Q3 = he_q[[2]],
      VLW_GDP_Q1 = vlw_q[[1]],
      VLW_GDP_Q3 = vlw_q[[2]]
    )
}

normalise_classification <- function(x) {
  recode(x, "Buffer / Middle" = "Other combinations")
}

classify_with_fixed_thresholds <- function(
    country_result,
    eligible_iso3,
    he_low,
    he_high,
    vlw_low,
    vlw_high
) {
  complete <- country_result %>%
    filter(ISO3 %in% eligible_iso3) %>%
    arrange(match(ISO3, eligible_iso3))
  
  if (
    nrow(complete) != length(eligible_iso3) ||
    !identical(complete$ISO3, eligible_iso3) ||
    any(!complete$classification_eligible) ||
    any(!is.finite(complete$HE_GDP)) ||
    any(!is.finite(complete$VLW_GDP))
  ) {
    stop(
      "A structural scenario does not reproduce the primary eligible-country set.",
      call. = FALSE
    )
  }
  
  complete %>%
    mutate(
      classification = case_when(
        HE_GDP <= he_low & VLW_GDP >= vlw_high ~ "Low HE / High VLW",
        HE_GDP >= he_high & VLW_GDP <= vlw_low ~ "High HE / Low VLW",
        TRUE ~ "Other combinations"
      ),
      fixed_HE_lower_threshold = he_low,
      fixed_HE_upper_threshold = he_high,
      fixed_VLW_lower_threshold = vlw_low,
      fixed_VLW_upper_threshold = vlw_high
    )
}

# Valid structural combinations. The restrictions reduce the 32-cell complete
# crossing to 22 valid scenarios:
# - a/LE is only evaluated for a<=LE;
# - LE-a is only evaluated for a<=LE and rows with a positive horizon.
structural_design <- crossing(
  age_ratio = c("a/(a+RLE)", "a/LE"),
  annualisation_horizon = c("RLE", "LE-a", "LE", "a+RLE"),
  discount_rate = c(0.03, 0),
  age_coverage = c("All 20 age groups", "Only a<=LE")
) %>%
  filter(
    !(age_ratio == "a/LE" & age_coverage != "Only a<=LE"),
    !(annualisation_horizon == "LE-a" & age_coverage != "Only a<=LE")
  ) %>%
  mutate(
    is_primary = age_ratio == "a/(a+RLE)" &
      annualisation_horizon == "RLE" &
      discount_rate == 0.03 &
      age_coverage == "All 20 age groups"
  ) %>%
  arrange(desc(is_primary), age_ratio, annualisation_horizon,
          desc(discount_rate), age_coverage) %>%
  mutate(
    scenario_id = sprintf("S%02d", row_number()),
    scenario_name = paste(
      age_ratio,
      annualisation_horizon,
      paste0(discount_rate * 100, "%"),
      age_coverage,
      sep = " | "
    ),
    income_elasticity = 1.0,
    .before = 1
  )

if (nrow(structural_design) != 22L || sum(structural_design$is_primary) != 1L) {
  stop("The structural design must contain 22 scenarios including one primary scenario.", call. = FALSE)
}

run_one <- function(IE, age_ratio, annualisation_horizon, discount_rate, age_coverage) {
  calculate_scenario(
    age_input_20,
    income_elasticity = IE,
    age_ratio = age_ratio,
    annualisation_horizon = annualisation_horizon,
    discount_rate = discount_rate,
    age_coverage = age_coverage
  ) %>%
    summarise_country()
}

# Parameter sensitivity: preserve the primary age function, RLE horizon,
# 3% discount rate, and all 20 age groups; vary only income elasticity.
parameter_design <- tibble(
  scenario_id = c("IE055", "IE100_PRIMARY", "IE150"),
  scenario_name = c("IE=0.55", "IE=1.00 (primary)", "IE=1.50"),
  income_elasticity = c(0.55, 1.0, 1.5),
  age_ratio = "a/(a+RLE)",
  annualisation_horizon = "RLE",
  discount_rate = 0.03,
  age_coverage = "All 20 age groups"
)

parameter_country <- pmap_dfr(
  parameter_design,
  function(scenario_id, scenario_name, income_elasticity, age_ratio,
           annualisation_horizon, discount_rate, age_coverage) {
    run_one(
      income_elasticity, age_ratio, annualisation_horizon,
      discount_rate, age_coverage
    ) %>%
      mutate(
        scenario_id = scenario_id,
        scenario_name = scenario_name,
        income_elasticity = income_elasticity,
        .before = 1
      )
  }
)

primary_parameter_country <- parameter_country %>%
  filter(scenario_id == "IE100_PRIMARY") %>%
  select(
    ISO3,
    primary_VLW = VLW,
    primary_VLW_GDP = VLW_GDP
  )

parameter_country <- parameter_country %>%
  left_join(primary_parameter_country, by = "ISO3") %>%
  mutate(
    VLW_difference_from_primary = VLW - primary_VLW,
    VLW_to_primary_ratio = VLW / primary_VLW,
    VLW_GDP_difference_from_primary = VLW_GDP - primary_VLW_GDP
  )

parameter_classification <- parameter_country %>%
  group_split(scenario_id) %>%
  map_dfr(function(d) {
    meta <- d %>% distinct(scenario_id, scenario_name, income_elasticity)
    classify_country(d) %>%
      mutate(
        scenario_id = meta$scenario_id,
        scenario_name = meta$scenario_name,
        income_elasticity = meta$income_elasticity,
        .before = 1
      )
  })

primary_parameter_class <- parameter_classification %>%
  filter(scenario_id == "IE100_PRIMARY") %>%
  select(ISO3, primary_classification = classification)

parameter_classification <- parameter_classification %>%
  left_join(primary_parameter_class, by = "ISO3") %>%
  mutate(classification_changed = classification != primary_classification)

parameter_summary <- parameter_country %>%
  group_by(scenario_id, scenario_name, income_elasticity) %>%
  summarise(
    VLW_estimable_n = sum(VLW_available),
    classification_complete_n = sum(classification_eligible),
    aggregate_VLW_million = sum(VLW[VLW_available]),
    aggregate_VLW_billion = aggregate_VLW_million / 1000,
    .groups = "drop"
  ) %>%
  left_join(
    parameter_classification %>%
      group_by(scenario_id) %>%
      summarise(
        classification_changed_n = sum(classification_changed),
        classification_changed_percent = mean(classification_changed) * 100,
        .groups = "drop"
      ),
    by = "scenario_id"
  ) %>%
  mutate(
    aggregate_VLW_to_primary_ratio =
      aggregate_VLW_million /
      aggregate_VLW_million[scenario_id == "IE100_PRIMARY"]
  )

# Exploratory structural scenarios: IE is fixed at 1.0.
structural_country <- pmap_dfr(
  structural_design,
  function(scenario_id, scenario_name, income_elasticity, age_ratio,
           annualisation_horizon, discount_rate, age_coverage, is_primary) {
    run_one(
      income_elasticity, age_ratio, annualisation_horizon,
      discount_rate, age_coverage
    ) %>%
      mutate(
        scenario_id = scenario_id,
        scenario_name = scenario_name,
        age_ratio = age_ratio,
        annualisation_horizon = annualisation_horizon,
        discount_rate = discount_rate,
        age_coverage = age_coverage,
        is_primary = is_primary,
        .before = 1
      )
  }
)

structural_classification <- structural_country %>%
  group_split(scenario_id) %>%
  map_dfr(function(d) {
    meta <- d %>%
      distinct(
        scenario_id, scenario_name, age_ratio,
        annualisation_horizon, discount_rate, age_coverage, is_primary
      )
    classify_country(d) %>%
      mutate(
        scenario_id = meta$scenario_id,
        scenario_name = meta$scenario_name,
        age_ratio = meta$age_ratio,
        annualisation_horizon = meta$annualisation_horizon,
        discount_rate = meta$discount_rate,
        age_coverage = meta$age_coverage,
        is_primary = meta$is_primary,
        .before = 1
      )
  })

primary_structural_country <- structural_country %>%
  filter(is_primary) %>%
  select(ISO3, primary_VLW = VLW, primary_VLW_GDP = VLW_GDP)
primary_structural_class <- structural_classification %>%
  filter(is_primary) %>%
  select(ISO3, primary_classification = classification)

structural_country <- structural_country %>%
  left_join(primary_structural_country, by = "ISO3") %>%
  mutate(
    VLW_to_primary_ratio = VLW / primary_VLW,
    VLW_GDP_to_primary_ratio = VLW_GDP / primary_VLW_GDP
  )

structural_classification <- structural_classification %>%
  left_join(primary_structural_class, by = "ISO3") %>%
  mutate(classification_changed = classification != primary_classification)

structural_summary <- structural_country %>%
  group_by(
    scenario_id, scenario_name, age_ratio, annualisation_horizon,
    discount_rate, age_coverage, is_primary
  ) %>%
  summarise(
    VLW_estimable_n = sum(VLW_available),
    classification_complete_n = sum(classification_eligible),
    aggregate_VLW_million = sum(VLW[VLW_available]),
    aggregate_VLW_billion = aggregate_VLW_million / 1000,
    median_country_VLW_to_primary = median(
      VLW_to_primary_ratio[VLW_available],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  left_join(
    structural_classification %>%
      group_by(scenario_id) %>%
      summarise(
        classification_changed_n = sum(classification_changed),
        classification_changed_percent = mean(classification_changed) * 100,
        .groups = "drop"
      ),
    by = "scenario_id"
  ) %>%
  mutate(
    aggregate_VLW_to_primary_ratio =
      aggregate_VLW_million /
      aggregate_VLW_million[is_primary]
  )

# Fixed-threshold mismatch sensitivity analysis. The primary mismatch export is
# the authoritative source for both the eligible-country set and the four
# primary-analysis thresholds. Structural scenarios change only VLW/GDP.
primary_mismatch_export <- read.csv(
  file.path("results", "VLW_mismatch_classification.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
required_primary_columns <- c(
  "ISO3", "HE_GDP", "VLW_GDP", "group",
  "HE_GDP_Q1", "HE_GDP_Q3", "VLW_GDP_Q1", "VLW_GDP_Q3"
)
if (
  !all(required_primary_columns %in% names(primary_mismatch_export)) ||
  anyDuplicated(primary_mismatch_export$ISO3)
) {
  stop("The primary mismatch export is missing required unique-country fields.", call. = FALSE)
}

primary_threshold_rows <- primary_mismatch_export %>%
  distinct(HE_GDP_Q1, HE_GDP_Q3, VLW_GDP_Q1, VLW_GDP_Q3)
if (nrow(primary_threshold_rows) != 1L) {
  stop("Primary mismatch thresholds are not constant in the primary export.", call. = FALSE)
}
primary_thresholds <- primary_threshold_rows %>%
  transmute(
    HE_GDP_Q1,
    HE_GDP_Q3,
    VLW_GDP_Q1,
    VLW_GDP_Q3
  )

primary_reference <- primary_mismatch_export %>%
  transmute(
    ISO3,
    primary_classification = normalise_classification(group)
  ) %>%
  arrange(ISO3)
primary_eligible_iso3 <- primary_reference$ISO3

primary_structural_values <- structural_country %>%
  filter(is_primary, ISO3 %in% primary_eligible_iso3) %>%
  arrange(ISO3)
recomputed_primary_thresholds <- c(
  quantile(primary_structural_values$HE_GDP, c(0.25, 0.75), names = FALSE),
  quantile(primary_structural_values$VLW_GDP, c(0.25, 0.75), names = FALSE)
)
exported_primary_thresholds <- unlist(primary_thresholds[1, ], use.names = FALSE)
if (
  !identical(primary_structural_values$ISO3, primary_eligible_iso3) ||
  any(abs(recomputed_primary_thresholds - exported_primary_thresholds) > 1e-12)
) {
  stop(
    "The structural primary scenario does not reproduce the primary mismatch sample or thresholds.",
    call. = FALSE
  )
}

structural_fixed_classification <- structural_country %>%
  group_split(scenario_id) %>%
  map_dfr(function(d) {
    meta <- d %>%
      distinct(
        scenario_id, scenario_name, age_ratio,
        annualisation_horizon, discount_rate, age_coverage, is_primary
      )
    classify_with_fixed_thresholds(
      d,
      eligible_iso3 = primary_eligible_iso3,
      he_low = primary_thresholds$HE_GDP_Q1[[1]],
      he_high = primary_thresholds$HE_GDP_Q3[[1]],
      vlw_low = primary_thresholds$VLW_GDP_Q1[[1]],
      vlw_high = primary_thresholds$VLW_GDP_Q3[[1]]
    ) %>%
      mutate(
        scenario_id = meta$scenario_id,
        scenario_name = meta$scenario_name,
        age_ratio = meta$age_ratio,
        annualisation_horizon = meta$annualisation_horizon,
        discount_rate = meta$discount_rate,
        age_coverage = meta$age_coverage,
        is_primary = meta$is_primary,
        threshold_method = "Fixed primary quartiles",
        .before = 1
      )
  }) %>%
  left_join(primary_reference, by = "ISO3") %>%
  mutate(classification_changed = classification != primary_classification)

primary_fixed_check <- structural_fixed_classification %>%
  filter(is_primary) %>%
  summarise(all_match = all(classification == primary_classification)) %>%
  pull(all_match)
if (!primary_fixed_check) {
  stop("Fixed-threshold classification does not reproduce the primary classification.", call. = FALSE)
}

scenario_specific_comparison <- structural_classification %>%
  transmute(
    scenario_id,
    ISO3,
    scenario_specific_classification = normalise_classification(classification)
  ) %>%
  left_join(primary_reference, by = "ISO3") %>%
  mutate(
    scenario_specific_changed =
      scenario_specific_classification != primary_classification
  )

scenario_specific_stats <- scenario_specific_comparison %>%
  group_by(scenario_id) %>%
  summarise(
    changed_under_scenario_specific_thresholds =
      sum(scenario_specific_changed),
    .groups = "drop"
  )

fixed_scenario_stats <- structural_fixed_classification %>%
  group_by(scenario_id) %>%
  summarise(
    changed_under_fixed_thresholds = sum(classification_changed),
    percentage_changing_classification = mean(classification_changed) * 100,
    low_HE_high_VLW_count = sum(classification == "Low HE / High VLW"),
    high_HE_low_VLW_count = sum(classification == "High HE / Low VLW"),
    primary_low_HE_high_VLW_retained = sum(
      primary_classification == "Low HE / High VLW" &
        classification == "Low HE / High VLW"
    ),
    primary_high_HE_low_VLW_retained = sum(
      primary_classification == "High HE / Low VLW" &
        classification == "High HE / Low VLW"
    ),
    Azerbaijan_classification = classification[ISO3 == "AZE"],
    Turkiye_classification = classification[ISO3 == "TUR"],
    .groups = "drop"
  )

fixed_threshold_comparison_summary <- structural_summary %>%
  select(
    scenario_id, scenario_name, is_primary,
    total_VLW_million = aggregate_VLW_million,
    total_VLW_billion = aggregate_VLW_billion
  ) %>%
  left_join(scenario_specific_stats, by = "scenario_id") %>%
  left_join(fixed_scenario_stats, by = "scenario_id")

fixed_threshold_scenario_sensitivity <-
  fixed_threshold_comparison_summary %>%
  transmute(
    scenario_ID = scenario_id,
    scenario_description = scenario_name,
    total_VLW_million,
    number_of_countries_changing_classification =
      changed_under_fixed_thresholds,
    percentage_changing_classification,
    low_HE_high_VLW_count,
    high_HE_low_VLW_count
  )

fixed_threshold_country_stability <- structural_fixed_classification %>%
  group_by(ISO3, primary_classification) %>%
  summarise(
    number_of_structural_scenarios = n(),
    number_classified_as_low_HE_high_VLW =
      sum(classification == "Low HE / High VLW"),
    number_classified_as_high_HE_low_VLW =
      sum(classification == "High HE / Low VLW"),
    number_classified_as_other =
      sum(classification == "Other combinations"),
    proportion_retaining_primary_classification =
      mean(classification == primary_classification),
    minimum_VLW_GDP_across_scenarios = min(VLW_GDP),
    maximum_VLW_GDP_across_scenarios = max(VLW_GDP),
    .groups = "drop"
  ) %>%
  left_join(country_lookup, by = "ISO3") %>%
  select(country, ISO3, everything()) %>%
  arrange(proportion_retaining_primary_classification, country)

# Additional cutoff sensitivity: primary medians and primary tertiles are held
# fixed across all structural scenarios, just like the primary quartiles above.
cutoff_definitions <- bind_rows(
  tibble(
    threshold_definition = "Median split",
    HE_lower_threshold = median(primary_mismatch_export$HE_GDP),
    HE_upper_threshold = median(primary_mismatch_export$HE_GDP),
    VLW_lower_threshold = median(primary_mismatch_export$VLW_GDP),
    VLW_upper_threshold = median(primary_mismatch_export$VLW_GDP)
  ),
  tibble(
    threshold_definition = "Tertile thresholds",
    HE_lower_threshold = quantile(
      primary_mismatch_export$HE_GDP, 1 / 3, names = FALSE
    ),
    HE_upper_threshold = quantile(
      primary_mismatch_export$HE_GDP, 2 / 3, names = FALSE
    ),
    VLW_lower_threshold = quantile(
      primary_mismatch_export$VLW_GDP, 1 / 3, names = FALSE
    ),
    VLW_upper_threshold = quantile(
      primary_mismatch_export$VLW_GDP, 2 / 3, names = FALSE
    )
  )
)

fixed_cutoff_classification <- map_dfr(
  seq_len(nrow(cutoff_definitions)),
  function(i) {
    cutoff <- cutoff_definitions[i, ]
    structural_country %>%
      group_split(scenario_id) %>%
      map_dfr(function(d) {
        meta <- d %>%
          distinct(scenario_id, scenario_name, is_primary)
        classify_with_fixed_thresholds(
          d,
          eligible_iso3 = primary_eligible_iso3,
          he_low = cutoff$HE_lower_threshold[[1]],
          he_high = cutoff$HE_upper_threshold[[1]],
          vlw_low = cutoff$VLW_lower_threshold[[1]],
          vlw_high = cutoff$VLW_upper_threshold[[1]]
        ) %>%
          mutate(
            threshold_definition = cutoff$threshold_definition[[1]],
            scenario_id = meta$scenario_id,
            scenario_name = meta$scenario_name,
            is_primary = meta$is_primary,
            .before = 1
          )
      })
  }
)

cutoff_primary_classification <- fixed_cutoff_classification %>%
  filter(is_primary) %>%
  select(
    threshold_definition,
    ISO3,
    cutoff_primary_classification = classification
  )

fixed_cutoff_classification <- fixed_cutoff_classification %>%
  left_join(
    cutoff_primary_classification,
    by = c("threshold_definition", "ISO3")
  ) %>%
  left_join(primary_reference, by = "ISO3") %>%
  mutate(
    classification_changed_from_cutoff_primary =
      classification != cutoff_primary_classification
  )

fixed_threshold_cutoff_sensitivity <- fixed_cutoff_classification %>%
  group_by(threshold_definition, scenario_id, scenario_name) %>%
  summarise(
    number_of_countries_changing_classification =
      sum(classification_changed_from_cutoff_primary),
    percentage_changing_classification =
      mean(classification_changed_from_cutoff_primary) * 100,
    low_HE_high_VLW_count = sum(classification == "Low HE / High VLW"),
    high_HE_low_VLW_count = sum(classification == "High HE / Low VLW"),
    Azerbaijan_classification = classification[ISO3 == "AZE"],
    Turkiye_classification = classification[ISO3 == "TUR"],
    .groups = "drop"
  ) %>%
  left_join(cutoff_definitions, by = "threshold_definition")

fixed_threshold_cutoff_country_stability <- fixed_cutoff_classification %>%
  group_by(
    threshold_definition, ISO3,
    cutoff_primary_classification, primary_classification
  ) %>%
  summarise(
    number_of_structural_scenarios = n(),
    number_classified_as_low_HE_high_VLW =
      sum(classification == "Low HE / High VLW"),
    number_classified_as_high_HE_low_VLW =
      sum(classification == "High HE / Low VLW"),
    number_classified_as_other =
      sum(classification == "Other combinations"),
    proportion_retaining_cutoff_primary_classification =
      mean(classification == cutoff_primary_classification),
    .groups = "drop"
  ) %>%
  mutate(
    primary_quartile_low_HE_high_VLW =
      primary_classification == "Low HE / High VLW"
  ) %>%
  left_join(country_lookup, by = "ISO3") %>%
  select(country, ISO3, everything()) %>%
  arrange(
    threshold_definition,
    proportion_retaining_cutoff_primary_classification,
    country
  )

# United States comparison with the published benchmark. Each scenario's
# current US estimate is divided by the observed change in total US DALYs and
# the reference VSL to obtain an approximate benchmark-year value.
benchmark <- read.csv(
  file.path("results", "VLW_US_benchmark_bridge.csv"),
  stringsAsFactors = FALSE
)
if (nrow(benchmark) != 1L) stop("Expected one US benchmark row.", call. = FALSE)

US_scenario_comparison <- structural_country %>%
  filter(ISO3 == "USA") %>%
  transmute(
    analysis = "Exploratory structural scenario",
    scenario_id,
    scenario_name,
    scenario_US_DALYs = included_DALYs,
    current_US_VLW_million = VLW
  ) %>%
  mutate(
    published_study = benchmark$published_study[[1]],
    published_year = benchmark$published_year[[1]],
    current_year = benchmark$current_year[[1]],
    published_US_DALYs = benchmark$published_US_DALYs[[1]],
    DALY_year_factor = scenario_US_DALYs / published_US_DALYs,
    published_reference_VSL = benchmark$published_reference_VSL[[1]],
    current_reference_VSL = benchmark$current_reference_VSL[[1]],
    reference_VSL_factor = benchmark$reference_VSL_factor[[1]],
    DALY_and_VSL_scaling_factor =
      DALY_year_factor * reference_VSL_factor,
    approximate_benchmark_year_US_VLW_million =
      current_US_VLW_million / DALY_and_VSL_scaling_factor,
    published_US_VLW_million = benchmark$published_US_VLW[[1]] / 1e6,
    approximate_to_published_ratio =
      approximate_benchmark_year_US_VLW_million /
      published_US_VLW_million
  )

coverage_audit <- tibble(
  item = c(
    "GBD locations",
    "GBD-provided age groups used directly",
    "VLW-estimable locations with 2023 inputs",
    "Complete classification cases with 2023 inputs",
    "Parameter scenarios including primary",
    "Valid structural scenarios including primary"
  ),
  value = c(
    204,
    20,
    unique(parameter_summary$VLW_estimable_n),
    unique(parameter_summary$classification_complete_n),
    nrow(parameter_design),
    nrow(structural_design)
  ),
  status = c(
    "PASS", "PASS",
    if_else(unique(parameter_summary$VLW_estimable_n) == 188L, "PASS", "FAIL"),
    if_else(unique(parameter_summary$classification_complete_n) == 184L, "PASS", "FAIL"),
    if_else(nrow(parameter_design) == 3L, "PASS", "FAIL"),
    if_else(nrow(structural_design) == 22L, "PASS", "FAIL")
  )
)

write_scenario_csv(age_coverage_check, "scenario_age_group_coverage.csv")
write_scenario_csv(
  age_input_20 %>% distinct(age_name, age) %>% arrange(age),
  "scenario_age_group_definitions.csv"
)
write_scenario_csv(coverage_audit, "scenario_coverage_audit.csv")
write_scenario_csv(parameter_design, "parameter_sensitivity_design.csv")
write_scenario_csv(parameter_summary, "parameter_sensitivity_summary.csv")
write_scenario_csv(parameter_country, "parameter_sensitivity_country_results.csv")
write_scenario_csv(
  parameter_classification,
  "parameter_sensitivity_classification_changes.csv"
)
write_scenario_csv(structural_design, "structural_scenario_design_22.csv")
write_scenario_csv(structural_summary, "structural_scenario_summary.csv")
write_scenario_csv(structural_country, "structural_scenario_country_results.csv")
write_scenario_csv(
  structural_classification,
  "structural_scenario_classification_changes.csv"
)
write_scenario_csv(
  structural_fixed_classification,
  "fixed_threshold_structural_classification.csv"
)
write_scenario_csv(
  fixed_threshold_comparison_summary,
  "fixed_threshold_comparison_summary.csv"
)
write_scenario_csv(
  fixed_threshold_country_stability,
  "fixed_threshold_country_stability.csv"
)
write_scenario_csv(
  fixed_threshold_scenario_sensitivity,
  "fixed_threshold_scenario_sensitivity.csv"
)
write_scenario_csv(
  fixed_threshold_cutoff_sensitivity,
  "fixed_threshold_cutoff_sensitivity.csv"
)
write_scenario_csv(
  fixed_threshold_cutoff_country_stability,
  "fixed_threshold_cutoff_country_stability.csv"
)
write_scenario_csv(US_scenario_comparison, "US_scenario_benchmark_comparison.csv")

scenario_validation <- tibble(
  check = c(
    "204 GBD locations",
    "No derived age-1 residual used",
    "20 age groups per location",
    "Three parameter scenarios",
    "Parameter analysis changes IE only",
    "Twenty-two valid structural scenarios",
    "One structural primary scenario",
    "188 locations with estimable VLW",
    "184 complete classification cases",
    "Country rows complete for every structural scenario",
    "Classification rows use a common complete-case set",
    "Fixed thresholds reproduce primary classification",
    "Fixed-threshold rows use the primary eligible-country set",
    "Median and tertile analyses use all structural scenarios",
    "US benchmark comparison produced"
  ),
  pass = c(
    n_distinct(age_input_20$ISO3) == 204L,
    !any(age_input_20$age_name == "Age 1 residual"),
    all(age_coverage_check$n_age_groups == 20L),
    nrow(parameter_design) == 3L,
    n_distinct(parameter_design$age_ratio) == 1L &
      n_distinct(parameter_design$annualisation_horizon) == 1L &
      n_distinct(parameter_design$discount_rate) == 1L &
      n_distinct(parameter_design$age_coverage) == 1L,
    nrow(structural_design) == 22L,
    sum(structural_design$is_primary) == 1L,
    unique(parameter_summary$VLW_estimable_n) == 188L,
    unique(parameter_summary$classification_complete_n) == 184L,
    nrow(structural_country) == 22L * 204L,
    n_distinct(structural_classification$ISO3) ==
      unique(parameter_summary$classification_complete_n),
    primary_fixed_check,
    nrow(structural_fixed_classification) ==
      22L * nrow(primary_reference) &
      all(
        structural_fixed_classification %>%
          count(scenario_id) %>%
          pull(n) == nrow(primary_reference)
      ),
    nrow(fixed_cutoff_classification) ==
      2L * 22L * nrow(primary_reference),
    nrow(US_scenario_comparison) == 22L
  )
) %>%
  mutate(status = if_else(pass, "PASS", "FAIL"))

write_scenario_csv(scenario_validation, "scenario_validation_report.csv")
if (any(!scenario_validation$pass)) {
  stop("One or more scenario-analysis validation checks failed.", call. = FALSE)
}

print(parameter_summary)
print(structural_summary)
print(coverage_audit)
print(scenario_validation)
