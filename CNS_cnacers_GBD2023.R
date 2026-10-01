# GBD 2023 analysis, validated on the prepared inputs supplied with this run.
# Run from the repository root. Inputs are read from data/ and outputs are
# written to results/.
# Study year: 2023; monetary price base: current 2023 PPP international dollars.
# US DOT 2023 VSL (13.2 million USD) is used as the peak-VSL model anchor.
# Never relabel old GBD estimates as a new release.
# ==========================================================
# 0. Packages
# ==========================================================

# tidyverse: Data organization and visualization
# countrycode: Conversion of country names to ISO3 country codes
# maps: World map data
# scales: Legend and color scale adjustment
library(tidyverse)
library(countrycode)
library(maps)
library(scales)

# ==========================================================
# Centralised output directory
# ==========================================================

results_dir <- "results"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
data_dir <- "data"

# Redirect CSV calls with a bare file name to results/. Calls that already
# contain a directory (for example, results/file.csv) are left unchanged.
write.csv <- function(x, file = "", ...) {
  if (!nzchar(file)) {
    stop("An output file name is required.", call. = FALSE)
  }

  output_file <- if (dirname(file) == ".") {
    file.path(results_dir, file)
  } else {
    file
  }

  utils::write.csv(x, file = output_file, ...)
}

# In non-interactive runs, printed plots would otherwise create Rplots.pdf in
# the data directory. Keep that device output inside results/ as well.
if (!interactive()) {
  grDevices::pdf(file.path(results_dir, "Rplots.pdf"))
}

# Save a publication-resolution PNG alongside a PDF output. The original
# analysis called this helper for VLW figures but did not define it.
save_manuscript_png <- function(pdf_file, plot, width, height, dpi = 300) {
  png_file <- sub("\\.pdf$", ".png", pdf_file, ignore.case = TRUE)

  if (identical(png_file, pdf_file)) {
    png_file <- paste0(pdf_file, ".png")
  }

  ggsave(
    filename = png_file,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    bg = "white"
  )

  invisible(png_file)
}

# ==========================================================
# 1. Read data
# ==========================================================

# SDI-level ASR data
ASR_SDI <- read.csv(file.path(data_dir, "SDI_data.csv"))

# Country-level ASR data
ASR_country <- read.csv(file.path(data_dir, "Country_data.csv"))

# SDI-level number data
Number_SDI <- read.csv(file.path(data_dir, "SDI_Number.csv"))

# Country-level number data
Number_country <- read.csv(file.path(data_dir, "Country_Number.csv"))

# Country-level disease burden data
# Used for the analysis of health inequality
Country_rate <- read.csv(file.path(data_dir, "Country_disease_burden.csv"))

# Country population data
population <- read.csv(file.path(data_dir, "Population.csv"))

# Four major measures of disease burden:
# 1. Incidence
# 2. Prevalence
# 3. Deaths
# 4. DALYs
measures <- c(
  "Incidence",
  "Prevalence",
  "Deaths",
  "DALYs (Disability-Adjusted Life Years)"
)


# Fail on ambiguous keys instead of silently deduplicating observations.
assert_unique <- function(d, keys, label) {
  if (!all(keys %in% names(d)) || anyNA(d[keys]) || anyDuplicated(d[keys]))
    stop(label, ": missing or duplicated key (", paste(keys, collapse=", "), ")", call.=FALSE)
  invisible(d)
}
check_estimates <- function(d, label) {
  if (any(!is.finite(as.matrix(d[c("val", "lower", "upper")])) |
          d$lower < 0 | d$lower > d$val | d$val > d$upper))
    stop(label, ": invalid estimate or bounds", call.=FALSE)
}
validate_series <- function(d, age, metric, n_locations, label) {
  d <- d %>% filter(sex_id == 3)
  if (!nrow(d) || !all(d$age_name == age) || !all(d$metric_name == metric) ||
      n_distinct(d$location_id) != n_locations ||
      !setequal(unique(d$measure_name), measures)) stop(label, ": wrong dimensions")
  assert_unique(d, c("location_id", "year", "measure_name"), label)
  check_estimates(d, label)
  ok <- d %>% group_by(location_id, measure_name) %>%
    summarise(ok=setequal(year, 1990:2023), .groups="drop")
  if (!all(ok$ok) || nrow(ok) != n_locations * length(measures))
    stop(label, ": incomplete annual series 1990-2023")
}
validate_series(ASR_SDI, "Age-standardized", "Rate", 6L, "SDI ASR")
validate_series(Number_SDI, "All ages", "Number", 6L, "SDI counts")
validate_series(ASR_country, "Age-standardized", "Rate", 204L, "Country ASR")
validate_series(Number_country, "All ages", "Number", 204L, "Country counts")
if (!setequal(ASR_SDI$location_name, c("Global", "High SDI", "High-middle SDI",
    "Middle SDI", "Low-middle SDI", "Low SDI"))) stop("Unexpected SDI groups")
if (!setequal(ASR_country$location_id, Number_country$location_id)) stop("Country ID sets differ")

# Split datasets

# The dataset containing multiple disease burden measures is divided according to 
# Incidence / Prevalence / Deaths / DALYs, facilitating subsequent calculations 
# and plotting separately.
split_measure <- function(data){
  
  lapply(
    measures,
    function(x){
      subset(data, measure_name == x)
    }
  ) |> 
    setNames(
      c(
        "Incidence",
        "Prevalence",
        "Deaths",
        "DALYs"
      )
    )
}

# Split ASR datasets
ASR_SDI_list <- split_measure(ASR_SDI)
ASR_country_list <- split_measure(ASR_country)

# Split number datasets
Number_SDI_list <- split_measure(Number_SDI)
Number_country_list <- split_measure(Number_country)

# Format ASR / Number

# Format GBD estimates
format_GBD <- function(dat, target_year, type="ASR"){
  
  out <- dat %>%
    filter(
      year == target_year,
      sex_id == 3 # bilateral gender (men and women)
    ) %>%
    select(
      location_name,
      val,
      upper,
      lower
    )
  
  # Keep two decimals
  out <- out %>%
    mutate(
      val = round(val,2),
      upper = round(upper,2),
      lower = round(lower,2)
    )
  
  # Combine estimate and 95% UI into one character variable
  out %>%
    mutate(
      !!type :=
        paste0(
          val,
          " (",
          lower,
          "-",
          upper,
          ")"
        )
    ) %>%
    select(
      location_name,
      all_of(type)
    )
}

# Generate 1990 / 2023 tables
generate_year_table <- function(data_list,
                                year,
                                type){
  
  lapply(
    data_list,
    function(x){
      format_GBD(
        x,
        year,
        type
      )
    }
  )
}



# ASR in 1990
ASR_SDI_1990 <-
  generate_year_table(
    ASR_SDI_list,
    1990,
    "ASR"
  )

# ASR in 2023
ASR_SDI_2023 <-
  generate_year_table(
    ASR_SDI_list,
    2023,
    "ASR"
  )

# Country-level ASR in 1990
ASR_country_1990 <-
  generate_year_table(
    ASR_country_list,
    1990,
    "ASR"
  )

# Country-level ASR in 2023
ASR_country_2023 <-
  generate_year_table(
    ASR_country_list,
    2023,
    "ASR"
  )



# Country-level number of cases/deaths/DALYs in 1990
Number_country_1990 <-
  generate_year_table(
    Number_country_list,
    1990,
    "Number"
  )

# Country-level number of cases/deaths/DALYs in 2023
Number_country_2023 <-
  generate_year_table(
    Number_country_list,
    2023,
    "Number"
  )

# ==========================================================
# 2.EAPC (Estimated Annual Percentage Change)
# ==========================================================
#  Calculate EAPC

# EAPC was estimated by fitting a linear regression model:
# log(value) = β × year + constant
# EAPC = (exp(β)-1) ×100%
calc_EAPC <- function(dat) {
  
  # Keep both-sex estimates across all available years
  EAPC_data <- dat %>%
    filter(sex_id == 3, year >= 1990, year <= 2023) %>% # both sexes
    select(location_name, year, val)
  
  locations <- unique(EAPC_data$location_name)
  
  # Initialize result table
  EAPC_result <- data.frame(
    location_name = locations,
    EAPC = NA,
    LCI = NA,
    UCI = NA
  )
  
  # Calculate EAPC separately for each country/territory
  for(i in seq_along(locations)){
    
    tmp <- EAPC_data %>%
      filter(location_name == locations[i])
    
    assert_unique(tmp, c("location_name", "year"), "EAPC")
    if (!setequal(tmp$year, 1990:2023) || any(!is.finite(tmp$val) | tmp$val <= 0))
      stop("EAPC requires a complete series of positive ASRs")

    # Log transformation is required for the log-linear model
    tmp$log_val <- log(tmp$val)
    
    # Fit log-linear regression model
    model <- lm(log_val ~ year, data = tmp)
    
    # Extract regression coefficient and standard error
    beta <- summary(model)$coefficients[2,1]
    se   <- summary(model)$coefficients[2,2]
    
    # Point estimate
    EAPC_result$EAPC[i] <-
      (exp(beta)-1)*100
    
    # 95% confidence interval
    EAPC_result$LCI[i] <-
      (exp(beta-1.96*se)-1)*100
    
    EAPC_result$UCI[i] <-
      (exp(beta+1.96*se)-1)*100
    
  }
  
  # Round estimates and generate a formatted EAPC (95% CI) variable
  EAPC_result <- EAPC_result %>%
    mutate(
      # Keep full precision for export and map classification.
      EAPC_CI =
        sprintf("%.2f (%.2f-%.2f)", EAPC, LCI, UCI)
    )
  
  return(EAPC_result)
}


# Table 1: all six locations, four measures, endpoint estimates and EAPC.
# UI = GBD uncertainty interval; CI = regression confidence interval.
# Retain the original normal-approximation CI beta +/- 1.96 * SE for comparability.
Table1_numeric <- bind_rows(lapply(measures, function(m) {
  a <- ASR_SDI %>% filter(sex_id == 3, measure_name == m)
  n <- Number_SDI %>% filter(sex_id == 3, measure_name == m)
  e <- calc_EAPC(a)
  endpoint <- function(d, y, prefix) {
    out <- d %>% filter(year == y) %>% select(location_name, val, lower, upper)
    names(out)[-1] <- paste0(prefix, "_", y, c("", "_lower", "_upper"))
    out
  }
  out <- Reduce(function(x,y) left_join(x,y,by="location_name",relationship="one-to-one"),
    list(endpoint(a,1990,"ASR"), endpoint(n,1990,"Number"),
         endpoint(a,2023,"ASR"), endpoint(n,2023,"Number"),e))
  out$measure_name <- m
  out
}))
stopifnot(nrow(Table1_numeric) == 24L, !anyNA(Table1_numeric))
write.csv(Table1_numeric, "Table1_numeric.csv", row.names=FALSE)
fmt_ui <- function(v,l,u) sprintf("%.1f (%.1f-%.1f)",v,l,u)
Table1 <- Table1_numeric %>% transmute(measure_name, location_name,
  ASR_1990=fmt_ui(ASR_1990,ASR_1990_lower,ASR_1990_upper),
  Number_1990=fmt_ui(Number_1990,Number_1990_lower,Number_1990_upper),
  ASR_2023=fmt_ui(ASR_2023,ASR_2023_lower,ASR_2023_upper),
  Number_2023=fmt_ui(Number_2023,Number_2023_lower,Number_2023_upper),
  EAPC_95CI=sprintf("%.2f (%.2f-%.2f)",EAPC,LCI,UCI))
write.csv(Table1, "Table1_formatted.csv", row.names=FALSE)

#  Calculate EAPC for different indicators
# Incidence
Incidence_EAPC_country <-
  calc_EAPC(
    ASR_country_list$Incidence
  )

# Prevalence
Prevalence_EAPC_country <-
  calc_EAPC(
    ASR_country_list$Prevalence
  )

# Deaths
Deaths_EAPC_country <-
  calc_EAPC(
    ASR_country_list$Deaths
  )

# DALYs
DALYs_EAPC_country <-
  calc_EAPC(
    ASR_country_list$DALYs
  )

#  Convert country names into ISO3 codes

# ISO3 codes are used to link country-level estimates with the world map polygons.
add_ISO3 <- function(data){
  
  data$ISO3 <- countrycode(
    data$location_name,
    origin="country.name",
    destination="iso3c"
  )
  
  return(data)
}


Incidence_EAPC_country <-
  add_ISO3(Incidence_EAPC_country)

Prevalence_EAPC_country <-
  add_ISO3(Prevalence_EAPC_country)

Deaths_EAPC_country <-
  add_ISO3(Deaths_EAPC_country)

DALYs_EAPC_country <-
  add_ISO3(DALYs_EAPC_country)

# Prepare world map data
# map_data("world") provides geographic polygon data for countries and territories
map_region_to_iso3 <- function(region) {
  countrycode(
    region,
    origin = "country.name",
    destination = "iso3c",
    custom_match = c(
      "Kosovo" = "XKX",
      "Micronesia" = "FSM",
      "Virgin Islands" = "VIR",
      "Saint Martin" = "MAF",
      "Saba" = "BES",
      "Sint Eustatius" = "BES",
      "Bonaire" = "BES",
      "Canary Islands" = NA,
      "Azores" = NA,
      "Madeira Islands" = NA,
      "Ascension Island" = NA,
      "Heard Island" = NA,
      "Chagos Archipelago" = NA,
      "Siachen Glacier" = NA,
      "Barbuda" = NA,
      "Grenadines" = "VCT"
    )
  )
}

worldData <- map_data("world")
worldData$ISO3 <- map_region_to_iso3(worldData$region)

#  Mapping function
plot_EAPC_map <- function(data, title){
  
  # Visualise the geographic distribution of country-level EAPC estimates
  map <- worldData %>%
    left_join(data, by = "ISO3") %>%
    
    # Categorise EAPC values
    mutate(
      EAPC_group = cut(
        EAPC,
        breaks = c(-Inf, 0, 2, 4, 6, Inf),
        labels = c(
          "<0",
          "0-2",
          "2-4",
          "4-6",
          ">=6"
        ),
        include.lowest = TRUE,
        right = FALSE
      )
    )
  
  # Generate world map
  ggplot(map,
         aes(long, lat,
             group = group,
             fill = EAPC_group)) +
    
    geom_polygon(
      colour = "grey30",
      linewidth = 0.15
    ) +
    
    # Define colour scale for EAPC categories
    scale_fill_manual(
      values = c(
        "<0"  = "#80ACF9",
        "0-2" = "#CFE8FF",
        "2-4" = "#FEE6A8",
        "4-6" = "#F4A261",
        ">=6" = "#D73027"
      ),
      na.value = "grey90",
      drop = FALSE,
      name = "EAPC"
    ) +
    
    theme_void() +
    labs(title = title)
}

# Generate EAPC maps for four disease burden measures
# Incidence
p_incidence <-
  plot_EAPC_map(
    Incidence_EAPC_country,
    "Incidence"
  )

# Prevalence
p_prevalence <-
  plot_EAPC_map(
    Prevalence_EAPC_country,
    "Prevalence"
  )

# Deaths
p_deaths <-
  plot_EAPC_map(
    Deaths_EAPC_country,
    "Deaths"
  )

# DALYs
p_DALYs <-
  plot_EAPC_map(
    DALYs_EAPC_country,
    "DALYs"
  )

# Save results
ggsave(
  "results/Incidence_EAPC.pdf",
  p_incidence,
  width=10,
  height=6
)


ggsave(
  "results/Prevalence_EAPC.pdf",
  p_prevalence,
  width=10,
  height=6
)


ggsave(
  "results/Deaths_EAPC.pdf",
  p_deaths,
  width=10,
  height=6
)


ggsave(
  "results/DALYs_EAPC.pdf",
  p_DALYs,
  width=10,
  height=6
)

# One reproducible composite for manuscript Figure 1.
if (!requireNamespace("patchwork", quietly=TRUE)) stop("Install patchwork for Figure 1")
Figure1 <- patchwork::wrap_plots(p_incidence, p_prevalence, p_deaths, p_DALYs,
  ncol=2, guides="collect") + patchwork::plot_annotation(tag_levels="A")
ggsave("results/Figure1_EAPC.png", Figure1, width=13, height=8, dpi=300)
ggsave("results/Figure1_EAPC.pdf", Figure1, width=13, height=8)

# Export EAPC results
write.csv(
  Incidence_EAPC_country,
  "results/Incidence_EAPC.csv"
)

write.csv(
  Prevalence_EAPC_country,
  "results/Prevalence_EAPC.csv"
)

write.csv(
  Deaths_EAPC_country,
  "results/Deaths_EAPC.csv"
)

write.csv(
  DALYs_EAPC_country,
  "results/DALYs_EAPC.csv"
)

# ==========================================================
# 3. Health inequality
# ==========================================================

# Prepare data for health inequality analysis
# Select the two study years: 1990 and 2023
data_hi <- subset(
  Country_rate,
  year %in% c("1990", "2023") &
    indicator_abbreviation %in% c(
      "CNS_ASIR", # CNS_ASIR = age-standardised incidence rate
      "CNS_ASPR", # CNS_ASPR = age-standardised prevalence rate
      "CNS_ASMR", # CNS_ASMR = age-standardised mortality rate
      "CNS_ASDR" # CNS_ASDR = age-standardised DALY rate
    )
)


# Validate before and after the many-to-one population join.
assert_unique(data_hi, c("location_id", "year", "indicator_abbreviation"), "Inequality rates")
check_estimates(data_hi, "Inequality rates")
population_hi <- population %>% filter(year %in% c(1990,2023), sex_id == 3,
  age_name == "All ages", metric_name == "Number")
assert_unique(population_hi, c("location_id","year"), "Population")
expected_ids <- sort(unique(ASR_country$location_id))
coverage <- data_hi %>% group_by(year, indicator_abbreviation) %>%
  summarise(n_rows=n(), n_locations=n_distinct(location_id),
    exact_country_set=setequal(location_id, expected_ids), .groups="drop")
write.csv(coverage,"inequality_coverage.csv",row.names=FALSE)
if (nrow(coverage) != 8L || any(coverage$n_rows != 204L | !coverage$exact_country_set))
  stop("Inequality data do not cover the same 204 countries for all 8 panels")
missing_pop <- anti_join(data_hi, population_hi, by=c("location_id","year"))
if (nrow(missing_pop)) stop("Unmatched inequality population keys")
n_before <- nrow(data_hi)
data_hi <- data_hi %>% left_join(
  population_hi %>% transmute(location_id, year, population=val),
  by=c("location_id","year"), relationship="many-to-one")
assert_unique(data_hi,c("location_id","year","indicator_abbreviation"),"Joined inequality")
stopifnot(nrow(data_hi)==n_before, all(is.finite(data_hi$population)), all(data_hi$population>0))
write.csv(data.frame(rows_before=n_before,rows_after=nrow(data_hi),unmatched=0L),
  "inequality_join_audit.csv",row.names=FALSE)

# Calculate population-weighted fractional rank
df_hi <- data_hi %>%
  select(
    year,
    indicator_abbreviation,
    sdi_val,
    val,
    lower,
    upper,
    population,
    location_name,
    location_id
  ) %>%
  group_by(
    year,
    indicator_abbreviation
  ) %>%
  arrange(
    sdi_val,
    location_id, # deterministic tie order
    .by_group = TRUE
  ) %>%
  mutate(
    
    # Population share of each country
    pop_share = population /
      sum(population, na.rm = TRUE),
    
    # Cumulative population share
    cum_pop = cumsum(pop_share),
    
    # Cumulative population share before the current country
    lag_cum_pop = lag(
      cum_pop,
      default = 0
    ),
    
    # Population-weighted fractional rank (ridit)
    ridit = lag_cum_pop +
      pop_share / 2,
    
    # Approximate standard error derived from the reported 95% uncertainty interval
    se = (upper - lower) / (2 * 1.96)
    
  ) %>%
  ungroup()

# Reproducible Monte Carlo settings. Separate seeds are used so that the
# CIX and SII simulations remain reproducible independently of one another.
n_mc_sim <- 5000L
cix_mc_seed <- 20210826L
sii_mc_seed <- 20210827L

# The simulation requires complete, ordered uncertainty bounds and positive
# population denominators. Stop instead of silently dropping invalid rows.
invalid_hi_rows <- df_hi %>%
  filter(
    !is.finite(sdi_val) |
      !is.finite(val) |
      !is.finite(lower) |
      !is.finite(upper) |
      !is.finite(se) |
      !is.finite(population) |
      lower > upper |
      se < 0 |
      population <= 0
  )

if (nrow(invalid_hi_rows) > 0) {
  stop(
    "Health-inequality input contains invalid estimates, uncertainty bounds, or populations.",
    call. = FALSE
  )
}

write.csv(
  df_hi,
  "health_inequality_analysis_data.csv",
  row.names = FALSE
)

# Monte Carlo simulation for Concentration Index (CIX)
calc_cix_cov <- function(dat) {
  
  dat <- dat %>%
    arrange(sdi_val) %>%
    mutate(
      
      # Population share
      pop_share = population /
        sum(population, na.rm = TRUE),
      
      # Population-weighted fractional rank
      rank = lag(
        cumsum(pop_share),
        default = 0
      ) +
        0.5 * pop_share
    )
  
  
  # Population-weighted mean
  mu <- sum(
    dat$sim_val * dat$pop_share,
    na.rm = TRUE
  )
  
  
  # Population-weighted covariance
  cov_y_r <- sum(
    dat$pop_share *
      (dat$sim_val - mu) *
      (dat$rank - 0.5),
    na.rm = TRUE
  )
  
  
  # Concentration index
  cix <- 2 * cov_y_r / mu
  
  return(cix)
}


mc_cix <- function(data, n_sim = 5000) {
  
  cix_vec <- numeric(n_sim)
  
  
  for (i in seq_len(n_sim)) {
    
    sim_data <- data
    
    # Simulate outcome values based on the reported
    # estimate and its standard error
    sim_data$sim_val <- rnorm(
      nrow(sim_data),
      mean = sim_data$val,
      sd = sim_data$se
    )
    
    
    # Avoid negative simulated estimates
    sim_data$sim_val[
      sim_data$sim_val < 0
    ] <- 0
    
    
    # Calculate CIX
    cix_vec[i] <- calc_cix_cov(sim_data)
  }
  
  
  # CIX based on the original estimates
  data$sim_val <- data$val
  
  data.frame(
    CIX = calc_cix_cov(data),
    
    MC_lower_95 = quantile(
      cix_vec,
      0.025,
      na.rm = TRUE
    ),
    
    MC_upper_95 = quantile(
      cix_vec,
      0.975,
      na.rm = TRUE
    )
  )
}


# Calculate CIX for all outcomes and years
set.seed(cix_mc_seed)

cix_results <- df_hi %>%
  group_by(
    year,
    indicator_abbreviation
  ) %>%
  group_modify(
    ~ mc_cix(
      .x,
      n_sim = n_mc_sim
    )
  ) %>%
  ungroup() %>%
  mutate(
    n_sim = n_mc_sim,
    mc_seed = cix_mc_seed,
    interval_method = paste0(
      "Approximate 95% Monte Carlo interval: independent Normal draws; ",
      "SE=(upper-lower)/(2*1.96); negative draws truncated at zero"
    )
  )


cix_results

write.csv(
  cix_results,
  "CIX_results.csv",
  row.names = FALSE
)

# Monte Carlo simulation for Slope Index of Inequality (SII)
calc_sii_mc <- function(data, n_sim = 5000) {
  
  sii_vec <- numeric(n_sim)
  
  
  for (i in seq_len(n_sim)) {
    
    sim_data <- data
    
    # Simulate outcome values
    sim_data$sim_val <- rnorm(
      nrow(sim_data),
      mean = sim_data$val,
      sd = sim_data$se
    )
    
    
    # Avoid negative simulated estimates
    sim_data$sim_val[
      sim_data$sim_val < 0
    ] <- 0
    
    
    # Population-weighted linear regression
    model <- lm(
      sim_val ~ ridit,
      data = sim_data,
      weights = population
    )
    
    
    # Store SII estimate
    sii_vec[i] <- coef(model)["ridit"]
  }
  
  
  # SII based on the original estimates
  original_model <- lm(
    val ~ ridit,
    data = data,
    weights = population
  )
  
  
  original_sii <- coef(
    original_model
  )["ridit"]
  
  
  data.frame(
    SII = original_sii,
    
    MC_lower_95 = quantile(
      sii_vec,
      0.025,
      na.rm = TRUE
    ),
    
    MC_upper_95 = quantile(
      sii_vec,
      0.975,
      na.rm = TRUE
    )
  )
}


# Calculate SII for all outcomes and years
set.seed(sii_mc_seed)

sii_mc <- df_hi %>%
  group_by(
    year,
    indicator_abbreviation
  ) %>%
  group_modify(
    ~ calc_sii_mc(
      .x,
      n_sim = n_mc_sim
    )
  ) %>%
  ungroup() %>%
  mutate(
    n_sim = n_mc_sim,
    mc_seed = sii_mc_seed,
    interval_method = paste0(
      "Approximate 95% Monte Carlo interval: independent Normal draws; ",
      "SE=(upper-lower)/(2*1.96); negative draws truncated at zero"
    )
  )


sii_mc

write.csv(
  sii_mc,
  "SII_results.csv",
  row.names = FALSE
)


# Functions for SII plots
# Outcome labels
outcome_labels <- c(
  CNS_ASIR = "Incidence per 100,000 population",
  CNS_ASPR = "Prevalence per 100,000 population",
  CNS_ASMR = "Deaths per 100,000 population",
  CNS_ASDR = "DALYs per 100,000 population"
)


# File names
sii_file_names <- c(
  CNS_ASIR = "SII_Incidence.pdf",
  CNS_ASPR = "SII_Prevalence.pdf",
  CNS_ASMR = "SII_Deaths.pdf",
  CNS_ASDR = "SII_DALYs.pdf"
)


# Function for calculating regression intercepts
get_sii_intercepts <- function(data) {
  
  data %>%
    group_by(year) %>%
    group_modify(~ {
      
      fit <- lm(
        val ~ ridit,
        data = .x,
        weights = population
      )
      
      data.frame(
        y0 = predict(
          fit,
          newdata = data.frame(
            ridit = 0
          )
        ),
        
        y1 = predict(
          fit,
          newdata = data.frame(
            ridit = 1
          )
        )
      )
    }) %>%
    ungroup()
}


# Function for preparing SII labels
get_sii_labels <- function(
    sii_results,
    indicator,
    data_curve
) {
  
  labels <- sii_results %>%
    filter(
      indicator_abbreviation == indicator
    ) %>%
    mutate(
      
      label = paste0(
        year,
        "\nSII = ",
        round(SII, 2),
        " (",
        round(MC_lower_95, 2),
        ", ",
        round(MC_upper_95, 2),
        ")"
      ),
      
      x = 0.05
    )
  
  
  # Automatically position labels
  labels$y <- c(
    max(data_curve$val, na.rm = TRUE) * 0.95,
    max(data_curve$val, na.rm = TRUE) * 0.82
  )
  
  return(labels)
}


# Function for generating SII plot
plot_sii <- function(
    data,
    sii_results,
    indicator,
    y_label
) {
  
  # Select outcome
  df_curve <- data %>%
    filter(
      indicator_abbreviation == indicator
    )
  
  
  # Regression intercepts
  intercepts <- get_sii_intercepts(
    df_curve
  )
  
  
  # SII labels
  sii_label <- get_sii_labels(
    sii_results,
    indicator,
    df_curve
  )
  
  
  # Plot
  p <- ggplot(
    df_curve,
    aes(
      x = ridit,
      y = val,
      color = factor(year)
    )
  ) +
    
    # Country-level estimates
    geom_point(
      aes(
        size = population
      ),
      alpha = 0.35
    ) +
    
    # Population-weighted regression line.
    # IMPORTANT: the ribbon from se = TRUE is the pointwise 95% CI
    # for the fitted WLS mean; it is NOT the Monte Carlo interval for SII.
    geom_smooth(
      method = "lm",
      aes(
        weight = population,
        fill = factor(year)
      ),
      se = TRUE,
      linewidth = 0.8,
      alpha = 0.15
    ) +
    
    # SII labels
    geom_text(
      data = sii_label,
      aes(
        x = x,
        y = y,
        label = label
      ),
      inherit.aes = FALSE,
      color = "black",
      hjust = 0,
      size = 4
    ) +
    
    # Regression intercept line
    geom_segment(
      data = intercepts,
      aes(
        x = 0.02,
        xend = 0.98,
        y = y0,
        yend = y0,
        color = factor(year)
      ),
      linetype = "dashed",
      linewidth = 0.5,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    
    scale_x_continuous(
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1),
      expand = expansion(
        mult = c(0.05, 0.05)
      )
    ) +
    
    scale_color_manual(
      values = c(
        "1990" = "#F1B656",
        "2023" = "#397FC7"
      ),
      name = "Year"
    ) +
    
    scale_fill_manual(
      values = c(
        "1990" = "#F1B656",
        "2023" = "#397FC7"
      ),
      guide = "none"
    ) +
    
    scale_size_continuous(
      range = c(2, 10),
      name = "Population"
    ) +
    
    labs(
      x = "Relative rank by SDI",
      y = y_label
    ) +
    
    theme_classic(
      base_size = 12
    ) +
    
    theme(
      panel.grid.major = element_line(
        colour = "grey90",
        linewidth = 0.3
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      axis.text = element_text(
        colour = "black"
      ),
      
      axis.title = element_text(
        colour = "black"
      )
    )
  
  
  return(p)
}

# Generate and save SII plots
for (indicator in names(outcome_labels)) {
  
  plot_sii_current <- plot_sii(
    data = df_hi,
    sii_results = sii_mc,
    indicator = indicator,
    y_label = outcome_labels[indicator]
  )
  
  
  print(plot_sii_current)
  
  # Save map as PDF
  ggsave(
    filename = file.path(
      "results",
      unname(sii_file_names[indicator])
    ),
    plot = plot_sii_current,
    width = 8,
    height = 6,
    device = "pdf"
  )
}

# Prepare data for CIX curves
df_cix_curve <- df_hi %>%
  group_by(
    year,
    indicator_abbreviation
  ) %>%
  arrange(
    sdi_val,
    location_id, # deterministic tie order
    .by_group = TRUE
  ) %>%
  mutate(
    
    # Outcome weighted by population
    outcome_share = val * population,
    
    # Total outcome
    total_outcome = sum(
      outcome_share,
      na.rm = TRUE
    ),
    
    # Cumulative outcome share
    cum_outcome = cumsum(
      outcome_share
    ) / total_outcome
    
  ) %>%
  ungroup()

# Function for CIX plots
# File names
cix_file_names <- c(
  CNS_ASIR = "CIX_Incidence.pdf",
  CNS_ASPR = "CIX_Prevalence.pdf",
  CNS_ASMR = "CIX_Deaths.pdf",
  CNS_ASDR = "CIX_DALYs.pdf"
)


# Function for preparing CIX labels
get_cix_labels <- function(
    cix_results,
    indicator
) {
  
  cix_results %>%
    filter(
      indicator_abbreviation == indicator
    ) %>%
    mutate(
      
      x = 0.65,
      
      y = c(
        0.30,
        0.22
      ),
      
      label = sprintf(
        "%s: %.3f (%.3f, %.3f)",
        year,
        CIX,
        MC_lower_95,
        MC_upper_95
      )
    )
}


# Function for generating CIX plot
plot_cix <- function(
    data,
    cix_results,
    indicator
) {
  
  # Select country-level observations for the requested outcome
  df_points <- data %>%
    filter(
      indicator_abbreviation == indicator
    )
  
  
  # Explicitly add the theoretical concentration-curve endpoints
  # (0, 0) and (1, 1) for each study year.
  df_endpoints <- df_points %>%
    distinct(year) %>%
    tidyr::crossing(
      cum_pop = c(0, 1)
    ) %>%
    mutate(
      cum_outcome = cum_pop
    )
  
  
  # Line data include the endpoints; country points remain unchanged.
  df_line <- bind_rows(
    df_points %>%
      select(
        year,
        cum_pop,
        cum_outcome
      ),
    df_endpoints
  ) %>%
    distinct(
      year,
      cum_pop,
      cum_outcome,
      .keep_all = TRUE
    ) %>%
    arrange(
      year,
      cum_pop
    )
  
  
  # CIX labels
  ci_label <- get_cix_labels(
    cix_results,
    indicator
  )
  
  
  # Plot
  p <- ggplot() +
    
    # Concentration curve, explicitly anchored at (0,0) and (1,1)
    geom_line(
      data = df_line,
      aes(
        x = cum_pop,
        y = cum_outcome,
        color = factor(year)
      ),
      linewidth = 0.8
    ) +
    
    # Country-level observations only
    geom_point(
      data = df_points,
      aes(
        x = cum_pop,
        y = cum_outcome,
        color = factor(year),
        size = population
      ),
      alpha = 0.35
    ) +
    
    # Equality line
    geom_abline(
      intercept = 0,
      slope = 1,
      colour = "orange",
      linewidth = 0.7
    ) +
    
    # CIX labels
    geom_text(
      data = ci_label,
      aes(
        x = x,
        y = y,
        label = label
      ),
      inherit.aes = FALSE,
      colour = "black",
      hjust = 0
    ) +
    
    labs(
      x = "Cumulative population (ordered by SDI)",
      y = "Cumulative outcome share",
      colour = "Year",
      size = "Population"
    ) +
    
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      expand = c(0, 0),
      oob = scales::squish
    ) +
    
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      expand = c(0, 0),
      oob = scales::squish
    ) +
    
    scale_color_manual(
      values = c(
        "1990" = "#F1B656",
        "2023" = "#397FC7"
      )
    ) +
    
    theme_minimal() +
    
    theme(
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.5
      ),
      
      panel.grid.major = element_line(
        colour = "grey90",
        linewidth = 0.3
      ),
      
      panel.grid.minor = element_blank()
    )
  
  
  return(p)
}

# Generate and save CIX plots
for (indicator in names(cix_file_names)) {
  
  plot_cix_current <- plot_cix(
    data = df_cix_curve,
    cix_results = cix_results,
    indicator = indicator
  )
  
  
  print(plot_cix_current)
  
  # Save map as PDF
  ggsave(
    filename = file.path(
      "results",
      unname(cix_file_names[indicator])
    ),
    plot = plot_cix_current,
    width = 8,
    height = 6,
    device = "pdf"
  )
}

# ==========================================================
# 4. Welfare loss and health expenditure context
# ==========================================================

# Estimate the welfare loss attributable to
# brain and central nervous system (CNS) cancers in 2023.

# Data input
# GDP per capita in 2023 current PPP international dollars
perGDP <- read.csv(
  file.path(data_dir, "perGDP-2023PPP.csv"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
) 

# Total GDP in 2023 current PPP international dollars
totalGDP <- read.csv(
  file.path(data_dir, "totalGDP-2023PPP.csv"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
) 

# Current health expenditure as a percentage of GDP
HE_GDP <- read.csv(
  file.path(data_dir, "HE-of-GDP.csv"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
) 

# Enforce economic observation year and price concept; no older-year fallback.
validate_economic_input <- function(x, indicator, label) {
  required <- c("Country Code", "Indicator Code", "2023")
  if (!all(required %in% names(x))) stop(label, ": missing metadata or 2023 column")
  if (any(is.na(x[["Indicator Code"]])) ||
      any(x[["Indicator Code"]] != indicator)) stop(label, ": wrong indicator / monetary basis")
  assert_unique(x, "Country Code", label)
  values <- suppressWarnings(as.numeric(x[["2023"]]))
  if (any(!is.na(x[["2023"]]) & is.na(values))) stop(label, ": nonnumeric 2023 values")
  if (any(!is.na(values) & (!is.finite(values) | values <= 0))) stop(label, ": invalid 2023 values")
}
validate_economic_input(perGDP, "NY.GDP.PCAP.PP.CD", "GDP per capita 2023")
validate_economic_input(totalGDP, "NY.GDP.MKTP.PP.CD", "GDP total 2023")
validate_economic_input(HE_GDP, "SH.XPD.CHEX.GD.ZS", "Health expenditure 2023")

# Age-specific remaining life expectancy in 2023
# Source: UN World Population Prospects 2024 combined-sex single-age mortality
# rates. Period life tables were constructed from mx; reconstructed e0 differs
# from the published WPP e0 by at most 0.008 years across all WPP locations.
LE_age <- read.csv(
  file.path(data_dir, "WPP2024_remaining_life_expectancy_2023.csv"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
) 

# Country-, age-, and sex-specific DALYs attributable to brain and CNS cancers in 2023
DALYs_2023 <- read.csv(
  file.path(data_dir, "DALYs-Number-Both-20-age.csv"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  encoding = "UTF-8"
) 

# Keep the 204 country/territory observations before ISO3 conversion so the
# global and SDI aggregate labels are not treated as unmatched countries.
DALYs_2023 <- DALYs_2023 %>%
  filter(
    year == 2023,
    sex_id == 3,
    !location_id %in% c(1L, 44634L, 44635L, 44636L, 44637L, 44639L)
  )

# Harmonise country names
# Create ISO3 codes
perGDP$ISO3 <- perGDP$`Country Code`

totalGDP$ISO3 <- totalGDP$`Country Code`

HE_GDP$ISO3 <- HE_GDP$`Country Code`

# Recode country names in GBD DALYs data
# The supplied Turkey label contains invalid UTF-8 bytes under some locales.
# Restrict the repair to a label beginning with T and ending with kiye.
turkey_label_rows <- grepl(
  "^T.*kiye$",
  DALYs_2023$location_name,
  useBytes = TRUE
)

if (sum(turkey_label_rows) == 0) {
  stop(
    "The Turkey label could not be identified in the DALY input.",
    call. = FALSE
  )
}

DALYs_2023$location_name[turkey_label_rows] <-
  "Turkey"

DALYs_2023$location_name <- DALYs_2023$location_name %>%
  recode(
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
  )


# Convert country names to ISO3 codes
DALYs_2023$ISO3 <- countrycode(
  DALYs_2023$location_name,
  origin = "country.name",
  destination = "iso3c",
  custom_match = c("Micronesia" = "FSM")
)


# Check unmatched countries
unmatched_DALY_locations <- DALYs_2023 %>%
  filter(is.na(ISO3)) %>%
  distinct(location_name)

if (nrow(unmatched_DALY_locations) > 0) {
  stop(
    "Unmatched GBD locations: ",
    paste(
      unmatched_DALY_locations$location_name,
      collapse = ", "
    ),
    call. = FALSE
  )
}

# Validate macroeconomic join keys before selecting columns
assert_unique(perGDP, "ISO3", "GDP per capita")
assert_unique(totalGDP, "ISO3", "Total GDP")
assert_unique(HE_GDP, "ISO3", "Health expenditure")

# Keep required variables
perGDP <- perGDP %>%
  transmute(
    ISO3 = ISO3,
    perGDP = `2023`
  )

totalGDP <- totalGDP %>%
  transmute(
    ISO3 = ISO3,
    totalGDP = `2023`
  )

HE_GDP <- HE_GDP %>%
  transmute(
    ISO3 = ISO3,
    HE_GDP = `2023`
  )

# Keep 2023 DALYs
DALYs_2023 <- DALYs_2023 %>%
  filter(
    year == 2023,
    sex_id == 3,
    !location_id %in% c(1L, 44634L, 44635L, 44636L, 44637L, 44639L)
  ) %>%
  transmute(
    ISO3 = ISO3,
    age_name = age_name,
    sex_id = sex_id,
    age = case_when(
      age_name == "<5 years"    ~ 3,
      age_name == "5-9 years"   ~ 7,
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
      age_name == "95+ years"   ~ 97,
      TRUE ~ NA_real_
    ),
    DALYs = val,
    upper = upper,
    lower = lower
  )

expected_vlw_age_rows <- n_distinct(DALYs_2023$age_name)
if (
  expected_vlw_age_rows != 20L ||
  n_distinct(DALYs_2023$ISO3) != 204L ||
  nrow(DALYs_2023) != 204L * 20L
) {
  stop("Expected 204 countries with 20 GBD-provided VLW age rows each.")
}

# Keep and validate the 2023 age-specific life table
LE_age <- LE_age %>%
  filter(
    year == 2023
  ) %>%
  select(
    ISO3,
    age,
    life_expectancy_at_birth,
    remaining_life_expectancy
  )

if (
  anyDuplicated(LE_age[c("ISO3", "age")]) > 0 ||
  any(!is.finite(LE_age$life_expectancy_at_birth)) ||
  any(!is.finite(LE_age$remaining_life_expectancy)) ||
  any(LE_age$remaining_life_expectancy <= 0)
) {
  stop(
    "The age-specific life-expectancy input contains duplicate keys or invalid values.",
    call. = FALSE
  )
}

if (
  any(is.na(DALYs_2023$age)) ||
  anyDuplicated(DALYs_2023[c("ISO3", "age")]) > 0
) {
  stop(
    "The 2023 DALY input contains unmapped or duplicate ISO3-age rows.",
    call. = FALSE
  )
}

missing_life_table_rows <- DALYs_2023 %>%
  distinct(ISO3, age) %>%
  anti_join(
    LE_age %>% distinct(ISO3, age),
    by = c("ISO3", "age")
  )

if (nrow(missing_life_table_rows) > 0) {
  stop(
    "Missing age-specific remaining life expectancy for one or more GBD ISO3-age rows.",
    call. = FALSE
  )
}

# Merge datasets
VLW_2023 <- DALYs_2023 %>%
  left_join(
    LE_age,
    by = c("ISO3", "age")
  ) %>%
  left_join(
    perGDP,
    by = "ISO3"
  ) %>%
  left_join(
    totalGDP,
    by = "ISO3"
  ) %>%
  left_join(
    HE_GDP,
    by = "ISO3"
  )

# Check missing key variables
VLW_missingness <- VLW_2023 %>%
  summarise(
    n = n(),
    missing_life_expectancy_at_birth = sum(
      is.na(.data$life_expectancy_at_birth)
    ),
    missing_remaining_life_expectancy = sum(
      is.na(.data$remaining_life_expectancy)
    ),
    missing_perGDP = sum(is.na(.data$perGDP)),
    missing_totalGDP = sum(is.na(.data$totalGDP)),
    missing_HE_GDP = sum(is.na(.data$HE_GDP))
  )

VLW_missingness

if (
  VLW_missingness$missing_life_expectancy_at_birth > 0 ||
  VLW_missingness$missing_remaining_life_expectancy > 0
) {
  stop(
    "Age-specific life expectancy did not merge completely to the DALY input.",
    call. = FALSE
  )
}

write.csv(
  VLW_2023,
  "VLW_age_specific_analysis_data.csv",
  row.names = FALSE
)

# Function for calculating value of lost welfare (VLW)
calculate_VLW <- function(
    data,
    epsilon = 1.0, # income elasticity parameter used to adjust VSL according to differences in GDP per capita
    VSL_peak_USA = 13200000, # reference peak VSL for the United States
    GDP_USA_2023, # US GDP per capita in 2023 current PPP international dollars
    discount_rate = 0.03 # annual discount rate used in the VSLY calculation
) {
  
  if (
    !is.numeric(discount_rate) ||
    length(discount_rate) != 1 ||
    !is.finite(discount_rate) ||
    discount_rate < 0
  ) {
    stop(
      "discount_rate must be one finite, non-negative number.",
      call. = FALSE
    )
  }
  
  # Income-adjusted peak VSL
  data <- data %>%
    mutate(
      VSL_peak =
        VSL_peak_USA *
        (perGDP / GDP_USA_2023)^epsilon # epsilon controls the degree to which VSL varies according to national income.
    )
  
  # Age-specific adjustment factor
  data <- data %>%
    mutate(
      # Conditional expected age at death for a person alive at this age.
      # Corlew supplementary equations 5-6 integrate from attained age a
      # to an age-specific life-expectancy upper age. Both limits must be
      # on the age scale, so the upper age is operationalised as a + e_a.
      # Full derivation and implementation details are reported in the
      # Supplementary Methods.
      expected_age_at_death =
        age + remaining_life_expectancy,
      
      # This keeps the Corlew age ratio within its intended [0, 1] domain,
      # including for people older than life expectancy at birth.
      ratio = age / expected_age_at_death
    )
  
  if (
    any(!is.finite(data$expected_age_at_death)) ||
    any(!is.finite(data$ratio)) ||
    any(data$ratio < 0 | data$ratio >= 1)
  ) {
    stop(
      "Age and remaining life expectancy produced an invalid age ratio.",
      call. = FALSE
    )
  }
  
  # Age adjustment function
  # The age-specific adjustment factor is calculated using the predefined polynomial function.
  
  # For ratio <= 0.236, the function is fixed at the value calculated at ratio = 0.236.
  fa <- 19.41 * (0.236^4) -
    43.17 * (0.236^3) +
    27.65 * (0.236^2) -
    4.33 * 0.236 +
    0.44
  
  data <- data %>%
    mutate(
      f_age = ifelse(
        ratio <= 0.236,
        fa,
        19.41 * ratio^4 -
          43.17 * ratio^3 +
          27.65 * ratio^2 -
          4.33 * ratio +
          0.44
      )
    )
  
  if (
    any(!is.finite(data$f_age)) ||
    any(data$f_age <= 0)
  ) {
    stop(
      "The age-adjustment function produced invalid values.",
      call. = FALSE
    )
  }
  
  # Age-specific VSL
  data <- data %>%
    mutate(
      VSL_age = VSL_peak * f_age
    )
  
  # Calculate Value of Statistical Life Year (VSLY)
  # VSLY represents the economic value assigned to one remaining life year at a given age.
  
  # Corlew supplementary equations 5-6 define VSL_age as the discounted
  # integral of a constant VSLY from attained age a to conditional
  # expected age at death L_a. Since L_a - a = e_a, the continuous
  # annuity factor is (1 - exp(-r * e_a)) / r. Its limit at r = 0 is e_a.
  data <- data %>%
    mutate(
      discounted_remaining_life_years = if (
        discount_rate == 0
      ) {
        remaining_life_expectancy
      } else {
        -expm1(
          -discount_rate * remaining_life_expectancy
        ) / discount_rate
      },
      
      VSLY = VSL_age /
        discounted_remaining_life_years
    )
  
  if (
    any(!is.finite(data$discounted_remaining_life_years)) ||
    any(data$discounted_remaining_life_years <= 0)
  ) {
    stop(
      "Discounted remaining life years must be finite and positive.",
      call. = FALSE
    )
  }
  
  # Algebraic identity check against the explicit Corlew integration
  # horizon. This guards against accidentally substituting total life
  # expectancy at birth for the age-specific remaining interval.
  corlew_horizon <-
    data$expected_age_at_death - data$age
  
  if (
    max(
      abs(
        corlew_horizon -
        data$remaining_life_expectancy
      ),
      na.rm = TRUE
    ) > 1e-10
  ) {
    stop(
      "The Corlew integration horizon is inconsistent with age-specific remaining life expectancy.",
      call. = FALSE
    )
  }
  
  # calculate value of lost welfare
  # VLW = VSLY × DALYs
  data <- data %>%
    mutate(
      VLW = VSLY * DALYs,
      VLW_upper = VSLY * upper,
      VLW_lower = VSLY * lower
    )
  
  return(data)
}

# Primary analysis
VSL_peak_USA <- 13200000

epsilon <- 1.0

GDP_USA_2023 <- VLW_2023 %>%
  filter(
    ISO3 == "USA"
  ) %>%
  pull(perGDP) %>%
  unique()

GDP_USA_2023

VLW_2023_primary <- calculate_VLW(
  data = VLW_2023,
  epsilon = epsilon,
  VSL_peak_USA = VSL_peak_USA,
  GDP_USA_2023 = GDP_USA_2023,
  discount_rate = 0.03
)

# Save a transparent United States age-level calculation chain.
VLW_US_age_bridge <- VLW_2023_primary %>%
  filter(
    ISO3 == "USA"
  ) %>%
  select(
    ISO3,
    age_name,
    age,
    DALYs,
    lower,
    upper,
    life_expectancy_at_birth,
    remaining_life_expectancy,
    expected_age_at_death,
    ratio,
    f_age,
    perGDP,
    totalGDP,
    VSL_peak,
    VSL_age,
    discounted_remaining_life_years,
    VSLY,
    VLW,
    VLW_lower,
    VLW_upper
  )

if (
  nrow(VLW_US_age_bridge) != expected_vlw_age_rows ||
  any(!is.finite(VLW_US_age_bridge$VLW))
) {
  stop(
    "The United States age-level VLW bridge is incomplete.",
    call. = FALSE
  )
}

write.csv(
  VLW_US_age_bridge,
 "VLW_US_age_bridge.csv",
  row.names = FALSE
)

# External benchmark bridge. Gerstl et al. reported US 2019 VLW of
# 35.58211 billion and VLW/GDP of 0.173244%, using a reference VSL of
# 9.979 million. The bridge quantifies how much of the difference is
# explained by the DALY year and reference VSL alone; it does not assume
# that the two studies' remaining implementation choices are equivalent.
gerstl_US_VLW_2019 <- 35.58211e9
gerstl_US_VLW_GDP_2019 <- 0.173244
gerstl_reference_VSL <- 9.979e6

US_DALYs_2019 <- Number_country %>%
  filter(
    location_name == "United States of America",
    measure_name ==
      "DALYs (Disability-Adjusted Life Years)",
    year == 2019,
    sex_id == 3
  ) %>%
  pull(val)

US_DALYs_2023 <- sum(
  VLW_US_age_bridge$DALYs
)

current_US_VLW_2023 <- sum(
  VLW_US_age_bridge$VLW
)

if (
  length(US_DALYs_2019) != 1 ||
  any(!is.finite(c(
    US_DALYs_2019,
    US_DALYs_2023,
    current_US_VLW_2023
  )))
) {
  stop(
    "The United States benchmark bridge inputs are missing or non-unique.",
    call. = FALSE
  )
}

US_DALY_year_factor <-
  US_DALYs_2023 / US_DALYs_2019

US_reference_VSL_factor <-
  VSL_peak_USA / gerstl_reference_VSL

US_explained_factor <-
  US_DALY_year_factor * US_reference_VSL_factor

US_observed_VLW_factor <-
  current_US_VLW_2023 / gerstl_US_VLW_2019

US_residual_unexplained_factor <-
  US_observed_VLW_factor / US_explained_factor

VLW_US_benchmark_bridge <- data.frame(
  published_study =
    "Gerstl et al., J Neurosurg, 2023",
  published_year = 2019,
  published_US_DALYs = US_DALYs_2019,
  current_year = 2023,
  current_US_DALYs = US_DALYs_2023,
  DALY_year_factor = US_DALY_year_factor,
  published_reference_VSL = gerstl_reference_VSL,
  current_reference_VSL = VSL_peak_USA,
  reference_VSL_factor = US_reference_VSL_factor,
  DALY_and_VSL_explained_factor = US_explained_factor,
  published_US_VLW = gerstl_US_VLW_2019,
  published_US_VLW_GDP_percent =
    gerstl_US_VLW_GDP_2019,
  expected_US_VLW_after_DALY_and_VSL_factors =
    gerstl_US_VLW_2019 * US_explained_factor,
  current_US_VLW = current_US_VLW_2023,
  current_US_VLW_GDP_percent =
    current_US_VLW_2023 /
    unique(VLW_US_age_bridge$totalGDP) * 100,
  observed_VLW_factor = US_observed_VLW_factor,
  residual_unexplained_factor =
    US_residual_unexplained_factor,
  interpretation = paste(
    "The DALY-year and reference-VSL factors do not explain",
    "the remaining implementation difference."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  VLW_US_benchmark_bridge,
   "VLW_US_benchmark_bridge.csv",
  row.names = FALSE
)

# Aggregate VLW to country level
summarise_VLW_country <- function(data) {
  
  sum_if_complete <- function(x) {
    if (any(is.na(x)) || any(!is.finite(x))) {
      return(NA_real_)
    }
    
    sum(x)
  }
  
  result <- data %>%
    group_by(
      ISO3
    ) %>%
    summarise(
      
      n_age_rows = n(),
      
      missing_perGDP = any(
        is.na(perGDP) | !is.finite(perGDP)
      ),
      
      missing_totalGDP = any(
        is.na(totalGDP) |
          !is.finite(totalGDP) |
          totalGDP <= 0
      ),
      
      missing_HE_GDP = any(
        is.na(HE_GDP) | !is.finite(HE_GDP)
      ),
      
      n_missing_VLW = sum(
        is.na(VLW) | !is.finite(VLW)
      ),
      
      VLW = sum_if_complete(VLW),
      
      # Marginal bounds cannot recover a joint 95% UI without joint draws.
      VLW_upper = sum_if_complete(VLW_upper),
      
      VLW_lower = sum_if_complete(VLW_lower),
      
      perGDP = first(
        perGDP
      ),
      
      totalGDP = first(
        totalGDP
      ),
      
      HE_GDP = first(
        HE_GDP
      ),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      
      VLW_available =
        !missing_perGDP &
        n_missing_VLW == 0 &
        is.finite(VLW),
      
      VLW_GDP_available =
        VLW_available &
        !missing_totalGDP,
      
      mismatch_eligible =
        VLW_GDP_available &
        !missing_HE_GDP,
      
      VLW_GDP = if_else(
        VLW_GDP_available,
        VLW / totalGDP * 100,
        NA_real_
      ),
      
      VLW_GDP_upper = if_else(
        VLW_GDP_available,
        VLW_upper / totalGDP * 100,
        NA_real_
      ),
      
      VLW_GDP_lower = if_else(
        VLW_GDP_available,
        VLW_lower / totalGDP * 100,
        NA_real_
      ),
      
      # Convert VLW to millions of 2023 current PPP-adjusted international dollars
      VLW = VLW / 1e6,
      
      VLW_upper = VLW_upper / 1e6,
      
      VLW_lower = VLW_lower / 1e6,
      
      analysis_status = case_when(
        mismatch_eligible ~
          "Included in mismatch analysis",
        VLW_available ~
          "VLW estimated; excluded from mismatch analysis",
        TRUE ~
          "VLW not estimable"
      ),
      
      exclusion_reason = paste0(
        if_else(
          missing_perGDP,
          "missing per-capita GDP; ",
          ""
        ),
        if_else(
          missing_totalGDP,
          "missing total GDP; ",
          ""
        ),
        if_else(
          missing_HE_GDP,
          "missing HE/GDP; ",
          ""
        )
      ),
      
      exclusion_reason = if_else(
        exclusion_reason == "",
        "None",
        sub(
          "; $",
          "",
          exclusion_reason
        )
      ),
      
      # Range from summed monetised age-specific DALY bounds; NOT a statistical 95% UI.
      VLW_bound_sum_range = if_else(
        VLW_available,
        sprintf(
          "%.2f (%.2f-%.2f)",
          VLW,
          VLW_lower,
          VLW_upper
        ),
        NA_character_
      )
    )
  
  if (
    nrow(result) != n_distinct(data$ISO3) ||
    any(result$n_age_rows != expected_vlw_age_rows) ||
    any(result$VLW_available & is.na(result$VLW)) ||
    any(!result$VLW_available & !is.na(result$VLW)) ||
    any(result$mismatch_eligible & is.na(result$VLW_GDP))
  ) {
    stop(
      "Country-level VLW aggregation failed its inclusion or missingness checks.",
      call. = FALSE
    )
  }
  
  result
}

VLW_country <- summarise_VLW_country(
  VLW_2023_primary
)

VLW_inclusion_flow <- VLW_country %>%
  select(
    ISO3,
    n_age_rows,
    missing_perGDP,
    missing_totalGDP,
    missing_HE_GDP,
    n_missing_VLW,
    VLW_available,
    VLW_GDP_available,
    mismatch_eligible,
    analysis_status,
    exclusion_reason
  )

VLW_inclusion_summary <- VLW_country %>%
  summarise(
    total_GBD_locations = n(),
    VLW_estimable = sum(VLW_available),
    VLW_not_estimable = sum(!VLW_available),
    VLW_GDP_estimable = sum(VLW_GDP_available),
    mismatch_eligible = sum(mismatch_eligible),
    VLW_estimated_but_not_mismatch = sum(
      VLW_available & !mismatch_eligible
    ),
    any_economic_input_missing = sum(
      missing_perGDP |
        missing_totalGDP |
        missing_HE_GDP
    ),
    missing_perGDP = sum(missing_perGDP),
    missing_totalGDP = sum(missing_totalGDP),
    missing_HE_GDP = sum(missing_HE_GDP)
  )

VLW_inclusion_summary

write.csv(
  VLW_inclusion_flow,
    "VLW_country_inclusion_flow.csv",
  row.names = FALSE
)

write.csv(
  VLW_inclusion_summary,
  
    "VLW_inclusion_summary.csv",
  row.names = FALSE
)

# Save VLW results
write.csv(
  VLW_country,
 "VLW_country.csv",
  row.names = FALSE
)

# Sensitivity analyses

# The income elasticity (epsilon) determines how strongly
# VSL varies with national income.

# Primary analysis: epsilon = 1.0
# Sensitivity analysis 1: epsilon = 1.5
# Sensitivity analysis 2: epsilon = 0.55

# All other parameters are kept unchanged from the primary analysis.

# IE = 1.5
VLW_2023_sens1 <- calculate_VLW(
  data = VLW_2023,
  epsilon = 1.5,
  VSL_peak_USA = VSL_peak_USA,
  GDP_USA_2023 = GDP_USA_2023,
  discount_rate = 0.03
)

VLW_country_sens1 <- summarise_VLW_country(
  VLW_2023_sens1
)

write.csv(
  VLW_country_sens1,
 "VLW_country_sens1.csv",
  row.names = FALSE
)

# IE = 0.55
VLW_2023_sens2 <- calculate_VLW(
  data = VLW_2023,
  epsilon = 0.55,
  VSL_peak_USA = VSL_peak_USA,
  GDP_USA_2023 = GDP_USA_2023,
  discount_rate = 0.03
)

VLW_country_sens2 <- summarise_VLW_country(
  VLW_2023_sens2
)

write.csv(
  VLW_country_sens2,
  "VLW_country_sens2.csv",
  row.names = FALSE
)

# Prepare world map

worldData <- map_data("world")
worldData$ISO3 <- map_region_to_iso3(worldData$region)

# Check unmatched regions
unique(
  worldData$region[
    is.na(worldData$ISO3)
  ]
)

# Prepare VLW map
VLW_map <- worldData %>%
  left_join(
    VLW_country,
    by = "ISO3"
  ) %>%
  mutate(
    ratio_val = VLW_GDP,
    HE_val = HE_GDP
  )

# Function for VLW map
plot_VLW_map <- function(
    data,
    variable,
    legend_title,
    output_file
) {
  
  # Determine scale limits
  map_values <- data[[variable]]
  
  scale_values <- c(
    min(
      map_values,
      na.rm = TRUE
    ),
    
    median(
      map_values,
      na.rm = TRUE
    ),
    
    max(
      map_values,
      na.rm = TRUE
    )
  )
  
  # Generate world map
  p <- ggplot(
    data,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = .data[[variable]]
    )
  ) +
    
    # Draw country polygons
    geom_polygon(
      colour = "grey30",
      linewidth = 0.2
    ) +
    
    # Define continuous colour gradient
    scale_fill_gradientn(
      colours = c(
        "#36A9E1",
        "white",
        "#EB5B25"
      ),
      
      values = scales::rescale(
        scale_values
      ),
      
      na.value = "grey90", # Countries without data are shown in light grey
      
      name = legend_title
    ) +
    
    theme_void() +
    
    labs(
      x = "",
      y = ""
    ) +
    
    guides(
      fill = guide_colorbar(
        title = legend_title
      )
    )
  
  # Save map as PDF
  ggsave(
    filename = output_file,
    plot = p,
    width = 10,
    height = 6,
    device = "pdf"
  )
  
  save_manuscript_png(
    output_file,
    p,
    10,
    6
  )
  
  
  return(p)
}

# Generate primary-analysis maps
# VLW/GDP map
p_VLW <- plot_VLW_map(
  data = VLW_map,
  variable = "ratio_val",
  legend_title = "VLW/GDP (%)",
  output_file = "results/VLW_GDP.pdf"
)

print(p_VLW)

# Healthcare expenditure as a percentage of GDP
p_HE <- plot_VLW_map(
  data = VLW_map,
  variable = "HE_val",
  legend_title = "HE/GDP (%)",
  output_file = "results/HE_GDP.pdf"
)

print(p_HE)


# ==========================================================
# 5. Parameter sensitivity analyses: IE = 1.5 and IE = 0.55
# ==========================================================

# IE = 1.5
# Merge sensitivity-analysis VLW estimates with world map
VLW_map_sens1 <- worldData %>%
  left_join(
    VLW_country_sens1,
    by = "ISO3"
  ) %>%
  mutate(
    ratio_val = VLW_GDP,
    HE_val = HE_GDP
  )

# VLW/GDP map under IE = 1.5
p_VLW_sens1 <- plot_VLW_map(
  data = VLW_map_sens1,
  variable = "ratio_val",
  legend_title = "VLW/GDP (%)",
  output_file = "results/VLW_GDP_sens1.pdf"
)

# HE/GDP map under IE = 1.5
p_HE_sens1 <- plot_VLW_map(
  data = VLW_map_sens1,
  variable = "HE_val",
  legend_title = "HE/GDP (%)",
  output_file = "results/HE_GDP_sens1.pdf"
)

# Merge sensitivity-analysis VLW estimates with world map
VLW_map_sens2 <- worldData %>%
  left_join(
    VLW_country_sens2,
    by = "ISO3"
  ) %>%
  mutate(
    ratio_val = VLW_GDP,
    HE_val = HE_GDP
  )

# VLW/GDP map under IE = 0.55
p_VLW_sens2 <- plot_VLW_map(
  data = VLW_map_sens2,
  variable = "ratio_val",
  legend_title = "VLW/GDP (%)",
  output_file = "results/VLW_GDP_sens2.pdf"
)

# HE/GDP map under IE = 0.55
p_HE_sens2 <- plot_VLW_map(
  data = VLW_map_sens2,
  variable = "HE_val",
  legend_title = "HE/GDP (%)",
  output_file = "results/HE_GDP_sens2.pdf"
)

# HE-VLW mismatch analysis
# Function for HE-VLW mismatch analysis
prepare_mismatch_data <- function(data) {
  
  # Apply the pre-specified complete-case rule recorded during aggregation.
  data <- data %>%
    filter(
      mismatch_eligible
    )
  
  if (
    nrow(data) == 0 ||
    any(!is.finite(data$HE_GDP)) ||
    any(!is.finite(data$VLW_GDP))
  ) {
    stop(
      "Mismatch analysis has no eligible complete cases or contains invalid values.",
      call. = FALSE
    )
  }
  
  # Calculate HE/GDP thresholds
  # The 25th and 75th percentiles of HE/GDP are used to define relatively low and high healthcare expenditure
  he_q <- quantile(
    data$HE_GDP,
    probs = c(
      0.25,
      0.75
    ),
    na.rm = TRUE
  )
  
  # Calculate VLW/GDP thresholds
  # The 25th and 75th percentiles of VLW/GDP define relatively low and high welfare loss.
  vlw_q <- quantile(
    data$VLW_GDP,
    probs = c(
      0.25,
      0.75
    ),
    na.rm = TRUE
  )
  
  # Classify HE/GDP and VLW/GDP
  data <- data %>%
    mutate(
      
      HE_flag = case_when(
        HE_GDP <= he_q[1] ~ "Low", # he_q[1] = 25th percentile
        HE_GDP >= he_q[2] ~ "High", # he_q[2] = 75th percentile
        TRUE ~ "Middle" # Middle = values between the two thresholds
      ),
      
      VLW_flag = case_when(
        VLW_GDP <= vlw_q[1] ~ "Low", # vlw_q[1] = 25th percentile
        VLW_GDP >= vlw_q[2] ~ "High", # vlw_q[2] = 75th percentile
        TRUE ~ "Middle" # Middle = values between the two thresholds
      ),
      
      # Identify mismatch categories
      group = case_when(
        HE_flag == "Low" &
          VLW_flag == "High" ~
          "Low HE / High VLW", # Relative discordance between overall health expenditure and welfare loss.
        
        HE_flag == "High" &
          VLW_flag == "Low" ~
          "High HE / Low VLW", # Reverse relative discordance between the two indicators.
        
        TRUE ~
          "Other combinations"
      )
    )
  
  
  return(
    list(
      data = data,
      he_q = he_q,
      vlw_q = vlw_q
    )
  )
}

export_mismatch_results <- function(
    data,
    scenario,
    output_file
) {
  mismatch <- prepare_mismatch_data(data)
  
  output <- mismatch$data %>%
    mutate(
      scenario = scenario,
      HE_GDP_Q1 = unname(mismatch$he_q[1]),
      HE_GDP_Q3 = unname(mismatch$he_q[2]),
      VLW_GDP_Q1 = unname(mismatch$vlw_q[1]),
      VLW_GDP_Q3 = unname(mismatch$vlw_q[2]),
      .before = 1
    )
  
  write.csv(
    output,
    output_file,
    row.names = FALSE
  )
  
  list(
    data = output,
    he_q = mismatch$he_q,
    vlw_q = mismatch$vlw_q
  )
}

summarise_mismatch_results <- function(
    mismatch,
    scenario
) {
  data.frame(
    scenario = scenario,
    complete_n = nrow(mismatch$data),
    HE_GDP_Q1 = unname(mismatch$he_q[1]),
    HE_GDP_Q3 = unname(mismatch$he_q[2]),
    VLW_GDP_Q1 = unname(mismatch$vlw_q[1]),
    VLW_GDP_Q3 = unname(mismatch$vlw_q[2]),
    low_HE_high_VLW_n = sum(
      mismatch$data$group == "Low HE / High VLW"
    ),
    low_HE_high_VLW_ISO3 = paste(
      sort(mismatch$data$ISO3[
        mismatch$data$group == "Low HE / High VLW"
      ]),
      collapse = ";"
    ),
    high_HE_low_VLW_n = sum(
      mismatch$data$group == "High HE / Low VLW"
    ),
    high_HE_low_VLW_ISO3 = paste(
      sort(mismatch$data$ISO3[
        mismatch$data$group == "High HE / Low VLW"
      ]),
      collapse = ";"
    ),
    stringsAsFactors = FALSE
  )
}

mismatch_primary <- export_mismatch_results(
  VLW_country,
  scenario = "IE=1.0",
  output_file = "VLW_mismatch_classification.csv"
)

mismatch_sens1 <- export_mismatch_results(
  VLW_country_sens1,
  scenario = "IE=1.5",
  output_file = "VLW_mismatch_classification_sens1.csv"
)

mismatch_sens2 <- export_mismatch_results(
  VLW_country_sens2,
  scenario = "IE=0.55",
  output_file = "VLW_mismatch_classification_sens2.csv"
)

VLW_mismatch_summary <- bind_rows(
  summarise_mismatch_results(
    mismatch_primary,
    "IE=1.0"
  ),
  summarise_mismatch_results(
    mismatch_sens1,
    "IE=1.5"
  ),
  summarise_mismatch_results(
    mismatch_sens2,
    "IE=0.55"
  )
)

expected_mismatch_n <- sum(
  VLW_country$mismatch_eligible
)

if (
  any(
    VLW_mismatch_summary$complete_n !=
    expected_mismatch_n
  )
) {
  stop(
    "Mismatch complete-case counts differ across scenarios or from the inclusion flow.",
    call. = FALSE
  )
}

write.csv(
  VLW_mismatch_summary,
"VLW_mismatch_summary.csv",
  row.names = FALSE
)


# Function for mismatch plot
plot_mismatch <- function(
    data,
    output_file
) {
  
  # Prepare mismatch classification and thresholds
  mismatch <- prepare_mismatch_data(
    data
  )
  
  # Generate scatter plot
  p <- ggplot(
    mismatch$data,
    aes(
      x = HE_GDP,
      y = VLW_GDP,
      colour = group
    )
  ) +
    
    # Country-level observations
    geom_point(
      size = 2.2,
      alpha = 0.85
    ) +
    
    # Add HE/GDP threshold lines
    geom_vline(
      xintercept = mismatch$he_q,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "grey40"
    ) +
    
    # Add VLW/GDP threshold lines
    geom_hline(
      yintercept = mismatch$vlw_q,
      linetype = "dashed",
      linewidth = 0.6,
      colour = "grey40"
    ) +
    
    # Define colours for mismatch categories
    scale_colour_manual(
      values = c(
        "Low HE / High VLW" = "#EB5B25",
        "High HE / Low VLW" = "#36A9E1",
        "Other combinations" = "grey"
      )
    ) +
    
    theme_classic(
      base_size = 13
    ) +
    
    theme(
      legend.title = element_blank(),
      legend.position = "right",
      axis.title = element_text(
        face = "bold"
      ),
      axis.text = element_text(
        colour = "black"
      )
    ) +
    
    labs(
      x = "Health expenditure (% of GDP)",
      y = "VLW (% of GDP)"
    )
  
  # Save figure
  ggsave(
    filename = output_file,
    plot = p,
    width = 6,
    height = 8,
    device = "pdf"
  )
  
  save_manuscript_png(
    output_file,
    p,
    6,
    8
  )
  
  
  return(p)
}

# Generate mismatch plots
# Primary analysis
p_mismatch <- plot_mismatch(
  data = VLW_country,
  output_file = "results/VLW_mismatch.pdf"
)

print(p_mismatch)


# IE = 1.5
p_mismatch_sens1 <- plot_mismatch(
  data = VLW_country_sens1,
  output_file = "results/VLW_mismatch_sens1.pdf"
)

print(p_mismatch_sens1)


# IE = 0.55
p_mismatch_sens2 <- plot_mismatch(
  data = VLW_country_sens2,
  output_file = "results/VLW_mismatch_sens2.pdf"
)

print(p_mismatch_sens2)

# Assemble the exact primary and supplementary figures described in the
# manuscript and supplement. Panel A contains the two geographic maps; Panel B
# contains the complete-case HE/GDP-VLW/GDP classification plot.
assemble_burden_figure <- function(p_vlw, p_he, p_mismatch, output_stem) {
  map_panel <- p_vlw | p_he
  figure <- patchwork::wrap_elements(map_panel) /
    patchwork::wrap_elements(p_mismatch) +
    patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(heights = c(1, 1.1))

  ggsave(
    file.path(results_dir, paste0(output_stem, ".pdf")),
    figure,
    width = 13,
    height = 11
  )
  ggsave(
    file.path(results_dir, paste0(output_stem, ".png")),
    figure,
    width = 13,
    height = 11,
    dpi = 300,
    bg = "white"
  )
  invisible(figure)
}

Figure4 <- assemble_burden_figure(
  p_VLW, p_HE, p_mismatch, "Figure4_primary_IE100"
)
FigureS1 <- assemble_burden_figure(
  p_VLW_sens1, p_HE_sens1, p_mismatch_sens1, "FigureS1_IE150"
)
FigureS2 <- assemble_burden_figure(
  p_VLW_sens2, p_HE_sens2, p_mismatch_sens2, "FigureS2_IE055"
)
