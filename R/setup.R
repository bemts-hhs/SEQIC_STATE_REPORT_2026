# IOWA SEQIC REPORT PREP 2025 ----------------------------------------------
# This script prepares for the analyses using the `traumar` package v1.2.7

# Handy Functions --------------------------------------------------------------

# CALCULATION FACILITIES =====================================================

# Apply rm_bin_summary with dynamic parameters based on sample size
#
# This function:
# - Accepts a dataset with propensity scores and binary outcomes.
# - Checks for insufficient data (n < 2) and returns a 1-row tibble with
#   pre-defined structure filled with NA_real_ values.
# - Otherwise, determines the number of observations in the dataset and
#   sets thresholds and divisors dynamically:
#     - ≤100 rows: thresholds (0.8, 0.9), divisors = 1
#     - 101–200 rows: thresholds (0.9, 0.99), divisors = 1
#     - >200 rows: thresholds (0.9, 0.99), divisors = 3
# - Applies `traumar::rm_bin_summary()` with the appropriate parameters,
#   using `{{ }}` to support unquoted column names.
# - Returns a tibble summarizing binary outcome performance across propensity bins,
#   with optional bootstrap-based confidence intervals.
#
# Used to flexibly summarize performance metrics across variable sample sizes,
# including edge-case handling for very small cohorts.
dynamic_rm_bin_summary <- function(
  data,
  Ps_col,
  outcome_col,
  group_vars,
  n_samples,
  bootstrap_ci,
  seed
) {
  # Count observations
  n_obs <- nrow(data)

  # safeguard against n_obs < 2
  if (n_obs < 2) {
    # if n_obs < 2, produce a tibble 1 x 19 with all NA_real_ values for that
    # facility and year
    results <- tibble::tibble(
      bin_number = NA_real_,
      TA_b = NA_real_,
      TD_b = NA_real_,
      N_b = NA_real_,
      EM_b = NA_real_,
      AntiS_b = NA_real_,
      AntiM_b = NA_real_,
      bin_start = NA_real_,
      bin_end = NA_real_,
      midpoint = NA_real_,
      R_b = NA_real_,
      population_RMM_LL = NA_real_,
      population_RMM = NA_real_,
      population_RMM_UL = NA_real_,
      population_CI = NA_real_,
      bootstrap_RMM_LL = NA_real_,
      bootstrap_RMM = NA_real_,
      bootstrap_RMM_UL = NA_real_,
      bootstrap_CI = NA_real_
    )

    # Return that fully missing result
    return(results)
  }

  # Define dynamic parameters based on sample size
  params <- if (n_obs <= 100) {
    list(Divisor1 = 1, Divisor2 = 1, Threshold_1 = 0.8, Threshold_2 = 0.9)
  } else if (n_obs <= 200) {
    list(Divisor1 = 1, Divisor2 = 1, Threshold_1 = 0.9, Threshold_2 = 0.99)
  } else {
    list(Divisor1 = 3, Divisor2 = 3, Threshold_1 = 0.9, Threshold_2 = 0.99)
  }

  # Call rm_bin_summary with dynamic thresholds/divisors
  results <- traumar::rm_bin_summary(
    data = data,
    Ps_col = {{ Ps_col }},
    outcome_col = {{ outcome_col }},
    group_vars = group_vars,
    n_samples = n_samples,
    Divisor1 = params$Divisor1,
    Divisor2 = params$Divisor2,
    Threshold_1 = params$Threshold_1,
    Threshold_2 = params$Threshold_2,
    bootstrap_ci = bootstrap_ci,
    seed = seed
  )

  # Return the results as a tibble
  return(results)
}

# Function to compute the statistical mode of a vector
# Returns the most frequently occurring value in x
# Handles NA values by removing them before computation (optional)
stat_mode <- function(x, na.rm = TRUE) {
  # Optionally remove missing values from the vector
  if (na.rm) {
    # Remove missing values to avoid skewing counts and errors
    x <- stats::na.omit(x)
  } else if (any(is.na(x))) {
    # If NAs present and not removing, return NA as mode is undefined
    return(NA)
  }

  # Handle empty input gracefully
  if (length(x) == 0) {
    return(NA)
  }

  # Get all unique values from the vector
  # These are the candidate mode values
  ux <- unique(x)

  # Find the position of each element of x in the unique values vector
  # This creates an integer index vector corresponding to ux
  idx <- match(x, ux)

  # Count the number of times each unique value appears in x
  # The result is an integer vector with counts aligned to ux
  counts <- tabulate(idx)

  # Determine the index of the unique value with the highest count
  # This identifies the position of the mode in ux
  mode_index <- which.max(counts)

  # Return the mode value itself, i.e., the unique value with the highest frequency
  ux[mode_index]
}

###_____________________________________________________________________________
### Helper to summarize reinjury counts after filtering for reinjuries (n > 1)
### Input:
###   - df: tibble with counts column `n` (injury counts per patient per group)
###   - grouping_vars: character vector for grouping variable names (for .by)
### Output:
###   - tibble summarizing min, max, mode, quartiles, and median of reinjury counts
###_____________________________________________________________________________
summarize_reinjury_stats <- function(df, grouping_vars = grouping_vars) {
  df |>
    dplyr::filter(n > 1) |> # retain only reinjuries (counts > 1)
    dplyr::summarize(
      Min_Reinjury = min(n, na.rm = TRUE),
      Max_Reinjury = max(n, na.rm = TRUE),
      Mode_Reinjury = stat_mode(n, na.rm = TRUE),
      Q25_Reinjury = stats::quantile(n, probs = 0.25, na.rm = TRUE, type = 7),
      Median_Reinjury = stats::quantile(n, probs = 0.5, na.rm = TRUE, type = 7),
      Q75_Reinjury = stats::quantile(n, probs = 0.75, na.rm = TRUE, type = 7),
      .by = dplyr::all_of(grouping_vars)
    )
}

###_____________________________________________________________________________
### Helper function to compute period-to-period change in counts
### Adds three columns:
###   - change: raw numeric difference in count (n - lag(n))
###   - prop_change: proportional change ((n - lag(n)) / lag(n))
###   - prop_label: formatted percent label using traumar::pretty_percent()
### This assumes that the input data is already grouped and counted, with a column `n`
###_____________________________________________________________________________
add_change_metrics <- function(df, group = NULL) {
  df |>
    dplyr::mutate(
      # Compute raw numeric change from previous row
      change = n - dplyr::lag(n),

      # Compute proportional change (as a fraction)
      prop_change = (n - dplyr::lag(n)) / dplyr::lag(n),

      # Generate a human-readable percent label (e.g., "14.2%")
      # Use NA_character_ if proportional change is NA (e.g., first row)
      prop_label = ifelse(
        !is.na(prop_change),
        traumar::pretty_percent(prop_change, n_decimal = 2),
        NA_character_
      ),
      .by = {{ group }}
    )
}

###_____________________________________________________________________________
# Custom functions for counts ----
###_____________________________________________________________________________

{
  ###_____________________________________________________________________________
  ### Custom function to get unique count of injury cases in Patient Registry
  ### Equivalent to counting unique inpatient visits based on Unique_Incident_ID
  ### Supports dynamic grouping via tidy evaluation (bare column names in ...)
  ###_____________________________________________________________________________
  injury_case_count <- function(
    df,
    ...,
    descriptive_stats = FALSE,
    group = NULL
  ) {
    # Capture grouping variables as symbols and convert to character strings for .by and joins
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Create temporary dataset with unique incident rows
    # Unique incident defined by Unique_Incident_ID (one row per inpatient visit)
    temp <- df |>
      dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE)

    if (!descriptive_stats) {
      # Simple count of unique cases by user-defined grouping variables
      out <- temp |>
        dplyr::count(...)

      cli::cli_alert_success(
        "Returning the count(s) of total unique inpatient injury cases."
      )

      return(out)
    }

    # When descriptive_stats = TRUE, add change metrics (numeric and percent change)
    out <- temp |>
      dplyr::count(...) |>
      add_change_metrics(group = {{ group }})

    cli::cli_alert_success(
      "Returning the count(s) of total unique inpatient injury cases and descriptive statistics."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of injuries in Patient Registry
  ### A true estimation of the number of incidents (not encounters or records)
  ###_____________________________________________________________________________
  injury_incident_count <- function(
    df,
    ...,
    descriptive_stats = FALSE,
    group = NULL
  ) {
    # Capture grouping variables from bare column names (e.g., Year, County)
    grouping_syms <- rlang::ensyms(...) # capture as symbols for tidy eval
    grouping_vars <- sapply(grouping_syms, rlang::as_string) # convert to character for .by and join

    # Create a temporary object with one row per unique injury event
    # A unique event is defined as a unique combination of Incident_Date and Unique_Patient_ID
    # This removes duplicated encounters for the same incident
    temp <- df |>
      dplyr::distinct(Year, Incident_Date, Unique_Patient_ID, .keep_all = TRUE)

    if (!descriptive_stats) {
      # Return a simple count of injury events by grouping variable(s)
      out <- temp |>
        dplyr::count(!!!grouping_syms)

      cli::cli_alert_success(
        "Returning the count(s) of total unique injury events leading to a trauma center visit."
      )

      return(out)
    }

    # When descriptive_stats = TRUE, include additional summary statistics
    # This captures reinjury characteristics for patients who appear >1 time per group

    # Step 1: Identify reinjury patterns (count > 1 per group)
    stat <- temp |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |>
      dplyr::count(!!!grouping_syms, Unique_Patient_ID) |>
      summarize_reinjury_stats(grouping_vars = grouping_vars)

    # Step 2: Count events and calculate change metrics
    out <- temp |>
      dplyr::count(!!!grouping_syms) |>
      add_change_metrics(group = {{ group }}) |>
      dplyr::left_join(stat, by = grouping_vars) # <- this fixes the join

    cli::cli_alert_success(
      "Returning the count(s) of total unique injury events leading to a trauma center visit and descriptive statistics."
    )

    return(out)
  }

  ###_____________________________________________________________________________
  ### Custom function to get unique count of patients in Patient Registry
  ### Estimates number of unique patients with trauma center visits per group
  ### Assumes Unique_Patient_ID is consistent across years (mostly)
  ### Groups first, then deduplicates per group to count unique patients correctly
  ### Supports flexible grouping via tidy evaluation (bare column names in ...)
  ###_____________________________________________________________________________
  injury_patient_count <- function(
    df,
    ...,
    descriptive_stats = FALSE,
    group = NULL
  ) {
    # Capture grouping variables as symbols and convert to character strings for .by and joins
    grouping_syms <- rlang::ensyms(...)
    grouping_vars <- sapply(grouping_syms, rlang::as_string)

    # Prepare dataset: remove rows with missing patient IDs, group by user variables,
    # then select distinct Unique_Patient_ID within each group to avoid counting duplicates
    temp <- df |>
      dplyr::filter(!is.na(Unique_Patient_ID)) |>
      dplyr::group_by(!!!grouping_syms) |>
      dplyr::distinct(Unique_Patient_ID, .keep_all = TRUE) |>
      dplyr::ungroup()

    if (!descriptive_stats) {
      # Basic count of unique patients per group
      out <- temp |>
        dplyr::count(!!!grouping_syms)

      cli::cli_alert_success(
        "Returning the count(s) of total unique patients who had a trauma center visit and have a non-missing unique patient ID."
      )

      return(out)
    }

    # When descriptive_stats = TRUE, add change metrics for counts
    out <- temp |>
      dplyr::count(!!!grouping_syms) |>
      add_change_metrics(group = {{ group }})

    cli::cli_alert_success(
      "Returning the count(s) of total unique patients who had a trauma center visit and have a non-missing unique patient ID and descriptive statistics."
    )

    return(out)
  }

###_____________________________________________________________________________
# calc_age_adjusted_rate()
# Calculate crude and directly standardized EMS overdose rates with 95% CIs

# This function computes:
# - Age‐specific crude rates
# - Directly standardized (age‐adjusted) rates
# - Crude rate 95% confidence intervals using chi‐square limits
# - Age‐adjusted rate 95% confidence intervals using the Fay–Feuer gamma method

# Assumptions:
# - The input data must contain pre‐aggregated age‐stratum rows
# (for example, County × Year × Age Group).
# - {{ count }}, {{ local_population }}, {{ standard_pops }}, and
# {{ standard_population_weight }} must be scalar fields per row.
# - StandardPopulations and weight must correspond to your binned US Standard
#   Million.
# - {{ .by }} identifies the grouping level for final rate summaries
# (for example: "INCIDENT_YEAR", "CTYNAME").

# Methodology:
# - Crude rates are calculated as:
# (sum(count) / sum(local_population)) × rate
# - Crude CIs use chi‐square limits:
# Lower = (0.5 × chisq(α/2, df = 2×Count)) / Population × rate
# Upper = (0.5 × chisq(1−α/2, df = 2×Count+1)) / Population × rate

# - Age‐adjusted rates follow direct standardization:
# AAR = Σ_i (count_i / pop_i) × standard_population_weight_i

# - Fay–Feuer variance components use:
# w_i = stdmil_i / (pop_i × Σ_j stdmil_j)
# v  = Σ_i (w_i^2 × count_i)

# - Lower and upper age‐adjusted limits use the Fay–Feuer gamma method:
# Lower = (v / (2 × R)) × chisq(α/2, df = 2 × (R^2 / v)) × rate
# Upper = ((v + w_max^2) / (2 × (R + w_max))) ×
# chisq(1−α/2, df = 2 × ((R + w_max)^2 / (v + w_max^2))) × rate
# where R = Age_Adjusted_Rate / rate

# Output:
# - A tibble containing Count, Population, Crude_Rate, Crude_Rate CIs,
# Age_Adjusted_Rate, and Fay–Feuer lower and upper limits for each grouping.

# Notes:
# - No variable names are altered.
# - The function stays faithful to the formulas you provided.

# References:
# - https://eportal.isdh.in.gov/CancerRegistry/help/formulas.htm
# - https://seer.cancer.gov/seerstat/tutorials/aarates/definition.html
###_____________________________________________________________________________

calc_age_adjusted_rate <- function(
  data, # tibble or data.frame of age-stratified rows
  count, # bare column name: observed count in each age stratum
  local_population, # bare column name: population in each age stratum
  standard_pops, # bare column name: standard population for each age stratum
  standard_population_weight, # bare column name: normalized standard weight for each stratum
  .by = NULL, # character vector of grouping variables (e.g., "INCIDENT_YEAR", "CTYNAME")
  rate = 100000, # multiplier: typically per 100,000 population
  conf_level = 0.95 # confidence level for CIs
) {
  # ____________________________________________________________________________
  # Extract distinct standard population and weight values.
  # This ensures the standard population total is computed once, not per-row.
  # standard_pops_wts$pops = stdmil_i (SEER standard population counts)
  # standard_pops_wts$wts = w_m (normalized SEER weights for rate calculation)
  # ____________________________________________________________________________
  std_pops_wts <- tibble::tibble(
    pops = data |> dplyr::select({{ standard_pops }}) |> dplyr::pull(),
    wts = data |>
      dplyr::select({{ standard_population_weight }}) |>
      dplyr::pull()
  ) |>
    dplyr::distinct()

  # Total standard population across all age groups: Σ_j stdmil_j.
  std_pops_total <- sum(std_pops_wts$pops)

  # ____________________________________________________________________________
  # Compute lower and upper α for confidence intervals.
  # alpha_lower = α/2 ; alpha_upper = 1 − α/2.
  # ____________________________________________________________________________
  alpha <- 1 - conf_level
  alpha_lower <- alpha / 2
  alpha_upper <- 1 - alpha_lower

  # ____________________________________________________________________________
  # Step 1: Create age-stratum-level metrics.
  #
  # crude_rate_i = (d_i / p_i) × rate
  # weighted_rate_i = crude_rate_i × w_m_i  (direct standardization)
  #
  # w_i = stdmil_i / (pop_i × Σ_j stdmil_j)
  # This is the Fay–Feuer variance weight, *not* the normalized weight.
  # ____________________________________________________________________________
  rate_data <- data |>
    dplyr::mutate(
      crude_rate = ({{ count }} / {{ local_population }}) * rate,

      weighted_rate = crude_rate * {{ standard_population_weight }},

      w = {{ standard_pops }} /
        ({{ local_population }} * std_pops_total)
    )

  # ____________________________________________________________________________
  # Step 2: Aggregate age-stratum metrics to the requested grouping level.
  #
  # Count: Σ_i d_i
  # Population: Σ_i p_i
  #
  # Crude_Rate = (Count / Population) × rate
  #
  # Crude CI: Chi-square limits for Poisson mean scaled to crude rate.
  #
  # Age_Adjusted_Rate = Σ_i weighted_rate_i
  #
  # AAR = unscaled adjusted rate = Age_Adjusted_Rate / rate
  #
  # Variance for direct standardized rate:
  # D = Σ_i (w_i² d_i)
  # Note: v is the D term in your notation.
  #
  # Lower CI (Fay–Feuer):
  #   L = (v / (2R)) × χ²(α/2, df = 2(R²/v)) × rate
  #
  # Upper CI (Fay–Feuer):
  #   U = [(v + w_max²) / (2(R + w_max))] ×
  #        χ²(1−α/2, df = 2((R + w_max)² / (v + w_max²))) × rate
  #
  # ____________________________________________________________________________
  rate_summary <- rate_data |>
    dplyr::summarize(
      Count = sum({{ count }}, na.rm = TRUE),
      Population = sum({{ local_population }}, na.rm = TRUE),

      # Crude rate and SE
      Crude_Rate = Count / Population * rate,
      Crude_Rate_SE = (sqrt(Count) / Population) * rate,

      # Chi-square crude rate lower and upper limits
      Crude_Rate_Lower = (0.5 * qchisq(p = alpha_lower, df = 2 * Count)) /
        Population *
        rate,

      Crude_Rate_Upper = (0.5 * qchisq(p = alpha_upper, df = (2 * Count) + 1)) /
        Population *
        rate,

      # Directly standardized rate
      Age_Adjusted_Rate = sum(weighted_rate, na.rm = TRUE),

      # Unscaled adjusted rate (R in formulas)
      AAR = Age_Adjusted_Rate / rate,

      # Standard error for age-adjusted rate (gamma-based variance component)
      Age_Adjusted_Rate_SE = sqrt(
        sum(
          ({{ standard_population_weight }}^2) *
            ({{ count }} / ({{ local_population }}^2)),
          na.rm = TRUE
        ) *
          rate^2
      ),

      # Fay–Feuer components: w_max and v = Σ_i w_i² d_i
      max_w = max(w, na.rm = TRUE),
      v = sum((w^2) * {{ count }}, na.rm = TRUE),

      # Fay–Feuer Lower limit
      Age_Adjusted_Rate_Lower = ((v / (2 * AAR)) *
        qchisq(
          p = alpha_lower,
          df = (2 * (AAR^2)) / v # df = 2(R² / v)
        )) *
        rate,

      # Fay–Feuer Upper limit
      Age_Adjusted_Rate_Upper = ((v + (max_w^2)) /
        (2 * (AAR + max_w))) *
        qchisq(
          p = alpha_upper,
          df = (2 * ((AAR + max_w)^2)) /
            (v + (max_w^2))
        ) *
        rate,

      .by = tidyselect::all_of({{ .by }})
    ) |>
    dplyr::select(
      -AAR, # remove intermediate unscaled rate
      -max_w, # remove Fay–Feuer w_max intermediate
      -v # remove variance component intermediate
    )

  return(rate_summary)
}

# DATA MANIPULATION FACILITIES ===============================================

###_____________________________________________________________________________
# Standard output from traumar is in wide format, with separate columns for each
# SEQIC indicator's numerator, denominator, and calculated value (seqic_*).
#
# The following metadata and helper functions facilitate transforming that wide
# output into a long, tidy format that is easier to analyze, visualize, and
# interpret.
#
# Recommended workflow:
# 1. Compute indicators using traumar::seqic_indicator_*() functions.
# 2. Pass the resulting wide-format data to reshape_seqic_indicators()
#    to pivot it into long format with separate rows per indicator.
# 3. Use match_seqic_indicator() to append indicator names and goals
#    by matching on the indicator code.
# 4. Compute comparison service area and trauma facility verification level
# results
# 5. Use format_seqic_comparison() to clean up service area and comparable trauma
# facility verification results
# 6. Join the service area and comparable verified trauma center data to the
# agency-level data to provide a type of benchmark
###_____________________________________________________________________________

# seqic_indicator_names
# A reference data frame containing SEQIC indicator metadata used for mapping,
# labeling, and evaluation of performance metrics. It includes:
#
# Columns:
# - Indicator: Character code identifying each SEQIC indicator (e.g., "1a", "9b_all").
# - Indicator_Name: Descriptive name of the indicator, suitable for labeling or reporting.
# - Goal: Numeric target threshold (if applicable) representing the performance benchmark
#         for the indicator. Values are `NA_real_` where no explicit goal is defined.
#
# This dataset supports automated indicator classification and goal evaluation within
# trauma and EMS quality assurance workflows.
seqic_indicator_names <- data.frame(
  Indicator = c(
    "1a",
    "1b",
    "1c",
    "1d",
    "1e",
    "1f",
    "2",
    "3",
    "4a",
    "4b",
    "5a",
    "5b",
    "5c",
    "5d",
    "6",
    "7",
    "8_all",
    "8_risk",
    "9a_all",
    "9b_all",
    "9c_all",
    "9d_all",
    "9e_all",
    "9f_all",
    "9a_activations",
    "9b_activations",
    "9c_activations",
    "9d_activations",
    "9e_activations",
    "9f_activations",
    "9a_risk",
    "9b_risk",
    "9c_risk",
    "9d_risk",
    "9e_risk",
    "9f_risk",
    "9a_activations_risk",
    "9b_activations_risk",
    "9c_activations_risk",
    "9d_activations_risk",
    "9e_activations_risk",
    "9f_activations_risk",
    "10a",
    "10b",
    "10c",
    "11",
    "12",
    "13"
  ),
  Indicator_Name = c(
    "Indicator 1a - Trauma Surgeon Responding Within 15 Minutes",
    "Indicator 1b - Trauma Surgeon Responding Within 30 Minutes",
    "Indicator 1c - Trauma Surgeon Response Time Unknown",
    "Indicator 1d - Physician Responding Within 5 Minutes",
    "Indicator 1e - Physician Responding Within 20 Minutes",
    "Indicator 1f - Physician Response Time Unknown",
    "Indicator 2 - Injury Time Blank",
    "Indicator 3 - Probability of Survival Calculated",
    "Indicator 4a - Deceased Trauma Patient Autopsied",
    "Indicator 4b - No Autopsy on Death with LOS > 72 Hours",
    "Indicator 5a - Blood ETOH Measured",
    "Indicator 5b - Blood ETOH Positive",
    "Indicator 5c - Drug Screen Completed",
    "Indicator 5d - Drug Screen Positive",
    "Indicator 6 - GCS Less Than 9 And Arrived to Definitive Care > 3 Hours from Injury",
    "Indicator 7 - Patients to Definitive Care > 3 Hours",
    "Indicator 8 - Overall Survival Rate",
    "Indicator 8 - Survival Rate by Risk Group",
    "Indicator 9a - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
    "Indicator 9b - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
    "Indicator 9c - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
    "Indicator 9d - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
    "Indicator 9e - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
    "Indicator 9f - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
    "Indicator 9a By TTA - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
    "Indicator 9b By TTA - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
    "Indicator 9c By TTA - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
    "Indicator 9d By TTA - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
    "Indicator 9e By TTA - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
    "Indicator 9f By TTA - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
    "Indicator 9a By Risk Group - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
    "Indicator 9b By Risk Group - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
    "Indicator 9c By Risk Group - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
    "Indicator 9d By Risk Group - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
    "Indicator 9e By Risk Group - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
    "Indicator 9f By Risk Group - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
    "Indicator 9a By TTA and Risk Group - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
    "Indicator 9b By TTA and Risk Group - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
    "Indicator 9c By TTA and Risk Group - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
    "Indicator 9d By TTA and Risk Group - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
    "Indicator 9e By TTA and Risk Group - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
    "Indicator 9f By TTA and Risk Group - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
    "Indicator 10a - Major Trauma With Limited-to-No TTA at Definitive Care (Cribari)",
    "Indicator 10b - Minor Trauma With the Highest Level TTA at Definitive Care",
    "Indicator 10c - Major Trauma With Limited-to-No TTA at Definitive Care (Modified Cribari)",
    "Indicator 11 - ISS < 9 With ED LOS < 24 Hours Among Patients Transferred to Definitive Care",
    "Indicator 12 - Incidents Submitted Within 60 Days of Patient Discharge",
    "Indicator 13 - Incidents with Validity Score > 84"
  ),
  Goal = c(
    0.80,
    0.80,
    0,
    0.80,
    1,
    0,
    0.25,
    0.90,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    0,
    0,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    NA_real_,
    0.05,
    0.35,
    NA_real_,
    NA_real_,
    0.80,
    0.90
  )
)

# Reshape SEQIC indicator columns from wide to long format
#
# Expects input data where each indicator (e.g., 1, 1a, 2, 8, etc.) has its own
# set of columns, such as:
#   numerator_8, denominator_8, seqic_8, lower_ci_8, upper_ci_8, etc.
#
# This function:
# - Optionally renames columns dynamically using user-defined regex patterns. Uses
# `pivot_longer()` with a regex pattern to split wide-format indicator data into
# long format.
# - Extracts both the value type (numerator, denominator, seqic, lower_ci,
# upper_ci) and the indicator code (e.g., "8", "8_all") into separate
# components.
# - Uses `.value` in `names_to` to spread the value types into columns.
# - Renames `seqic` to `performance` and standardizes `lower_ci` and `upper_ci` to
# `lower ci` and `upper ci` for readability.
#
# Optional renaming parameters (`rename_cols`, `column_pattern`,
# `match_pattern`, `replace_with`) allow users to dynamically standardize
# indicator suffixes (e.g., converting "_8" to "_8_all").
#
# Returns a long-format tibble with columns: indicator, numerator, denominator,
# performance, lower ci, and upper ci, along with any grouping variables like
# Year, Service Area, etc.

reshape_seqic_indicators <- function(
  data,
  rename_cols = FALSE, # Whether to apply dynamic renaming to columns
  column_pattern = NULL, # Regex pattern to match columns of interest
  match_pattern = NULL, # Regex pattern to match suffixes needing renaming
  replace_with = NULL # Replacement string to apply if renaming is enabled
) {
  # If dynamic renaming is requested but pattern/replacement is missing, throw error
  if (rename_cols) {
    if (
      is.null(match_pattern) || is.null(replace_with) || is.null(column_pattern)
    ) {
      rlang::abort(
        "If `rename_cols = TRUE`, both `match_pattern` and `replace_with` must be provided."
      )
    }

    # Apply regex replacement to any column names matching lower/upper CI patterns
    data <- data |>
      dplyr::rename_with(
        .cols = tidyselect::matches(column_pattern), # Select only CI-related columns
        ~ stringr::str_replace_all(
          .,
          pattern = match_pattern,
          replacement = replace_with
        )
      )
  }

  # Reshape wide-format indicator columns to long format using `pivot_longer()`
  data |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c(
        "numerator_",
        "denominator_",
        "seqic_",
        "lower_ci",
        "upper_ci"
      )),
      names_to = c(".value", "indicator"), # Use `.value` to keep value type columns separate
      names_pattern = "(numerator|denominator|seqic|lower_ci|upper_ci)_(\\d+[a-z]?_?\\w*_?\\w*)"
      # This regex splits names into the value type (e.g., numerator) and the indicator (e.g., 8_all)
    ) |>
    dplyr::rename(
      performance = seqic, # Rename for clarity
      `lower ci` = lower_ci,
      `upper ci` = upper_ci
    )
}


# match_seqic_indicator()
# This function searches for the first matching SEQIC indicator code
# (as defined in `seqic_indicator_names`) within a specified text column.
# It appends three new columns to the input dataframe:
#   - matched_indicator: the first matching indicator code (e.g., "9b_all")
#   - Indicator_Name: the corresponding descriptive name of the indicator
#   - Goal: the numeric target threshold associated with the indicator
#
# Usage:
#   match_seqic_indicator(data, col)
#   - data: a dataframe containing a column with text to search
#   - col: unquoted name of the column to search (uses tidy evaluation)
#
# Note: Only the first match per row is returned. If no match is found,
# all new columns will be NA for that row.
match_seqic_indicator <- function(
  data,
  col,
  indicator_df = seqic_indicator_names,
  performance_col
) {
  # Build regex pattern to match indicators exactly (as whole words)
  pattern <- stringr::str_c(indicator_df$Indicator, collapse = "|")

  data |>
    dplyr::mutate(
      name = dplyr::case_when(
        !is.na({{ col }}) ~
          indicator_df$Indicator_Name[
            match({{ col }}, indicator_df$Indicator)
          ],
        TRUE ~ NA_character_
      ),
      goal = dplyr::case_when(
        !is.na({{ col }}) ~
          indicator_df$Goal[
            match({{ col }}, indicator_df$Indicator)
          ],
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::relocate(name, .after = {{ col }}) |>
    dplyr::relocate(goal, .after = {{ performance_col }})
}

# --- format_seqic_comparison ---
# Generalized function to format SEQIC indicator results for use as
# comparison benchmarks (district or trauma facility level).
#
# Arguments:
# - data: A tibble of wide-format SEQIC indicator results.
# - type: A string specifying the benchmark type, either "district" or "level".
#
# Output:
# A long-format tibble with indicator metadata and a single column renamed to
# reflect the comparison type:
# - "comparison district performance" or
# - "comparison trauma facility performance"
#
# Usage in workflow:
# 1. Calculate comparison data grouped by district or trauma level.
# 2. Use this function to prepare those data for joining with agency-level outputs.

format_seqic_comparison <- function(
  data,
  type = c("district", "level"),
  rename_cols = FALSE, # Whether to apply dynamic renaming to columns
  column_pattern = NULL, # Regex pattern to match columns of interest
  match_pattern = NULL, # Regex pattern to match suffixes needing renaming
  replace_with = NULL # Replacement string to apply if renaming is enabled
) {
  type <- rlang::arg_match(type)

  # Determine appropriate name
  perf_col <- dplyr::case_when(
    type == "district" ~ "comparison district performance",
    type == "level" ~ "comparison trauma facility performance"
  )

  data |>
    reshape_seqic_indicators(
      rename_cols = rename_cols, # Whether to apply dynamic renaming to columns
      column_pattern = column_pattern, # Regex pattern to match columns of interest
      match_pattern = match_pattern, # Regex pattern to match suffixes needing renaming
      replace_with = replace_with # Replacement string to apply if renaming is enabled
    ) |>
    match_seqic_indicator(col = indicator, performance_col = performance) |>
    dplyr::select(-c(name, numerator, denominator, goal)) |>
    dplyr::rename(!!perf_col := performance)
}

# Join comparison benchmarks to SEQIC results and format for reporting
#
# This function merges SEQIC performance results with comparison benchmarks
# at both the state level (`data_level`) and the district level (`data_district`).
#
# It performs the following operations:
# - Joins `data` to `data_level` by Year, Level I/II designation, and indicator code.
# - Joins `data` to `data_district` by Year, Service Area, and indicator code.
# - Applies `pretty_percent()` formatting from the traumar package to all relevant
#   percentage-based columns, rounding to 2 decimal places and preserving `NA` values.
# - Standardizes all column names to lowercase.
# - Renames `level_i_ii` to `level` for consistency in output.
#
# Expected inputs:
# - `data`: Long-format SEQIC results with columns like Year, Level_I_II, Service Area, and indicator.
# - `data_level`: State-level comparison data, matched on Year, Level_I_II, and indicator.
# - `data_district`: Region-level comparison data, matched on Year, Service Area, and indicator.
#
# Returns a formatted tibble with performance values and comparisons ready for tabular report output.
join_comparison_data <- function(data, data_level, data_district) {
  data |>
    dplyr::left_join(
      data_level,
      by = dplyr::join_by(Year, Level_I_II, indicator)
    ) |>
    dplyr::left_join(
      data_district,
      by = dplyr::join_by(Year, `Service Area`, indicator)
    ) |>
    dplyr::mutate(dplyr::across(
      c(
        performance,
        goal,
        `lower ci`,
        `upper ci`,
        `comparison trauma facility performance`,
        `comparison district performance`
      ),
      ~ ifelse(
        is.na(.),
        NA_real_,
        traumar::pretty_percent(variable = ., n_decimal = 2)
      )
    )) |>
    dplyr::rename_with(~ tolower(.)) |>
    dplyr::rename(level = level_i_ii)
}

# DATA EXPORT FACILITIES =====================================================

# export_state_data
# Export State Data to CSV
#
# This function exports a given data frame or tibble to a CSV file. The file is saved in a dynamically generated
# directory based on the provided root directory and subfolder name. The file name is constructed using the name
# of the input data frame or tibble.
#
# Parameters:
# - x: A data frame or tibble containing the data to be exported.
#
# - output_dir_root: A string specifying the root directory where the output file will be saved. Defaults to the
#   value of the "STATE_OUTPUT_FILE_PATH" environment variable.
#
# - subfolder: A string specifying the subfolder name to be used in the output file path.
#
# Side Effects:
# - Creates the output directory if it does not already exist.
#
# - Writes the data frame or tibble to a CSV file in the specified directory.
#
# Output:
# - Returns `invisible(NULL)`; the primary function is the side-effect of file export.
#
# Workflow Context:
# - Use this function to export state-level data to CSV files for further analysis or reporting.
#
export_state_data <- function(
  x,
  output_dir_root = Sys.getenv("STATE_OUTPUT_FILE_PATH"),
  subfolder
) {
  # Validate input
  # Check if 'x' is a tibble or data frame. If not, abort the function with an error message.
  if (!tibble::is_tibble(x) && !is.data.frame(x)) {
    rlang::abort("`seqic_results` must be a data.frame or tibble.")
  }

  # Get the name of 'x' as a character string
  # This will be used to dynamically create the file name.
  x_name <- deparse(substitute(x))

  # Create dynamic output path
  # Construct the output directory path using the provided root directory.
  output_path <- fs::path(output_dir_root)
  # Create the directory if it does not exist.
  fs::dir_create(output_path)

  # Build file path and write CSV
  # Construct the full file path by combining the output directory, subfolder, and the name of 'x' with a ".csv" extension.
  file_path <- fs::path(
    output_path,
    subfolder,
    paste0(x_name, ".csv")
  )

  # Create the final directory if it does not exist.
  fs::dir_create(output_path, subfolder)

  # Write the data frame or tibble 'x' to the constructed file path in CSV format.
  readr::write_csv(x = x, file = file_path)
}
