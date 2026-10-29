###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 8 Survival Rates
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 8 ####

# Districts - overall
seqic_indicator_8_districts_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |> # get the first list element, which houses the overall results
  format_seqic_comparison(
    type = "district",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .after = `Service Area`)

# Districts - risk groups
seqic_indicator_8_districts_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |> # get the second list element, which houses the overall results
  format_seqic_comparison(
    type = "district",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the district tibbles
seqic_indicator_8_districts <- seqic_indicator_8_districts_overall |>
  dplyr::bind_rows(seqic_indicator_8_districts_risk) |>
  dplyr::arrange(`Service Area`, Year, Risk_Definition)

# Level - overall
seqic_indicator_8_level_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  format_seqic_comparison(
    type = "level",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .after = Level_I_II)

# Level - risk groups
seqic_indicator_8_level_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  format_seqic_comparison(
    type = "level",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the level tibbles
seqic_indicator_8_level <- seqic_indicator_8_level_overall |>
  dplyr::bind_rows(seqic_indicator_8_level_risk) |>
  dplyr::arrange(`Level_I_II`, Year, Risk_Definition)

###_____________________________________________________________________________
### State, District, and Verification Level Performance Reporting
###_____________________________________________________________________________

# state level
seqic_indicator_8_results_state_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .before = indicator)

# state level - risk groups
seqic_indicator_8_results_state_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(
    Risk_Definition = ifelse(
      is.na(Risk_Definition),
      "Missing",
      Risk_Definition
    )
  )

# union the state level tables
seqic_indicator_8_results_state <- seqic_indicator_8_results_state_overall |>
  dplyr::bind_rows(seqic_indicator_8_results_state_risk) |>
  dplyr::mutate(
    Risk_Definition = factor(
      Risk_Definition,
      levels = c("All", "Low", "Moderate", "High", "Missing")
    )
  ) |>
  dplyr::arrange(Year, Risk_Definition)

# state level - by age group
seqic_indicator_8_results_state_overall_age <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Age_Range"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .before = indicator) |>
  dplyr::mutate(
    Age_Range = ifelse(is.na(Age_Range), "Missing", Age_Range),
    Age_Range = stringr::str_replace(
      string = Age_Range,
      pattern = "-",
      replacement = " to "
    ),
    Age_Range = factor(
      Age_Range,
      levels = c(
        "0 to 9",
        "10 to 19",
        "20 to 29",
        "30 to 39",
        "40 to 49",
        "50 to 59",
        "60 to 69",
        "70 to 79",
        "80 to 89",
        "90 to 99",
        "100+",
        "Missing"
      )
    )
  ) |>
  dplyr::arrange(Year, Age_Range)

# state level by age - risk groups
seqic_indicator_8_results_state_risk_age <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Age_Range"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(
    Risk_Definition = ifelse(
      is.na(Risk_Definition),
      "Missing",
      Risk_Definition
    )
  ) |>
  dplyr::mutate(
    Age_Range = ifelse(is.na(Age_Range), "Missing", Age_Range),
    Age_Range = stringr::str_replace(
      string = Age_Range,
      pattern = "-",
      replacement = " to "
    ),
    Age_Range = factor(
      Age_Range,
      levels = c(
        "0 to 9",
        "10 to 19",
        "20 to 29",
        "30 to 39",
        "40 to 49",
        "50 to 59",
        "60 to 69",
        "70 to 79",
        "80 to 89",
        "90 to 99",
        "100+",
        "Missing"
      )
    )
  ) |>
  dplyr::arrange(Year, Age_Range)

# union the state level age group tables
seqic_indicator_8_results_state_age <- seqic_indicator_8_results_state_overall_age |>
  dplyr::bind_rows(seqic_indicator_8_results_state_risk_age) |>
  dplyr::mutate(
    Risk_Definition = factor(
      Risk_Definition,
      levels = c("All", "Low", "Moderate", "High", "Missing")
    )
  ) |>
  dplyr::arrange(Year, Age_Range, Risk_Definition)

# districts - overall
seqic_indicator_8_results_state_districts_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .before = indicator)

# districts - risk groups
seqic_indicator_8_results_state_districts_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(
    Risk_Definition = ifelse(
      is.na(Risk_Definition),
      "Missing",
      Risk_Definition
    )
  )

# union the district level tables
seqic_indicator_8_results_state_districts <- seqic_indicator_8_results_state_districts_overall |>
  dplyr::bind_rows(seqic_indicator_8_results_state_districts_risk) |>
  dplyr::mutate(
    Risk_Definition = factor(
      Risk_Definition,
      levels = c("All", "Low", "Moderate", "High", "Missing")
    )
  ) |>
  dplyr::arrange(Year, `Service Area`, Risk_Definition)

# districts (wide)
seqic_indicator_8_results_state_districts_wide <- seqic_indicator_8_results_state_districts |>
  dplyr::filter(Year == 2024) |>
  dplyr::select(
    Year,
    `Service Area`,
    Risk_Definition,
    indicator,
    name,
    performance
  ) |>
  tidyr::pivot_wider(
    id_cols = c(Year, Risk_Definition, indicator, name),
    names_from = `Service Area`,
    values_from = performance
  )

# trauma center verification levels overall
seqic_indicator_8_results_state_verification_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .before = indicator)

# trauma center verification levels - risk groups
seqic_indicator_8_results_state_verification_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(
    Risk_Definition = ifelse(
      is.na(Risk_Definition),
      "Missing",
      Risk_Definition
    )
  )

# union the verification level tables
seqic_indicator_8_results_state_verification <- seqic_indicator_8_results_state_verification_overall |>
  dplyr::bind_rows(seqic_indicator_8_results_state_verification_risk) |>
  dplyr::mutate(
    Risk_Definition = factor(
      Risk_Definition,
      levels = c("All", "Low", "Moderate", "High", "Missing")
    )
  ) |>
  dplyr::arrange(Year, Level_I_II, Risk_Definition)

### Export ####

# state level reporting
export_state_data(
  x = seqic_indicator_8_results_state,
  subfolder = "8"
)

# state level by age reporting
export_state_data(
  x = seqic_indicator_8_results_state_age,
  subfolder = "8"
)

# district level reporting
export_state_data(
  x = seqic_indicator_8_results_state_districts,
  subfolder = "8"
)

# wide district level reporting
export_state_data(
  x = seqic_indicator_8_results_state_districts_wide,
  subfolder = "8"
)

# verification level reporting
export_state_data(
  x = seqic_indicator_8_results_state_verification,
  subfolder = "8"
)
