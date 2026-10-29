###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 6 Head injury arrival time
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 6 ####

# Districts
seqic_indicator_6_districts <- trauma_2020_2024 |>
  traumar::seqic_indicator_6(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    low_GCS_indicator = Initial_Assessment_GCS_Less_9,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "district") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level
seqic_indicator_6_level <- trauma_2020_2024 |>
  traumar::seqic_indicator_6(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    low_GCS_indicator = Initial_Assessment_GCS_Less_9,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

###_____________________________________________________________________________
### State, District, and Verification Level Performance Reporting
###_____________________________________________________________________________

# state level
seqic_indicator_6_results_state <- trauma_2020_2024 |>
  traumar::seqic_indicator_6(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    low_GCS_indicator = Initial_Assessment_GCS_Less_9,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    groups = c("Year"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  ))

# state level - by age group
seqic_indicator_6_results_state_age <- trauma_2020_2024 |>
  traumar::seqic_indicator_6(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    low_GCS_indicator = Initial_Assessment_GCS_Less_9,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    groups = c("Year", "Age_Range"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
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

# districts
seqic_indicator_6_results_state_districts <- trauma_2020_2024 |>
  traumar::seqic_indicator_6(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    low_GCS_indicator = Initial_Assessment_GCS_Less_9,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  ))

# districts (wide)
seqic_indicator_6_results_state_districts_wide <- seqic_indicator_6_results_state_districts |>
  dplyr::filter(Year == 2024) |>
  dplyr::select(Year, `Service Area`, indicator, name, performance) |>
  tidyr::pivot_wider(
    id_cols = c(Year, indicator, name),
    names_from = `Service Area`,
    values_from = performance
  )

# trauma center verification levels
seqic_indicator_6_results_state_verification <- trauma_2020_2024 |>
  traumar::seqic_indicator_6(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    low_GCS_indicator = Initial_Assessment_GCS_Less_9,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  ))

### Export ####

# state level reporting
export_state_data(
  x = seqic_indicator_6_results_state,
  subfolder = "6"
)

# state level by age reporting
export_state_data(
  x = seqic_indicator_6_results_state_age,
  subfolder = "6"
)

# district level reporting
export_state_data(
  x = seqic_indicator_6_results_state_districts,
  subfolder = "6"
)

# wide district level reporting
export_state_data(
  x = seqic_indicator_6_results_state_districts_wide,
  subfolder = "6"
)

# verification level reporting
export_state_data(
  x = seqic_indicator_6_results_state_verification,
  subfolder = "6"
)
