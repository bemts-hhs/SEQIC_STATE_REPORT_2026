###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 9 Transfer Delays
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 9 ####

# Districts - overall
seqic_indicator_9_districts_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |> # get the first list element, which houses the overall results
  format_seqic_comparison(type = "district") |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Service Area`)

# Districts - by trauma team activation status
seqic_indicator_9_districts_activation <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |> # get the second list element, which houses the overall results
  format_seqic_comparison(type = "district") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the district tibbles
seqic_indicator_9_districts <- seqic_indicator_9_districts_overall |>
  dplyr::bind_rows(seqic_indicator_9_districts_activation) |>
  dplyr::arrange(`Service Area`, Year, Trauma_Team_Activated)

# Level - overall
seqic_indicator_9_level_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = Level_I_II)

# Level - by trauma team activation status
seqic_indicator_9_level_activation <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the level tibbles
seqic_indicator_9_level <- seqic_indicator_9_level_overall |>
  dplyr::bind_rows(seqic_indicator_9_level_activation) |>
  dplyr::arrange(`Level_I_II`, Year, Trauma_Team_Activated)

###_____________________________________________________________________________
### More granular analyses
### These analyses will not have the same features as the previous
### They will just report the simple outputs from the functions and compare each
### facility to others in their district and among similarly verified trauma
### centers
###_____________________________________________________________________________

# Districts - risk group
seqic_indicator_9_state_districts_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(3) |> # get the third list element, which houses the risk group results
  format_seqic_comparison(type = "district") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Districts - activations and risk group
seqic_indicator_9_state_districts_activations_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(4) |> # get the fourth list element, which houses the activation / risk group results
  format_seqic_comparison(type = "district") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level - risk group
seqic_indicator_9_level_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(3) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level - activations and risk group
seqic_indicator_9_level_activations_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(4) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

###_____________________________________________________________________________
### State, District, and Verification Level Performance Reporting
###_____________________________________________________________________________

# state level - overall
seqic_indicator_9_results_state_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |> # get the first list element, which houses the overall results
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
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = indicator)

# state level - trauma team activations
seqic_indicator_9_results_state_activations <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |> # get the first list element, which houses the overall results
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

# union the state level tables
seqic_indicator_9_results_state <- seqic_indicator_9_results_state_overall |>
  dplyr::bind_rows(seqic_indicator_9_results_state_activations)

# state level - overall by age group
seqic_indicator_9_results_state_overall_age <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Age_Range"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |> # get the first list element, which houses the overall results
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
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = indicator) |>
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

# state level - by age group and trauma team activations
seqic_indicator_9_results_state_activations_age <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Age_Range"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |> # get the first list element, which houses the overall results
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

# union the state level tables
seqic_indicator_9_results_state_age <- seqic_indicator_9_results_state_overall_age |>
  dplyr::bind_rows(seqic_indicator_9_results_state_activations_age) |>
  dplyr::arrange(Year, Age_Range, Trauma_Team_Activated)

# Districts - overall
seqic_indicator_9_results_state_districts_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |> # get the first list element, which houses the overall results
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
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Service Area`)

# Districts - by trauma team activation status
seqic_indicator_9_results_state_districts_activation <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |> # get the second list element, which houses the overall results
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

# Union the district tibbles
seqic_indicator_9_results_state_districts <- seqic_indicator_9_results_state_districts_overall |>
  dplyr::bind_rows(seqic_indicator_9_results_state_districts_activation) |>
  dplyr::arrange(`Service Area`, Year, Trauma_Team_Activated)

# districts (wide)
seqic_indicator_9_results_state_districts_wide <- seqic_indicator_9_results_state_districts |>
  dplyr::filter(Year == 2024) |>
  dplyr::select(
    Year,
    `Service Area`,
    Trauma_Team_Activated,
    indicator,
    name,
    performance
  ) |>
  tidyr::pivot_wider(
    id_cols = c(Year, Trauma_Team_Activated, indicator, name),
    names_from = `Service Area`,
    values_from = performance
  )

# Level - overall
seqic_indicator_9_results_state_verification_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
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
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = Level_I_II)

# Level - by trauma team activation status
seqic_indicator_9_results_state_verification_activation <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
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

# Union the level tibbles
seqic_indicator_9_results_state_verification <- seqic_indicator_9_results_state_verification_overall |>
  dplyr::bind_rows(seqic_indicator_9_results_state_verification_activation) |>
  dplyr::arrange(`Level_I_II`, Year, Trauma_Team_Activated)

### Export ####

# state level reporting
export_state_data(
  x = seqic_indicator_9_results_state,
  subfolder = "9"
)

# state level by age reporting
export_state_data(
  x = seqic_indicator_9_results_state_age,
  subfolder = "9"
)

# district level reporting
export_state_data(
  x = seqic_indicator_9_results_state_districts,
  subfolder = "9"
)

# wide district level reporting
export_state_data(
  x = seqic_indicator_9_results_state_districts_wide,
  subfolder = "9"
)

# verification level reporting
export_state_data(
  x = seqic_indicator_9_results_state_verification,
  subfolder = "9"
)
