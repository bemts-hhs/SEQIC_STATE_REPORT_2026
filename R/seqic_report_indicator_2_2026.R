###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 2 Missing Injury Time
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 2 ####

# Districts
seqic_indicator_2_districts <- trauma_2021_2025 |>
  traumar::seqic_indicator_2(
    unique_incident_id = Unique_Incident_ID,
    level = Level,
    incident_time = Incident_Time,
    groups = c("Year", "Iowa HHS District"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "district") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level
seqic_indicator_2_level <- trauma_2021_2025 |>
  traumar::seqic_indicator_2(
    unique_incident_id = Unique_Incident_ID,
    level = Level,
    incident_time = Incident_Time,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

###_____________________________________________________________________________
### State, District, and Verification Level Performance Reporting
###_____________________________________________________________________________

# state level
seqic_indicator_2_results_state <- trauma_2021_2025 |>
  traumar::seqic_indicator_2(
    unique_incident_id = Unique_Incident_ID,
    level = Level,
    incident_time = Incident_Time,
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
      traumar::pretty_percent(variable = ., n_decimal = 1)
    )
  ))

## convert state-level into a Word friendly format ----
seqic_indicator_2_results_state_convert <- seqic_indicator_2_results_state |>
  dplyr::select(Year, indicator, numerator:tidyselect::last_col()) |>
  dplyr::mutate(
    dplyr::across(
      numerator:denominator,
      ~ prettyNum(x = ., big.mark = ",")
    ),
    performance = glue::glue("{performance} ({`lower ci`}-{`upper ci`})")
  ) |>
  dplyr::select(-tidyselect::matches("ci$")) |>
  dplyr::rename_with(~ stringr::str_to_title(.)) |>
  dplyr::rename("Performance (95% CI)" = Performance)

# state level - by age group
seqic_indicator_2_results_state_age <- trauma_2021_2025 |>
  traumar::seqic_indicator_2(
    unique_incident_id = Unique_Incident_ID,
    level = Level,
    incident_time = Incident_Time,
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
      traumar::pretty_percent(variable = ., n_decimal = 1)
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
seqic_indicator_2_results_state_districts <- trauma_2021_2025 |>
  traumar::seqic_indicator_2(
    unique_incident_id = Unique_Incident_ID,
    level = Level,
    incident_time = Incident_Time,
    groups = c("Year", "Iowa HHS District"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 1)
    )
  ))

# districts (wide)
seqic_indicator_2_results_state_districts_wide <- seqic_indicator_2_results_state_districts |>
  dplyr::select(Year, `Iowa HHS District`, indicator, name, performance) |>
  tidyr::pivot_wider(
    id_cols = c(Year, indicator, name),
    names_from = `Iowa HHS District`,
    values_from = performance
  )

# trauma center verification levels
seqic_indicator_2_results_state_verification <- trauma_2021_2025 |>
  traumar::seqic_indicator_2(
    unique_incident_id = Unique_Incident_ID,
    level = Level,
    incident_time = Incident_Time,
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
      traumar::pretty_percent(variable = ., n_decimal = 1)
    )
  ))

### Export ####

# state level reporting
export_state_data(
  x = seqic_indicator_2_results_state,
  subfolder = "2"
)

# state level by age reporting
export_state_data(
  x = seqic_indicator_2_results_state_age,
  subfolder = "2"
)

# district level reporting
export_state_data(
  x = seqic_indicator_2_results_state_districts,
  subfolder = "2"
)

# wide district level reporting
export_state_data(
  x = seqic_indicator_2_results_state_districts_wide,
  subfolder = "2"
)

# verification level reporting
export_state_data(
  x = seqic_indicator_2_results_state_verification,
  subfolder = "2"
)

# save the converted values to disk for use in the report ----
export_state_xlsx(
  data = seqic_indicator_2_results_state_convert,
  sheet_name = "indicator_2",
  column_fill = "E0A624",
  font_family = "Work Sans",
  inner_grid_color = "B9E1DA",
  performance_col_name = "performance",
  performance_fill_color = "F8EDD3",
  loop = FALSE,
  subfolder = "2"
)

# save the wide district performance table to disk as .xlsx ----
export_state_district_xlsx(
  data = seqic_indicator_2_results_state_districts_wide,
  sheet_name = "indicator_2_districts",
  remove_pattern = "indicator\\s\\d[a-z]?\\s-\\s",
  column_fill = "E0A624",
  font_family = "Work Sans",
  inner_grid_color = "B9E1DA",
  subfolder = "2"
)
