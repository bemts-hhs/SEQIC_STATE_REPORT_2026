###___________________________________________________________________________
# Creation of a Map of Iowa with Counties colored by District
###___________________________________________________________________________

# get Iowa county shapefile data from tigris ----
iowa_counties <- tigris::counties(state = "Iowa", year = 2025)

# join district data to the county shapefile ----
iowa_county_sf <- iowa_counties |>
  dplyr::left_join(
    locations |>
      dplyr::select(`County FIPS`, `Region: BH`) |>
      dplyr::mutate(`County FIPS` = as.character(`County FIPS`)),
    by = dplyr::join_by(GEOID == `County FIPS`)
  ) |>
  dplyr::rename(district = `Region: BH`) |>
  dplyr::mutate(district = as.character(district))

# target counties for district labels
label_counties <- c(
  "Plymouth",
  "Kossuth",
  "Chickasaw",
  "Jones",
  "Keokuk",
  "Polk",
  "Cass"
)

# create the county map of Iowa with counties colored by HHS district ----
iowa_county_map <- iowa_county_sf |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(
    mapping = ggplot2::aes(fill = district)
  ) +
  ggplot2::geom_sf_text(
    ggplot2::aes(label = ifelse(NAME %in% label_counties, district, "")),
    size = 12,
    family = "Work Sans",
    fontface = "bold"
  ) +
  paletteer::scale_fill_paletteer_d(
    palette = "ggthemes::colorblind",
    direction = -1
  ) +
  ggplot2::labs(fill = "Iowa HHS District") +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.title = ggplot2::element_text(size = 20, family = "Work Sans"),
    legend.text = ggplot2::element_text(size = 20, family = "Work Sans")
  )

# save the map to disk ----
ggplot2::ggsave(
  filename = "iowa_county_map.png",
  plot = iowa_county_map,
  path = state_output_folder
)
