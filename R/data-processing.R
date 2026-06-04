make_capitals <- function(country_data) {
  country_data |>
    dplyr::select(c(1:4)) |>
    dplyr::rename(
      country = CountryName,
      capital_city = CapitalName,
      capital_lat = CapitalLatitude,
      capital_long = CapitalLongitude
    )
}

make_air_vs_plastic <- function(plastics, capitals, air_data) {
  pollution_data <- dplyr::left_join(plastics, capitals, by = "country")
  pollution_data <- dplyr::left_join(pollution_data, air_data, by = "country")

  pollution_data |>
    dplyr::filter(!parent_company %in% c("Grand Total", "Unbranded", "null", "NULL")) |>
    dplyr::group_by(country) |>
    dplyr::summarize(
      total_plastic = sum(grand_total, na.rm = TRUE),
      total_volunteers = sum(volunteers, na.rm = TRUE),
      avg_aqi = mean(overall_aqi, na.rm = TRUE),
      capital_city = dplyr::first(capital_city),
      capital_lat = dplyr::first(capital_lat),
      capital_long = dplyr::first(capital_long),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(avg_aqi), total_plastic > 0) |>
    dplyr::arrange(dplyr::desc(total_plastic))
}

make_air_vs_plastic_clean <- function(air_vs_plastic) {
  Q1 <- stats::quantile(air_vs_plastic$total_plastic, 0.25)
  Q3 <- stats::quantile(air_vs_plastic$total_plastic, 0.75)
  IQR <- Q3 - Q1

  air_vs_plastic |>
    dplyr::filter(
      total_plastic >= Q1 - 1.5 * IQR,
      total_plastic <= Q3 + 1.5 * IQR
    )
}

make_combined <- function(air_vs_plastic, fertility) {
  fertility_ph <- fertility |>
    dplyr::filter(`Country Name` == "Philippines") |>
    dplyr::rename(country = `Country Name`)

  dplyr::right_join(air_vs_plastic, fertility_ph, by = "country") |>
    dplyr::filter(country == "Philippines") |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("19") | tidyselect::starts_with("20"),
      names_to = "year",
      values_to = "fertility_rate"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::filter(!is.na(fertility_rate))
}






