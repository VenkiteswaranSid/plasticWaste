#' GDP and Plastics Summary Table
#'
#' Creates a summary table of the countries with the highest plastic waste,
#' displaying each country's total plastic waste and average GDP per capita.
#'
#' @param top_x_countries Integer. Number of countries to display.
#'
#' @return A data frame containing the top countries by total plastic waste,
#' along with their total plastic waste and average GDP per capita.#'
#' @export
#'
#' @examples
#' gdp_plastics_table(5)
gdp_plastics_table <- function(top_x_countries = 10) {

  stopifnot(is.numeric(top_x_countries),
            length(top_x_countries) == 1,
            top_x_countries > 0)

  gdp_plastics <- function(input_country){

    country_data <- plastics_top |>
      filter(country == input_country) |>
      distinct(year, grand_total, gdp_per_capita_nominal)

    tibble(
      plastic_total = sum(country_data$grand_total, na.rm = TRUE),
      avg_gdp_per_capita_nominal = mean(country_data$gdp_per_capita_nominal, na.rm = TRUE)
    )
  }

  plastics_top <- make_plastics_top()

  plastics_top |>
    distinct(country) |>
    filter(!country %in% c("EMPTY", "Taiwan_ Republic of China (ROC)", "Nigeria")) |>
    mutate(
      result = map(country, gdp_plastics),
      country = recode(country, "NIGERIA" = "Nigeria", "ECUADOR" = "Ecuador")
    ) |>
    unnest(result) |>
    slice_max(plastic_total, n = top_x_countries)
}
