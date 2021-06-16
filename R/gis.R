#' Get a defined CRS for a given code
#'
#' @param country A string with the country name
#'
#' @return A crs object
#' @export
gtl_crs_proj <- function(code) {
  crs_proj %>%
    dplyr::filter(crs == code) -> crs

  if(nrow(crs) != 1) stop("Could not find a matching CRS.")

  sf::st_crs(crs$proj4)
}

#' Get a defined CRS for a given country
#'
#' @param country A string with the country name
#'
#' @return A crs object
#' @export
gtl_crs_regional <- function(country) {
  crs_regional |>
    dplyr::filter(country == country) -> crs

  if(nrow(crs) != 1) stop("Could not find a matching CRS.")

  sf::st_crs(crs$proj4)
}
