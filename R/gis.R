#' Get a defined CRS definition for a given code
#'
#' A function that returns the corresponding CRS for different
#' codes.
#'
#' The current list of available codes is:
#' * eqearth
#' * equirec
#' * gallpeters
#' * goode
#' * hobodyer
#' * mercator
#' * robinson
#' * wintri
#'
#' @param code A string with the CRS identifier
#'
#' @return A CRS object
#' @export
#' @examples
#' gtl_crs_proj("eqearth")
#'
#' gtl_crs_proj("robinson")
#'
gtl_crs_proj <- function(code) {
  crs_proj |>
    dplyr::filter(crs == code) -> crs

  if(nrow(crs) != 1) stop("Could not find a matching CRS.")

  sf::st_crs(crs$proj4)
}

#' Get a defined proj4 string for a given country
#'
#' A function that returns an ideal CRS for a given
#' country.
#'
#' The current list of possible countries:
#' * Russia
#'
#' @param country A string with the country name
#'
#' @return A CRS object
#' @export
#'
#' @examples
#' gtl_crs_regional("Russia")
#'
gtl_crs_regional <- function(country) {
  crs_regional |>
    dplyr::filter(country == country) -> crs

  if(nrow(crs) != 1) stop("Could not find a matching CRS.")

  sf::st_crs(crs$proj4)
}

#' World Tissot Matrix
#'
#' @return A simple feature layer
#' @export
#'
#' @examples
#' gtl_gis_tissot_indicatrix()
gtl_gis_tissot_indicatrix <- function() {
  tissot_matrix
}
