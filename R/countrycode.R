#' Provide custom countrycode function
#'
#' @param country Select the custom dictionary, currently one of *China*, *Switzerland* or *Russia*
#' @inheritDotParams countrycode::countrycode
#'
#' @return A vector with the desired output
#' @export
#' @examples
#' tibble::tribble(
#'  ~canton, ~population,
#'  "Neuchâtel", 176496,
#'  "Vaud", 805098,
#'  "Genève", 504128
#' ) -> population
#'
#' population |>
#'  dplyr::mutate(iso = gtl_admin_code(
#'    sourcevar = canton,
#'    origin = "canton.name.regex",
#'    destination = "iso",
#'    origin_regex = TRUE,
#'    country = "Switzerland"
#'  ))
gtl_admin_code <- function(..., country = c("China", "Switzerland", "Russia")) {
  country <- match.arg(country)

  dictionary <- get(paste0("adm1_", stringr::str_to_lower(country)))

  countrycode::countrycode(
    ...,
    custom_dict = dictionary
  )
}

#' Provide custom countrycode function
#'
#' @param country Select the custom dictionary, currently one of *China*, *Switzerland* or *Russia*
#' @inheritDotParams countrycode::countrycode
#'
#' @description
#' `admincode()` was renamed to `gtl_admin_code()` to create a more
#' consistent API.
#'
#' @return A vector with the desired output
#' @export
#' @keywords internal
admincode <- function(..., country = c("China", "Switzerland", "Russia")) {
  lifecycle::deprecate_warn("1.0.0", "admincode()", "gtl_admin_code()")

  gtl_admin_code(..., country)
}

#' Provide HMD country codes
#'
#' This function returns a dataframe with the 47 countries for which
#' the [Human Mortality Database](https://www.mortality.org) provides
#' data. For each country there is a `name` and a `code`.
#'
#' @return A dataframe with 2 columns and 47 rows
#' @export
#' @examples
#' gtl_hmd_codes()
#'
gtl_hmd_codes <- function() {
  return(hmd_codes)
}
