#' Provide custom countrycode function
#'
#' @param country Select the custom dictionary, currently one of *China*, *Switzerland* or *Russia*
#' @inheritDotParams countrycode::countrycode
#'
#' @return A vector with the desired output
#' @export
#' @examples
#' tibble::tribble(
#'   ~canton, ~population,
#'   "Neuchâtel", 176496,
#'   "Vaud", 805098,
#'   "Genève", 504128
#' ) -> population
#'
#' population |>
#'   dplyr::mutate(iso = gtl_admin_code(
#'     sourcevar = canton,
#'     origin = "canton.name.regex",
#'     destination = "iso",
#'     origin_regex = TRUE,
#'     country = "Switzerland"
#'   ))
gtl_admin_code <- function(..., country = c("China", "Switzerland", "Russia")) {
  country <- match.arg(country)

  dictionary <- get(paste0("adm1_", stringr::str_to_lower(country)))

  countrycode::countrycode(
    ...,
    custom_dict = dictionary
  )
}

#' Get Swiss canton FSO ids
#'
#' This is a shortcut for Swiss canton codes based on the
#' canton name.
#'
#' @param canton_name A vector of Swiss canton names
#'
#' @return A vector of Swiss canton FSO ids
#' @export
#' @examples
#' gtl_swiss_canton_id(c("Jura", "Genève", "Neuchâtel"))
#'
gtl_swiss_canton_id <- function(canton_name) {
  gtl_admin_code(
    sourcevar = canton_name,
    origin = "canton.name.regex",
    destination = "fso.number",
    origin_regex = TRUE,
    country = "Switzerland"
  )
}

#' Get Swiss canton abbreviations
#'
#' This is a shortcut for Swiss canton abbreviation based
#' on the canton name.
#'
#' @param canton_name A vector of Swiss canton names
#'
#' @return A vector of Swiss canton abbreviations
#' @export
#' @examples
#' gtl_swiss_canton_abbr(c("Jura", "Genève", "Neuchâtel"))
#'
gtl_swiss_canton_abbr <- function(canton_name) {
  gtl_admin_code(
    sourcevar = canton_name,
    origin = "canton.name.regex",
    destination = "abbreviation",
    origin_regex = TRUE,
    country = "Switzerland"
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
  lifecycle::deprecate_stop("1.1.0", "admincode()", "gtl_admin_code()")

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

#' World Bank country list
#'
#' This function returns a list of World Bank countries
#' for a choice selector in a Shiny app.
#'
#' @export
#' @examples
#' gtl_country_list_wb()
#'
gtl_country_list_wb <- function() {
  df <- wbstats::wb_countries()

  df |>
    dplyr::filter(region != "Aggregates") -> df

  list <- as.list(df$iso3c)
  names(list) <- df$country

  return(list)
}

#' NOAA country list
#'
#' This function returns a list of World Bank countries
#' for a choice selector in a Shiny app.
#'
#' @export
#' @examplesIf interactive()
#' # Not run: need the NCDC city database
#' gtl_country_list_noaa()
#'
gtl_country_list_noaa <- function() {
  cachedir <- rappdirs::user_cache_dir("geodata")
  filename <- file.path(cachedir, "ncdc_cities.RData")
  if (!file.exists(filename)) update_city_list()

  df <- readRDS(filename)

  df |>
    dplyr::select(iso, country) |>
    unique() |>
    dplyr::arrange(country) -> df

  list <- as.list(df$iso)
  names(list) <- df$country

  return(list)
}
