# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
gtl_pkg_options <-
  settings::options_manager(
    language = "en",
    country = "UK",
    theme = "pomological_red",
    mode = "light",
    opacity = 1,
    book_resources = "/",
    plot_standard_width = 63.5,
    plot_standard_height = 28.57,
    plot_full_width = 67.73,
    plot_full_height = 38.1
  )


#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or `[key]=[value]` pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{`language`}{The default language}
#'  \item{`country`}{The default country setting}
#'  \item{`theme`}{The default ggplot theme}
#'  \item{`mode`}{The default ggplot mode}
#'  \item{`opacity`}{The default opacity}
#' }
#'
#' @export
gtl_options <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  gtl_pkg_options(...)
}

#' Set language and country settings
#'
#' @param code A valid i18n code (example: fr_CH)
#'
#' @export
gtl_opt_set_i18n <- function(code) {
  if (!stringr::str_detect(code, "[a-z]{2}_[A-Z]{2}")) {
    stop("Code is not a valid i18n setup!")
  }
  gtl_pkg_options(language = stringr::str_split(code, "_")[[1]][[1]])
  gtl_pkg_options(country = stringr::str_split(code, "_")[[1]][[2]])
}

#' Return short language code
#'
#' @param valid A vector with valid language options
#'
#' @return A string
#' @export
gtl_opt_short_language <- function(valid = c("en", "fr")) {
  if(gtl_pkg_options("language") %in% valid) {
    gtl_pkg_options("language")
  } else {
    valid[[1]]
  }
}

#' Return long language name
#'
#' @return A string with the language name (lowercase)
#' @export
gtl_opt_long_language <- function() {
  ISOcodes::ISO_639_3 |>
    dplyr::filter(Part1 == gtl_pkg_options("language")) |>
    dplyr::pull(Name) |>
    stringr::str_to_lower()
}
