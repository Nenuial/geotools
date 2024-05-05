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
#' @examples
#' # Retrieve default options
#' gtl_options("language")
#' gtl_options("plot_standard_width")
#'
#' # Change a setting and retrieve it
#' gtl_options(plot_standard_width = 25)
#' gtl_options("plot_standard_width")
gtl_options <- function(...) {
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  gtl_pkg_options(...)
}

#' Set language and country settings
#'
#' This function allows setting the default language and country
#' settings using a valid i18n code.
#'
#' @param code A valid i18n code (example: fr_CH)
#'
#' @export
#' @examples
#' # Swiss french locale
#' gtl_opt_set_i18n("fr_CH")
#' gtl_options("language")
#' gtl_options("country")
#'
#' # American english locale
#' gtl_opt_set_i18n("en_US")
#' gtl_options("language")
#' gtl_options("country")
gtl_opt_set_i18n <- function(code) {
  if (!stringr::str_detect(code, "[a-z]{2}_[A-Z]{2}")) {
    stop("Code is not a valid i18n setup!")
  }
  gtl_pkg_options(language = stringr::str_split(code, "_")[[1]][[1]])
  gtl_pkg_options(country = stringr::str_split(code, "_")[[1]][[2]])
}

#' Return short language code
#'
#' Function that returns the short language code currently set
#' if it is in the list of *valid* options. If the current language
#' isn't in the list of *valid* options, the first element of the
#' valid elements is returned.
#'
#' @param valid A vector with valid language options
#'
#' @seealso [gtl_opt_set_i18n()]
#'
#' @return A string
#' @export
#' @examples
#' gtl_opt_set_i18n("fr_CH")
#' gtl_opt_short_language(valid = c("de", "fr"))
#'
#' # With the current local not among the valid options
#' gtl_opt_set_i18n("en_US")
#' gtl_opt_short_language(valid = c("de", "fr"))
gtl_opt_short_language <- function(valid = c("en", "fr")) {
  if (gtl_pkg_options("language") %in% valid) {
    gtl_pkg_options("language")
  } else {
    valid[[1]]
  }
}

#' Return long language name
#'
#' Function to get the current language setting in full.
#'
#' @seealso [gtl_opt_set_i18n()]
#'
#' @return A string with the language name (lowercase)
#' @export
#' @examples
#' # With a french locale
#' gtl_opt_set_i18n("fr_CH")
#' gtl_opt_long_language()
#'
#' # With an english locale
#' gtl_opt_set_i18n("en_US")
#' gtl_opt_long_language()
gtl_opt_long_language <- function() {
  ISOcodes::ISO_639_3 |>
    dplyr::filter(Part1 == gtl_pkg_options("language")) |>
    dplyr::pull(Name) |>
    stringr::str_to_lower()
}
