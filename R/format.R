#' Format numbers using scientific notation for html
#'
#' @param x Number to format
#'
#' @return A formatted expression
#' @export
#' @examples
#' gtl_sci_10_html(10e6)
#' gtl_sci_10_html(10e9)
#'
gtl_sci_10_html <- function(x) {
  paste0(gsub("e\\+?", " \u00b7 10<sup>", scales::scientific_format()(x)), "</sup>")
}

#' Format full date
#'
#' This function formats a date in a long format (see return value). The format
#' takes the current language setting into account (see [`gtl_opt_set_i18n()`]).
#'
#' @seealso [gtl_opt_set_i18n()]
#'
#' @param date A date in YYYY-MM-DD format
#'
#' @return A formated `day month year` (%d %B %Y) date
#' @export
#' @examples
#' # Set en english locale
#' gtl_opt_set_i18n("en_US")
#' gtl_full_date("2020-02-20")
#'
#' # Set a french locale
#' gtl_opt_set_i18n("fr_FR")
#' gtl_full_date("2020-02-20")
#'
gtl_full_date <- function(date) {
  withr::with_locale(
    new = c("LC_TIME" = paste0(gtl_options("language"), "_", gtl_options("country"), ".UTF-8")),
    format(clock::date_parse(date), "%d %B %Y")
  )
}
