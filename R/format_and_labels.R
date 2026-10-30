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

#' Format numbers in the current language
#'
#' Label functions for ggplot2 scales (or for any vector of numbers) that
#' follow the current language setting (see [`gtl_opt_set_i18n()`]): an
#' apostrophe between thousands in every language (60'000), a decimal comma in
#' French and German (2,5) and a space before the percent sign in French and
#' German (2,5 %). The language is read when the numbers are formatted.
#'
#' @seealso [gtl_opt_set_i18n()]
#'
#' @param ... Arguments passed on to [`scales::number()`] or
#'   [`scales::percent()`] (`accuracy`, `scale`, `suffix`...)
#'
#' @return A function that formats numbers
#' @export
#' @examples
#' gtl_opt_set_i18n("fr_CH")
#' gtl_label_number()(c(60000, 2.5))
#' gtl_label_percent(accuracy = 0.1)(0.025)
#'
#' gtl_opt_set_i18n("en_US")
#' gtl_label_number()(c(60000, 2.5))
#' gtl_label_percent(accuracy = 0.1)(0.025)
gtl_label_number <- function(...) {
  function(x) {
    do.call(scales::number, c(list(x), gtl_number_marks(list(...))))
  }
}

#' @rdname gtl_label_number
#' @export
gtl_label_percent <- function(...) {
  function(x) {
    marks <- gtl_number_marks(list(...))
    if (is.null(marks$suffix)) {
      marks$suffix <- if (gtl_pkg_options("language") %in% c("fr", "de")) "\u00a0%" else "%"
    }
    do.call(scales::percent, c(list(x), marks))
  }
}

# The marks of the current language, under the arguments given
gtl_number_marks <- function(args) {
  marks <- list(
    big.mark = "'",
    decimal.mark = if (gtl_pkg_options("language") %in% c("fr", "de")) "," else "."
  )
  marks[names(args)] <- args
  marks
}

#' Reformat santoku chop dash labels
#'
#' This function can be used to reformat the levels produced
#' by the [`santoku::lbl_dash()`] function.
#'
#' @seealso [santoku::lbl_dash()]
#'
#' @param x A string vector of labels
#'
#' @return A string vector with clean labels
#' @export
#' @examples
#' data <- seq(1, 20)
#' data_cut <- santoku::chop(data,
#'   breaks = c(5, 7, 13),
#'   labels = santoku::lbl_dash(),
#'   extend = TRUE, drop = FALSE
#' )
#' forcats::fct_relabel(data_cut, gtl_relabel_dash)
#'
gtl_relabel_dash <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "-Inf") ~ stringr::str_replace(x, "-Inf\u2014(.*)", "< \\1"),
    stringr::str_detect(x, "Inf") ~ stringr::str_replace(x, "(.*?)\u2014Inf", "> \\1"),
    TRUE ~ x
  )
}
