#' Format full date
#'
#' @param date A date in YYYY-MM-DD format
#'
#' @return A formated `day month year` (%d %B %Y) date
#' @export
gtl_full_date <- function(date) {
  withr::with_locale(
    new = c("LC_TIME" = paste0(gtl_options("language"), "_", gtl_options("country"), ".UTF-8")),
    format(clock::date_parse(date), "%d %B %Y")
  )
}
