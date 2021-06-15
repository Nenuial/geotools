#' Format numbers using scientific notation for html
#'
#' @param x Number to format
#'
#' @return A formatted expression
#' @export
gtl_sci_10_html <- function(x) {
  paste0(gsub("e\\+?", " · 10<sup>", scales::scientific_format()(x)), "</sup>")
}
