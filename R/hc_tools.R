#' Return a list of color breaks for Higchart maps
#'
#' @param breaks The data with the breaks (using chopped data with [`santoku::santoku`])
#' @param palette A color palette that can take a numeric argument
#'
#' @return A list of lists to use in colorAxis' dataClasses argument
#' @export
#' @examples
#' data <- seq(1, 20)
#' data_cut <- santoku::chop(data, breaks = c(5, 10, 15))
#' gtl_hc_color_axis(data_cut, rainbow)
gtl_hc_color_axis <- function(breaks, palette) {
  lvls <- levels(breaks)
  colors <- palette(length(lvls))

  tibble::tibble(
    lvls = lvls,
    colors = colors
  ) |>
    purrr::pmap(gtl_hc_color_list)
}

#' Function to build the highchart colorAxis list
#'
#' @param ... lvls and colors
#'
#' @keywords internal
gtl_hc_color_list <- function(...) {
  data <- list(...)

  from <- stringr::str_extract(data$lvls, "[\\[)]([^,]*),\\W([^\\])]*)[)\\]]", group = 1)
  to <- stringr::str_extract(data$lvls, "[\\[)]([^,]*),\\W([^\\])]*)[)\\]]", group = 2)

  list(
    color = data$colors,
    from = from,
    to = to
  )
}

#' Return a list of color breaks for Higchart maps
#'
#' @param breaks The data with the breaks (factors !)
#' @param palette A color palette that can take a numeric argument
#'
#' @return A list of lists to use in colorAxis' dataClasses argument
#' @export
gtl_hc_discrete_color_axis <- function(breaks, palette) {
  lvls <- levels(breaks)
  colors <- palette(length(lvls))

  tibble::tibble(
    lvls = lvls,
    colors = colors
  ) |>
    purrr::pmap(gtl_hc_discrete_color_list)
}

#' Function to build the highchart colorAxis list
#'
#' @param ... lvls and colors
#'
#' @keywords internal
gtl_hc_discrete_color_list <- function(...) {
  data <- list(...)

  from <- as.character(data$lvls)
  to <- as.character(data$lvls)

  list(
    color = data$colors,
    from = from,
    to = to
  )
}
