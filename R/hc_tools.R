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

  from <- stringr::str_extract(
    data$lvls,
    "[\\[)]([^,]*),\\W([^\\])]*)[)\\]]",
    group = 1
  )
  to <- stringr::str_extract(
    data$lvls,
    "[\\[)]([^,]*),\\W([^\\])]*)[)\\]]",
    group = 2
  )

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
  lvls <- breaks |>
    unique() |>
    sort()
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

  from <- as.integer(data$lvls)
  to <- as.integer(data$lvls)

  list(
    name = as.character(data$lvls),
    color = data$colors,
    from = from,
    to = to
  )
}


#' mapView options for highcharter objects
#' @param hc A `highchart` htmlwidget object.
#' @param ... Options as defined in <https://api.highcharts.com/highmaps/mapView>.
#' @export
gtl_hc_map_view <- function(hc, ...) {
  stopifnot(highcharter::is.highchart(hc))
  opts <- list(...)
  if (is.null(hc$x$hc_opts[["mapView"]])) {
    hc$x$hc_opts[["mapView"]] <- opts
  } else {
    hc$x$hc_opts[["mapView"]] <- utils::modifyList(
      hc$x$hc_opts[["mapView"]],
      opts
    )
  }
  hc
}
