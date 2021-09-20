#' Provide custom countrycode function
#'
#' @param country Select the custom dictionary, currently one of *switzerland* or *russia*
#' @inheritDotParams countrycode::countrycode
#'
#' @return A vector with the desired output
#' @export
#'
#' @md
admincode <- function(..., country = c("Switzerland", "Russia")) {
  country <- match.arg(country)

  dictionary <- get(paste0("adm1_", stringr::str_to_lower(country)))

  countrycode::countrycode(
    ...,
    custom_dict = dictionary
  )
}

#' Provide HMD country codes
#'
#' @return A dataframe with 2 columns and 47 rows
#' @export
gtl_hmd_codes <- function() {
  return(hmd_codes)
}
