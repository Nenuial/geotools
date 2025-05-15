#' Check IDB API key
#'
#' Function to check if the IDB API key is correctly set.
#'
#' @seealso [idbr::idb_api_key()]
#'
#' @export
#' @return Returns an error if the IDB API key is not properly set
#' @examplesIf interactive()
#' # Not run: fails if the IDB API key isn't set
#'
#' gtl_chk_idb_api_key()
#'
gtl_chk_idb_api_key <- function() {
  if (Sys.getenv("IDB_API") != "") {
    api_key <- Sys.getenv("IDB_API")
  } else if (is.null(api_key)) {
    cli::cli_abort(c(
      "No IDB API key set.",
      "i" = "Supply a valid IDB API key using `idbr::idb_api_key()`"
    ))
  }
}

#' Check NOAA API key
#'
#' Function to check if the NOAA API key is correctly set.
#'
#' @export
#' @return Returns an error if the NOAA API key is not properly set
#' @examplesIf interactive()
#' # Not run: fails if the NOAA API key isn't set
#'
#' gtl_chk_rnoaa_api_key()
#'
gtl_chk_rnoaa_api_key <- function() {
  if (is.null(options("noaakey")$noaakey)) {
    cli::cli_abort(c(
      "No NOAA API key set.",
      "i" = "Supply a valid NOAA API key using `options(noaakey = 'API_KEY')`"
    ))
  }
}

#' Check API connexion
#'
#' @param call An API url
#'
#' @export
#' @return Returns an error if the API call fails
gtl_chk_api_call <- function(call) {
  tryCatch(
    httr2::request(call) |> httr2::req_perform(),
    error = function(e) {
      cli::cli_abort(c(
        "The API does not respond properly.",
        "i" = "Do you have an internet connection and an open proxy?",
        "x" = "Connection to {call} failed."
      ))
    }
  )
}

#' Download JSON response from API
#'
#' @param call An API url
#'
#' @export
#' @return JSON body
#' @examples
#' gtl_dwnl_api_json("https://dummyjson.com/products/1")
#'
gtl_dwnl_api_json <- function(call) {
  httr2::request(call) |>
    httr2::req_headers("User-Agent" = "Mozilla/5.0") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}
