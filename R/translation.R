#' Translate function
#'
#' @param english English string
#' @param french French string
#'
#' @description
#' `translate_enfr()` was renamed to `gtl_translate_enfr()` to create a more
#' consistent API.
#'
#' @return String depending on package language option
#' @keywords internal
#' @export
translate_enfr <- function(english, french) {
  lifecycle::deprecate_warn("1.0.0", "translate_enfr()", "gtl_translate_enfr()")

  gtl_translate_enfr(english, french)
}

#' Translate function
#'
#' @param english English string
#' @param french French string
#'
#' A function that returns the english or french argument depending
#' on the current language setting.
#'
#' @seealso [gtl_opt_set_i18n()]
#'
#' @return String depending on package language option
#' @export
#' @examples
#' gtl_opt_set_i18n("fr_CH") # Set language to french
#' gtl_translate_enfr("Hello world!", "Bonjour le monde !")
#'
#' gtl_opt_set_i18n("en_US") # Set language to english
#' gtl_translate_enfr("Hello world!", "Bonjour le monde !")
gtl_translate_enfr <- function(english, french) {
  language <- gtl_pkg_options("language") #nolint: object_usage_linter

  dplyr::case_when(
    language == "en" ~ english,
    language == "fr" ~ french,
    TRUE ~ english
  )
}

#' Creates translation function for named string list
#'
#' @param dictionary A list of translations
#'
#' @description
#' `translator()` was renamed to `gtl_translator()` to create a more
#' consistent API.
#'
#' @return A function that translates
#' @keywords internal
#' @export
translator <- function(dictionary) {
  lifecycle::deprecate_warn("1.0.0", "translator()", "gtl_translator()")

  gtl_translator(dictionary)
}

#' Creates translation function for named string list
#'
#' @param dictionary A list of translations
#'
#' @return A function that translates
#' @export
#' @examples
#' translations <- list(
#'   "maison" = "house",
#'   "chapeau" = "hat",
#'   "chat" = "cat"
#' )
#'
#' translate_to_english <- gtl_translator(translations)
#' translate_to_english("maison")
#' translate_to_english("chat")
#'
# @TODO Rewrite this function in a better way
gtl_translator <- function(dictionary) {
  function(word) {
    purrr::map_chr(
      word,
      ~ ifelse(exists(.x, where = dictionary),
        return(dictionary[[.x]]),
        return(.x)
      )
    )
  }
}
