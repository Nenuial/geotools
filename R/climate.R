# Climate determination -----------------------------------------------------------------------

#' Determine the Köppen climate based on temperature and precipitation
#'
#' The Köppen-Geiger classification as in Beck et al. (2018, 2023) after Peel
#' et al. (2007), the same as the GeoResources climate diagrams: B (arid) is
#' tested first, summer is the warmer half-year (April–September or
#' October–March), and C and D are separated at 0 °C (`cd = -3` for Köppen's
#' original -3 °C).
#'
#' @param temp A vector with temperatures in degree Celsius for each month
#' @param prec A vector with precipitations in millimeters for each month
#' @param lat Unused: the summer half-year is found from the temperatures.
#'   Kept for compatibility.
#' @param cd The temperature of the coldest month separating C from D climates
#'
#' @return A string with the Köppen climate determination
#' @export
#' @examples
#' # Based on climate data for Geneva
#' gtl_koppen_code(
#'   temp = c(2.1, 3, 6.8, 10.4, 14.8, 18.4, 20.5, 20.2, 15.8, 11.5, 6, 2.7),
#'   prec = c(70.8, 60.6, 56.9, 69, 75.7, 78.8, 83.2, 81.4, 94.9, 97.6, 90.3, 85.2)
#' )
#'
gtl_koppen_code <- function(temp, prec, lat = NULL, cd = 0) {
  stopifnot(length(temp) == 12, length(prec) == 12)

  mat <- mean(temp)
  map <- sum(prec)
  thot <- max(temp)
  tcold <- min(temp)

  # summer: the warmer half-year
  amjjas <- 4:9
  ondjfm <- c(10:12, 1:3)
  north <- sum(temp[amjjas]) >= sum(temp[ondjfm])
  psummer <- if (north) prec[amjjas] else prec[ondjfm]
  pwinter <- if (north) prec[ondjfm] else prec[amjjas]

  ## B climates
  pth <- gtl_calculate_threshold(mat, map, psummer, pwinter)
  if (map < 10 * pth) {
    return(paste0("B", if (map < 5 * pth) "W" else "S", if (mat >= 18) "h" else "k"))
  }

  ## E climates
  if (thot <= 10) {
    return(if (thot <= 0) "EF" else "ET")
  }

  ## A climates
  if (tcold >= 18) {
    if (min(prec) >= 60) {
      return("Af")
    }
    return(if (min(prec) >= 100 - map / 25) "Am" else "Aw")
  }

  ## C and D climates
  group <- if (tcold > cd) "C" else "D"
  paste0(group, gtl_koppen_precipitation(psummer, pwinter), gtl_koppen_heat(temp, group))
}

#' Internal function to determine the precipitation regime of C and D climates
#'
#' @param psummer Precipitations of the summer half-year
#' @param pwinter Precipitations of the winter half-year
#'
#' @return "s" (dry summer), "w" (dry winter) or "f" (no dry season)
#' @keywords internal
gtl_koppen_precipitation <- function(psummer, pwinter) {
  dry_summer <- min(psummer) < 40 && min(psummer) < max(pwinter) / 3
  dry_winter <- min(pwinter) < max(psummer) / 10

  if (dry_summer && dry_winter) {
    return(if (sum(psummer) < sum(pwinter)) "s" else "w")
  }
  if (dry_summer) {
    return("s")
  }
  if (dry_winter) {
    return("w")
  }
  "f"
}

#' Internal function to determine the summer heat of C and D climates
#'
#' @param temp Temperatures
#' @param group "C" or "D"
#'
#' @return "a", "b", "c" or "d"
#' @keywords internal
gtl_koppen_heat <- function(temp, group) {
  if (max(temp) >= 22) {
    return("a")
  }
  if (sum(temp > 10) >= 4) {
    return("b")
  }
  if (group == "D" && min(temp) < -38) {
    return("d")
  }
  "c"
}

#' Internal function to determine the Köppen aridity threshold
#'
#' @param mat The mean annual temperature
#' @param map The mean annual precipitation
#' @param psummer A vector with the precipitation in summer months
#' @param pwinter A vector with the precipitation in winter months
#'
#' @return The aridity threshold in mm (for B climates)
#' @keywords internal
gtl_calculate_threshold <- function(mat, map, psummer, pwinter) {
  if (sum(pwinter) > 0.7 * map) {
    return(2 * mat)
  }
  if (sum(psummer) > 0.7 * map) {
    return(2 * mat + 28)
  }
  2 * mat + 14
}
