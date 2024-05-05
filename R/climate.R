# Climate determination -----------------------------------------------------------------------

#' Determine the Köppen climate based on temperature,
#' precipitation and the latitude.
#'
#' @param temp A vector with temperatures in degree Celsius for each month
#' @param prec A vector with precipitations in millimeters for each month
#' @param lat The latitude in degrees
#'
#' @return A string with the Köppen climate determination
#' @export
#' @examples
#' # Based on climate data for Geneva
#' gtl_koppen_code(
#'   temp = c(2.1, 3, 6.8, 10.4, 14.8, 18.4, 20.5, 20.2, 15.8, 11.5, 6, 2.7),
#'   prec = c(70.8, 60.6, 56.9, 69, 75.7, 78.8, 83.2, 81.4, 94.9, 97.6, 90.3, 85.2),
#'   lat = 6
#' )
#'
gtl_koppen_code <- function(temp, prec, lat) {
  ptot <- sum(prec)
  tavg <- mean(temp)
  psummer <- gtl_calculate_psummer(prec, lat)
  pwinter <- gtl_calculate_pwinter(prec, lat)
  e_thresh <- gtl_calculate_threshold(tavg, ptot, psummer, pwinter)

  ## E climates
  if (length(which(temp < 10)) == 12) {
    return(gtl_koppen_e_climates(temp))
  }

  ## B climates
  if (e_thresh > ptot) {
    return(gtl_koppen_b_climates(tavg, ptot, e_thresh))
  }

  ## A climates
  if (length(which(temp >= 18)) == 12) {
    return(gtl_koppen_a_climates(prec, ptot, pwinter, psummer))
  }

  ## C climates
  if (length(which(temp >= -3)) == 12) {
    return(gtl_koppen_c_climates(temp, prec, pwinter, psummer))
  }

  ## D climates
  return(gtl_koppen_d_climates(temp, prec, pwinter, psummer))
}

#' Internal function to determine E climates
#'
#' @param temp Temperatures
#'
#' @return A string
#' @keywords internal
gtl_koppen_e_climates <- function(temp) {
  if (length(which(temp < 0)) == 12) {
    return("EF")
  } else {
    return("ET")
  }
}

#' Internal function to determine B climates
#'
#' @param tavg Average temperatures
#' @param ptot Total precipitation
#' @param e_thresh Evaporation threshold
#'
#' @return A string
#' @keywords internal
gtl_koppen_b_climates <- function(tavg, ptot, e_thresh) {
  clim <- ""

  if (ptot >= (e_thresh * 0.5)) {
    clim <- "BS"
  } else {
    clim <- "BW"
  }
  if (tavg > 18) clim <- glue::glue("{clim}h") else clim <- glue::glue("{clim}k")

  return(clim)
}

#' Internal function to determine A climates
#'
#' @param prec Precipitations
#' @param ptot Total precipitation
#' @param pwinter Winter precipitations
#' @param psummer Summer precipitations
#'
#' @return A string
#' @keywords internal
gtl_koppen_a_climates <- function(prec, ptot, pwinter, psummer) {
  clim <- ""

  if (length(which(prec >= 60)) == 12) {
    clim <- "Af"
  } else if (ptot >= 25 * (100 - min(prec))) {
    clim <- "Am"
  } else if (min(psummer) < min(pwinter)) {
    clim <- "As"
  } else {
    clim <- "Aw"
  }

  return(clim)
}

#' Internal function to determine B climates
#'
#' @param temp Temperatures
#' @param prec Precipitations
#' @param pwinter Winter precipitations
#' @param psummer Summer precipitations
#'
#' @return A string
#' @keywords internal
gtl_koppen_c_climates <- function(temp, prec, pwinter, psummer) {
  clim <- ""

  if (length(which(prec >= 30)) == 12) {
    clim <- "Cf"
  } else if (min(pwinter) <= (max(psummer) / 10)) {
    clim <- "Cw"
  } else if (min(psummer) <= (max(pwinter) / 3)) {
    clim <- "Cs"
  } else {
    clim <- "Cf"
  }
  ## C 3rd letter
  if (max(temp) >= 22) {
    clim <- glue::glue("{clim}a")
  } else if (length(which(temp >= 10)) >= 4) {
    clim <- glue::glue("{clim}b")
  } else {
    clim <- glue::glue("{clim}c")
  }

  return(clim)
}

#' Internal function to determine B climates
#'
#' @param temp Temperatures
#' @param prec Precipitations
#' @param pwinter Winter precipitations
#' @param psummer Summer precipitations
#'
#' @return A string
#' @keywords internal
gtl_koppen_d_climates <- function(temp, prec, pwinter, psummer) {
  if (length(which(prec >= 30)) == 12) {
    clim <- "Df"
  } else if (min(pwinter) <= (max(psummer) / 10)) {
    clim <- "Dw"
  } else if (min(psummer) <= (max(pwinter) / 3)) {
    clim <- "Ds"
  } else {
    clim <- "Df"
  }
  ## D 3rd letter
  if (min(temp) < -38) {
    clim <- glue::glue("{clim}d")
  } else if (max(temp) >= 22) {
    clim <- glue::glue("{clim}a")
  } else if (length(which(temp >= 10)) >= 4) {
    clim <- glue::glue("{clim}b")
  } else {
    clim <- glue::glue("{clim}c")
  }

  return(clim)
}

#' Internal function to determine the summer precipitation months
#'
#' @param prec A vector with precipitation for each month
#' @param lat The latitude
#'
#' @return A vector with the precipitation in the "summer" month as defined for Köppen climate classification
#' @keywords internal
gtl_calculate_psummer <- function(prec, lat) {
  dplyr::case_when(
    lat >= 0 ~ prec[4:9],
    TRUE ~ prec[c(1:3, 10:12)]
  )
}

#' Internal function to determine the summer precipitation months
#'
#' @param prec A vector with precipitation for each month
#' @param lat The latitude
#'
#' @return A vector with the precipitation in the "summer" month as defined for Köppen climate classification
#' @keywords internal
gtl_calculate_pwinter <- function(prec, lat) {
  dplyr::case_when(
    lat >= 0 ~ prec[c(1:3, 10:12)],
    TRUE ~ prec[4:9]
  )
}


#' Internal function to determine the Köppen threshold number
#'
#' @param tavg The annual average temperature
#' @param ptot The annual total precipitation
#' @param psummer A vector with the precipitation in summer months
#' @param pwinter A vector with the precipitation in winter months
#'
#' @return The threshold number for Köppen evaporation (for B climates)
#' @keywords internal
gtl_calculate_threshold <- function(tavg, ptot, psummer, pwinter) {
  pth <- (2 / 3) * ptot #nolint: object_usage_linter

  dplyr::case_when(
    sum(pwinter) > pth ~ 20 * tavg,
    sum(psummer) > pth ~ 20 * tavg + 280,
    TRUE ~ 20 * tavg + 140
  )
}
