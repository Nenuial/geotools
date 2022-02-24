#' Determine the Köppen climate based on temperature,
#' precipitation and the latitude.
#'
#' @param temp A vector with temperatures for each month
#' @param prec A vector with precipitations for each month
#' @param lat The latitude
#'
#' @return A string with the Köppen climate determination
#' @export
gtl_koppen_code <- function(temp, prec, lat) {
  clim <- ""
  ptot <- sum(prec)
  tavg <- mean(temp)
  psummer <- gtl_calculate_psummer(prec,lat)
  pwinter <- gtl_calculate_pwinter(prec,lat)
  e_thresh <- gtl_calculate_threshold(tavg, ptot, psummer, pwinter)

  ## E climates
  if(length(which(temp < 10)) == 12) {
    if(length(which(temp < 0)) == 12) {
      clim <- "EF"
    } else{
      clim <- "ET"
    }
  } else {
    ## B climates
    if(e_thresh > ptot) {
      if(ptot>=(e_thresh*0.5)) {
        clim <- "BS"
      } else {
        clim <- "BW"
      }
      if(tavg > 18) clim <- glue::glue("{clim}h") else clim <- glue::glue("{clim}k")
    } else {
      ## A climates
      if(length(which(temp >= 18)) == 12) {
        p_thresh <- 100-(ptot/25)
        if(length(which(prec >= 60)) == 12) {
          clim <- "Af"
        } else if(ptot >= 25*(100-min(prec))) {
          clim <- "Am"
        } else if(min(psummer) < min(pwinter)) {
          clim <- "As"
        } else {
          clim <- "Aw"
        }
      } else {
        ## C climates
        if(length(which(temp >= -3)) == 12) {
          if(length(which(prec >= 30)) == 12) {
            clim <- "Cf"
          } else if(min(pwinter) <= (max(psummer)/10)) {
            clim <- "Cw"
          } else if(min(psummer) <= (max(pwinter)/3)) {
            clim <- "Cs"
          } else {
            clim <- "Cf"
          }
          ## C 3rd letter
          if(max(temp) >= 22) {
            clim <- glue::glue("{clim}a")
          } else if(length(which(temp >= 10)) >= 4) {
            clim <- glue::glue("{clim}b")
          } else {
            clim <- glue::glue("{clim}c")
          }
        } else {
          ## D climates
          if(length(which(prec >= 30)) == 12) {
            clim <- "Df"
          } else if(min(pwinter) <= (max(psummer)/10)) {
            clim <- "Dw"
          } else if(min(psummer) <= (max(pwinter)/3)) {
            clim <- "Ds"
          } else {
            clim <- "Df"
          }
          ## D 3rd letter
          if(min(temp) < -38) {
            clim <- glue::glue("{clim}d")
          } else if(max(temp) >= 22) {
            clim <- glue::glue("{clim}a")
          } else if(length(which(temp >= 10)) >= 4) {
            clim <- glue::glue("{clim}b")
          } else {
            clim <- glue::glue("{clim}c")
          }
        }
      }
    }
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
  dplyr::case_when(lat >= 0 ~ prec[4:9],
                   TRUE ~ prec[c(1:3,10:12)])
}

#' Internal function to determine the summer precipitation months
#'
#' @param prec A vector with precipitation for each month
#' @param lat The latitude
#'
#' @return A vector with the precipitation in the "summer" month as defined for Köppen climate classification
#' @keywords internal
gtl_calculate_pwinter <- function(prec, lat) {
  dplyr::case_when(lat >= 0 ~ prec[c(1:3,10:12)],
                   TRUE ~ prec[4:9])
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
  pth <- (2/3)*ptot
  dplyr::case_when(sum(pwinter) > pth ~ 20*tavg,
                   sum(psummer) > pth ~ 20*tavg+280,
                   TRUE               ~ 20*tavg+140)
}