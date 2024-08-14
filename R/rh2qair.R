#' Convert relative humidity to absolute humidity
#'
#' @description function to convert humidity to absolute humidity using tetens formula, assuming standart atmosphere conditions.
#'
#' @param rh vector of relative humidity (in percentage)
#' @param temp vector of temperature (in celcius)
#'
#' @export
#'

rh2qair <- function(rh, temp) {
  temp  = temp + 273.15
  rh    = rh / 100
  qair <- rh * 2.541e6 * exp(-5415.0 / temp) * 18/29
  return(qair)
}
