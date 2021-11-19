#' @title PLot latitude and longitude
#'
#' @description function to plot cool latitude and longitude.
#' labels for latitude range from 180ºW to 0 to 180ºE and longitude from 90ºS to 0 to 90ºN.
#'
#' @param int interval in degrees
#' @param side side to plot, see axis
#' @param lmin condition (TRUE or FALSE) for combine x and y or return only x
#' @param lmax message in case of bind is false (no binding)
#' @param ... additional arguments passed to axis function
#'
#' @examples
#' library(raster)
#' br <- shapefile(paste0(system.file("extdata",package="hackWRF"),"/BR_states.shp"),verbose=FALSE)
#' plot(br)
#' box()
#' latitude()
#' longitude()
#' grid()
#'
#' @describeIn plot cool latitude
#' @export
latitude <- function(int = 10, side = 1,lmin = -180, lmax = 180, ...){
  vet_lat <- seq(lmin,lmax,by = int)
  lab_lat <- c(paste0(seq(-lmin,int,by=-int),"\u00baW"),'0',
               paste0(seq(int,lmax,by=int),"\u00baE"))

  axis(side,at = vet_lat,labels = lab_lat, ...)
}
#' @describeIn plot cool longitude
#' @export
longitude <- function(int = 10,side = 2,lmin = -90, lmax = 90, ...){
  vet_lon <- seq(lmin,lmax,by = int)
  lab_lon <- c(paste0(seq(-lmin,int,by=-int),"\u00baS"),'0',
               paste0(seq(int,lmax,by=int),"\u00baN"))

  axis(side,at = vet_lon,labels = lab_lon, ...)
}
