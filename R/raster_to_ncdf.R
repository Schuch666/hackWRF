#' Convert a raster or brick to a array for WRF-Chem
#'
#' @description Conversion a raster or brick to a array for WRF-Chem model
#'
#' @param r input raster
#' @param na_value value for missing values
#'
#' @return array
#'
#' @import raster
#'
#' @examples
#'
#' r <- raster::raster(paste0(system.file("extdata", package = "hackWRF"),
#'                            "/wrf.day1.o3.nc"))
#' a <- raster_to_ncdf(r)
#'
#' @export
#'
raster_to_ncdf <- function(r,na_value = 0){
  N_times <- dim(r)[3]
  a       <- array(na_value,c(dim(r)[2],dim(r)[1],N_times))
  for(i in 1:N_times){
    a[,,i] <- as.matrix(t(raster::flip(r[[i]],2)))
  }
  return(a)
}
