#' project emissions using growth factors.
#'
#' @param r raster object
#' @param shape sf object from a shapefile containing 5 regions
#' @param factor numeric vector with 5 growth factors
#' @param background growth factors to be used outside the domain (default is 1)
#'
#' @import sf raster
#'
#' @examples
#' regions <- sf::read_sf(paste0(system.file("extdata",package="hackWRF"),"/BR_regions.shp"))
#' factors <- c(1.1,1.5,2,1.5,0.75)
#' # using XLAT only for test purposis
#' x  <- eixport::wrf_raster(paste0(system.file("extdata",package="hackWRF"),"/wrfinput_d01"),'XLAT')
#' project_x <- project_emission(x,regions,factors)
#' plot_raster(project_x,proj = TRUE)
#' @export
#'

project_emission <- function(r, shape, factor = rep(1,nrow(shape)), background = 1){

  region <- sf::st_transform(shape,crs = raster::projection(r))
  X      <- r

  if(nrow(region) == 5){
    RE1    <- raster::mask(X,region[1,])
    RE2    <- raster::mask(X,region[2,])
    RE3    <- raster::mask(X,region[3,])
    RE4    <- raster::mask(X,region[4,])
    RE5    <- raster::mask(X,region[5,])
    BACK   <- raster::mask(X,sf::st_sf(sf::st_union(region)),inverse=T)

    FINAL <- raster::merge(factor[1] * RE1,
                           factor[2] * RE2,
                           factor[3] * RE3,
                           factor[4] * RE4,
                           factor[5] * RE5,
                           background * BACK)
    names(FINAL) <- names(X)
  }

  if(nrow(region) == 1){
    RE1    <- raster::mask(X,region[1,])
    BACK   <- raster::mask(X,sf::st_sf(sf::st_union(region)),inverse=T)

    FINAL <- raster::merge(factor[1] * RE1,
                           background * BACK)
    names(FINAL) <- names(X)
  }

  if(!nrow(region) %in% c(1,5)) stop('operation not suported')

  return(FINAL)
}
