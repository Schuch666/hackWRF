#' Custom function to interpolation of a raster from satellite to a raster from model
#'
#' @description function to interpolate an satellite observation to model domain
#'
#' @return a raster object
#'
#' @param x a raster from observation
#' @param y a raster from model
#' @param fast to use only the raster extent region
#' @param e raster extenet
#' @param verbose set TRUE to display additional information
#'
#' @seealso for raster from satellite observations see \code{\link{read_satellite}}. For raster from model see wrf_raster from eixport package and \code{\link{calculate_var}}.
#'
#' @export
#'
#' @import raster
#'

interpolate <- function(x,y,fast = F,e = extent(-80, -25, -40, 10),verbose = T){
  if(fast){
    x      <- raster::crop(x,e)
  }
  if(verbose) cat('interpolating data to model grid...\n')
  x_proj <- projectRaster(x,crs = raster::crs(y,asText=TRUE))
  x_res  <- resample(x_proj,y)
  return(x_res)
}
