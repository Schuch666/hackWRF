#' Plot raster object
#'
#' @description functions that modifi plot from raster package
#'
#' @param r raster
#' @param log TRUE to plot in log-scale
#' @param min minimum for log calculation
#' @param legend.shrink argument passed to plot
#' @param legend.width argument passed to plot
#' @param axe to plot axis
#' @param llaxis to plot custom axis
#' @param int argument passed to latitude / longitude functions
#' @param ... arguments to be passing to stats and plot
#'
#' @import raster
#'
#' @export
#'
#' @examples
#' model <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/model.Rds"))
#'
#'
plot_raster <- function(r, log = F, min = -3,
                        legend.shrink = 0.98,legend.width = 3,
                        axe = !llaxis, llaxis = T, int = 10, ...){

  # fazer uma logica com zlim para definir min se log = T

  Rlog10 <- function(r,min){
    test <- suppressWarnings(log10(x = r))
    test[is.infinite(test)] <- min
    test[test[] < min ] = min
    return(test)
  }

  if(log){
    # fazer a legenda
    r_log <- Rlog10(r = r,min = min)
    plot(x             = r_log,
         legend.shrink = legend.shrink,
         legend.width  = legend.width,
         axe           = axe, ...)
  }else{
    plot(x             = r,
         legend.shrink = legend.shrink,
         legend.width  = legend.width,
         axe           = axe, ...)
  }
  if(llaxis){
    latitude(int  = int)
    longitude(int = int)
  }
}
