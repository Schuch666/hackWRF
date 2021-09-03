#' Plot raster object
#'
#' @description functions that modifi plot from raster package
#'
#' @param r raster
#' @param log TRUE to plot in log-scale
#' @param min minimum for log plot (default is -3)
#' @param max maximum for log plot
#' @param legend.shrink argument passed to plot
#' @param legend.width argument passed to plot
#' @param axe to plot axis
#' @param llaxis to plot custom axis
#' @param int argument passed to latitude / longitude functions
#' @param proj TRUE to project the raster to latlon
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
plot_raster <- function(r, log = F, min = -3, max,
                        legend.shrink = 0.98,legend.width = 3,
                        axe = !llaxis, llaxis = T, int = 10,
                        proj = F,
                        ...){

  if(proj){
    r <- projectRaster(r, crs="+proj=longlat +datum=WGS84 +no_defs")
  }

  Rlog10 <- function(r,min){
    test <- suppressWarnings(log10(x = r))
    test[is.infinite(test)] <- min
    test[test[] < min ] = min
    return(test)
  }

  if(log){
    r_log  <- Rlog10(r = r,min = min)
    rng    <- range(r_log[], na.rm = T)
    if(missing(max)){
      at     <- seq(round(rng[1], 1),round(rng[2], 1),by = 1)
    }else{
      at     <- seq(round(min, 1),round(max, 1),by = 1)
    }
    label  <- paste0('10^',at)
    label <- parse(text = label)
    label[at == 0] = '  1'

    arg <- list(at=at, labels=label)

    if(missing(max)){
      plot(x             = r_log,
           legend.shrink = legend.shrink,
           legend.width  = legend.width,
           axe           = axe,
           axis.args     = arg,
           ...)
    }else{
      plot(x             = r_log,
           legend.shrink = legend.shrink,
           legend.width  = legend.width,
           axe           = axe,
           axis.args     = arg,
           zlim          = c(min, max),
           ...)
    }
  }else{
    plot(x             = r,
         legend.shrink = legend.shrink,
         legend.width  = legend.width,
         axe           = axe,
         ...)
  }
  if(llaxis){
    latitude(int  = int)
    longitude(int = int)
  }
}
