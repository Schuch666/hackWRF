#' Plot raster object
#'
#' @description functions that modified plot from raster package
#'
#' @param r raster
#' @param log TRUE to plot in log-scale
#' @param min minimum for log plot (default is -3)
#' @param max maximum for log plot
#' @param legend.shrink legend height (default is 0.98)
#' @param legend.width legend width (default is 3)
#' @param axe to plot axis
#' @param llaxis to plot custom axis
#' @param int argument passed to latitude / longitude functions
#' @param proj TRUE to project the raster to latlon
#' @param col color
#' @param x_adjust to raster shift dx
#' @param y_adjust to raster shift dy
#' @param ... arguments to be passing to stats and plot
#'
#' @import raster
#' @importFrom grDevices colorRampPalette
#'
#' @export
#'
#' @examples
#' m <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/model.Rds"))
#'
#'
plot_raster <- function(r, log = FALSE, min = -3, max,
                        legend.shrink = 0.98,legend.width = 3,
                        axe = !llaxis, llaxis = F, int = 10,
                        proj = FALSE,
                        col = c('white',
                                colorRampPalette(colors = c("#D1F5B1",
                                                            "#FFDE24FC",
                                                            "#C70000"))(39)),

                        x_adjust = 0,
                        y_adjust = 0,
                        ...){

  attach(list(...), warn.conflicts = F)
  # name <- attach(list(...), warn.conflicts = F)
  # on.exit(detach(name))

  if(proj){
    r <- projectRaster(r, crs="+proj=longlat +datum=WGS84 +no_defs")
  }

  if(x_adjust!=0){
    r <- raster::shift(x = r,dx=x_adjust)
  }
  if(y_adjust!=0){
    r <- raster::shift(x = r,dy=y_adjust)
  }

  if(FALSE) zlim = FALSE # do nothing / avoid warning

  if(!log & exists('zlim')){
    r[r[] < zlim[1] ] = zlim[1]
    r[r[] > zlim[2] ] = zlim[2]
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
           col           = col,
           ...)
    }else{
      plot(x             = r_log,
           legend.shrink = legend.shrink,
           legend.width  = legend.width,
           axe           = axe,
           axis.args     = arg,
           zlim          = c(min, max),
           col           = col,
           ...)
    }
  }else{
    plot(x             = r,
         legend.shrink = legend.shrink,
         legend.width  = legend.width,
         axe           = axe,
         col           = col,
         ...)
  }
  if(llaxis){
    latitude(int  = int)
    longitude(int = int)
  }
}
