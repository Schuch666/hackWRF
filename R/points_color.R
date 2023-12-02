#' Plot points with color
#'
#' @param x x-axis coordinate
#' @param y y-axis coordinate
#' @param z values
#' @param col color
#' @param zlim custom zlim
#' @param pch passed to points
#' @param cex passed to points
#' @param outside to include values outside zlim
#' @param verbose to display additional information
#' @param ... other arguments passed to points and value2color
#'
#' @examples
#' library(raster)
#' ## some maps to include
#' masp   <- raster::shapefile(paste0(system.file("extdata",package="hackWRF"),
#'                             "/RMSP.shp"))
#' br     <- raster::shapefile(paste0(system.file("extdata",package="hackWRF"),
#'                             "/BR_states.shp"))
#'
#' ## site-list w/ lat-lon
#' stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),
#'                     "/stations.Rds"))
#'
#' ## open a evaluation table
#' sample <- read.stat(paste0(system.file("extdata", package = "hackWRF"),
#'                            "/sample.csv"),verbose=TRUE)
#' row.names(sample) <- c("Americana","Campinas","Congonhas")
#'
#' ## combination of lat-lon and evaluation table
#' coord  <- latlon(sample, coord = stations)
#'
#' ## plot
#' plot(NA, xlim = c(-47.75,-45.5), ylim = c(-24.7,-22.2),
#'      xlab = 'lon',ylab = 'lat')
#' points_color(x = coord$lon, y = coord$lat,z = coord$MB)
#' lines(masp, col = 'gray');lines(br)
#'
#' @export
#'

points_color <- function(x,
                         y,
                         z,
                         col     = hcl.colors(41,"Blue-Red"),
                         zlim    = range(z),
                         pch     = 19,
                         cex     = 1.0,
                         outside = TRUE,
                         verbose = F,
                         ...){
  nlevels = length(col)
  if(outside){
    z[z >= zlim[2]] = zlim[2]
    z[z <= zlim[1]] = zlim[1]
  }
  levels <- seq(zlim[1],zlim[2],length.out = nlevels)
  # to use cut, the zlim should be used
  # otherwise only the z range is considered
  colz   <- col[cut(c(zlim[1],z,zlim[2]),nlevels,include.lowest = TRUE,labels = FALSE)]
  colz   <- colz[-1]              # drop first -> zlim min
  colz   <- colz[-length(colz)]   # drop last  -> zlim max

  points(x = x, y = y, col = colz, pch = pch, cex = cex, ... )
}
