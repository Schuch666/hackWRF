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
#' x <- sin(pi/8 * 1:15)
#' barplot(x, col = value2color(x))
#' box()
#'
#' @export
#'

points_color <- function(x,
                         y,
                         z,
                         col       = hcl.colors(41,"Blue-Red"),
                         zlim      = range(z),
                         pch       = 19,
                         cex       = 1.0,
                         outside   = TRUE,
                         verbose   = F,
                         ...){

  if(outside){
    z[z >= zlim[2]] = zlim[2]
    z[z <= zlim[1]] = zlim[1]
  }
  levels <- seq(zlim[1],zlim[2],length.out = length(col))
  # to use cut, the zlim should be used
  # otherwise only the z range is considered
  colz   <- col[cut(c(zlim[1],z,zlim[2]),nlevels,include.lowest = TRUE,labels = FALSE)]
  colz   <- colz[-1]              # drop first -> zlim min
  colz   <- colz[-length(colz)]   # drop last  -> zlim max

  points(x = x, y = y, col = colz, pch = pch, cex = cex, ... )
}
