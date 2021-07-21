#' Get color values from a vctor inside a regular interval
#'
#' @param var numeric vector
#' @param col colors
#' @param interval range of values for the color interval (values outside the range are mapped in extremes)
#' @param n_classes number of classes, default is the number of colors (col) or length(var) - 1 if length(var) <= n_classes
#' @param verbose to display additional information
#'
#' @import classInt
#'
#' @examples
#' color    <- hcl.colors(13, "Blue-Red 2")
#' x        <- c(-5,-20,-40,-34,25,50,55,25,12)
#'
#' vcolors  <- value_color(var = x,
#'                         col = color,
#'                         interval = c(-50,50))
#'
#' barplot(x, col = vcolors, ylim = c(-60,60))
#' box()
#'
#' @export
#'

value_color <- function(var,col,interval,n_classes = length(col),verbose = T){
  plotvar <- var
  # include values outside the interval
  var[var < interval[1]] = interval[1] + 0.01
  var[var > interval[2]] = interval[2] - 0.01
  # if(verbose)
  #   print(var)

  if(length(var) <= n_classes)
    n_classes = length(var) - 1
  # this function causes warnings for small vectors
  class   <- suppressWarnings( classInt::classIntervals(var   = plotvar,
                                                        n     = n_classes,
                                                        style = "fixed",
                                                        fixedBreaks=seq(interval[1],
                                                                        interval[2],
                                                                        length.out=n_classes)) )
  colcode <- classInt::findColours(class, col)
  return(colcode)
}
