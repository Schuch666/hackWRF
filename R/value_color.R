#' Get color values from a vector inside a regular interval
#'
#' @param var numeric vector
#' @param col colors
#' @param interval range of values for the color interval (values outside the range are mapped in extremes)
#' @param n_classes number of classes, see details
#' @param verbose to display additional information
#'
#' @details the default number of classes (n_classes) is the number of colors (col) or length(var) - 1 if length(var) <= n_classes
#'
#' @import classInt
#' @importFrom grDevices hcl.colors
#'
#' @examples
#' x <- sin(pi/8 * 1:15)
#' barplot(x, col = value_color(x))
#' box()
#'
#' @export
#'

value_color <- function(var,
                        col       = hcl.colors(41,"Blue-Red"),
                        interval  = range(var),
                        n_classes = length(col),
                        verbose   = T){

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
