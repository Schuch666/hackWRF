#' Get color values from a vector inside a regular interval
#'
#' @param var numeric vector
#' @param col colors
#' @param interval range of values for the color interval (values outside the range are mapped in extremes)
#' @param n_classes number of classes, see details
#' @param alpha value or vector (0 to 1 range) to modify the alpha values
#' @param beta value or vector (0 to 1 range) to lighten the color values
#' @param space one of "HCL", "HLS" or "combined" for colorspace::lighten
#' @param verbose to display additional information
#'
#' @details the default number of classes (n_classes) is the number of colors (col) or length(var) - 1 if length(var) <= n_classes
#'
#' @import classInt
#' @importFrom grDevices hcl.colors
#' @importFrom scales alpha
#' @importFrom colorspace lighten
#'
#' @examples
#' x <- sin(pi/8 * 1:15)
#' barplot(x, col = value_color(x))
#' box()
#'
#' barplot(x, col = value_color(x, alpha = seq(0,1,along.with=x)))
#' box()
#'
#' barplot(x, col = value_color(x, beta = seq(0,1,along.with=x)))
#' box()
#'
#' @export
#'

value2color <- function(var,
                        col       = hcl.colors(41,"Blue-Red"),
                        interval  = range(var),
                        n_classes = length(col),
                        alpha     = NA,
                        beta      = NA,
                        space     = "HLS",
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
  if(!is.na(alpha[1])){
    colcode <- scales::alpha(colour = colcode, alpha = alpha)
  }
  if(!is.na(beta[1])){
    colcode <- colorspace::lighten(col = colcode, space = space, amount = 1 - beta)
  }
  return(colcode)
}
