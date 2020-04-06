#' Get the last element of a vector
#'
#' @description This function return the last member of a vector
#'
#' @param x vector
#'
#' @export
#'
#' @examples
#' a <- 1:666
#' print(last(a))
#'
last <- function(x) { return( x[length(x)] ) }
