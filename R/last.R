#' Get the last element of a vector
#'
#' @description This function return the last member of a vector
#'
#' @param x vector
#' @return last element
#'
#' @export
#'
#' @examples
#' a <- 1:666
#' print(last(a))
#'
last <- function(x) { return( x[length(x)] ) }
