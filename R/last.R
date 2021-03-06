#' Get the last element of a vector of data.frame
#'
#' @description This function return the last member of the input
#'
#' @param x vector or data.frame
#' @return last element
#'
#' @export
#'
#' @examples
#' a <- 1:666
#' print(last(a))
#'
last <- function(x) {
  if(is.data.frame(x)){
    return( x[nrow(x),] )
  }
  return( x[length(x)] )
}

#' Get the first element of a vector of data.frame
#'
#' @description This function return the first member of the input
#'
#' @param x vector or data.frame
#' @return first element
#'
#' @export
#'
#' @examples
#' a <- 666:1
#' print(first(a))
#'
first <- function(x) {
  if(is.data.frame(x)){
    return( x[1,] )
  }
  return( x[1] )
}
