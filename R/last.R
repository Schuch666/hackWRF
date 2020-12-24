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
