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
  }else if('RasterBrick' %in% class(x)){
    return( x[[dim(x)[3]]] )
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
  }else if(class(x) %in% 'RasterBrick'){
    return( x[[1]] )
  }
  return( x[1] )
}

#' return character with zero left
#'
#' @description return character with zero left
#'
#' @export
#'
#' @return character with zeros left
#' @param x numerical input
#' @param dig number of digits (defoult is 2)
#'
#' @examples
#' a <- 1:66
#' print(nd(a))
#'
nd <- function(x, dig = 2){
  return(formatC(x,width=dig,format="d",flag = "0"))
}
