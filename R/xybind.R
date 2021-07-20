#' @title Combine R Objects by Columns or Rows
#'
#' @description This function perform a conditional cbind or rbind.
#'
#' @param x first element to combine
#' @param y second element to combine
#' @param bind condition (TRUE or FALSE) for combine x and y or return only x
#' @param message message in case of bind is false (no binding)
#'
#' @examples
#' a <- 1:10
#' b <- 2:11
#'
#' ccbind(a,b,length(a) == length(b))
#' ccbind(a,b,length(a) == 12, message = 'returning x = a')
#' crbind(a,b,length(a) > 9)

#' @describeIn xybind Perform a conditional cbind
#' @export
ccbind <- function(x, y, bind = TRUE, message = NA) {
  if(bind == FALSE){
    if(!is.na(message))
      cat(message,'\n')
    return(x)
  }else{
    return(cbind(x,y))
  }
}
#' @describeIn xybind Perform a conditional rbind
#' @export
crbind <- function(x, y, bind = TRUE, message = NA) {
  if(bind == FALSE){
    if(!is.na(message))
      cat(message,'\n')
    return(x)
  }else{
    return(rbind(x,y))
  }
}
