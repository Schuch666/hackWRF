#' Displace a vector
#'
#' @param x vector
#' @param n number of displacement (positive is right)
#'
#' @export
#'

displace <- function(x, n = 0){
  y <- x
  if(n > 0){
    for(i in 1:n){
      y[i] <- NA
    }
    for(i in (n+1):length(x)){
      y[i] <- x[i-n]
    }
  }
  if(n < 0){
    for(i in (length(x)+n+1):length(x)){
      y[i] <- NA
    }
    for(i in (n+1):length(x)){
      y[i] <- x[i-n]
    }
  }
  return(y)
}

#' @examples
# A <- 1:20
# B <- displace(A, n = 10)
# print(A)
# print(B)
# C <- displace(A, n = -15)
# print(C)
