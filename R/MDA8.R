#' Maximum Daily 8-hr Average
#'
#' @description function to calculate Ozone Maximum Daily 8-hr Average or 8-hr moving Average for a data.frame
#'
#' @param x data.frame with time column and aditional columns to be processed
#' @param maximum true to return the dayli maximum instead of the hourly 8-hr
#' @return data.frame
#' @importFrom openair rollingMean
#' @importFrom openair timeAverage
#' @export
#'
MDA8 <- function(x,maximum = F){
  nomes <- names(x)[-1]
  start <- length(nomes) + 2
  # to 8-h avarage
  for(i in nomes){
    x <- rollingMean(x,pollutant = i)
  }
  y <- cbind(x$date,x[,start:ncol(x)])
  names(y) <- c('date',nomes)
  # to daily maximum
  if(maximum){
    y <- timeAverage(y,avg.time = 'day',statistic = 'max')
    y <- as.data.frame(y)
  }
  return(y)
}
