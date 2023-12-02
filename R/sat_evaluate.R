#' Functions to model evaluation using satellite
#'
#' @description functions to evaluate the spatial performance using satellite
#'
#' @return a data.frame
#'
#' @param mo raster with model
#' @param ob raster with observations
#' @param n number of points from the boundary removed, default is 5
#' @param min minimum value cutoff
#' @param max maximum value cutoff
#' @param verbose set TRUE to display additional information
#'
#' @export
#'
#' @import raster ncdf4
#'

sat_evaluation <- function(mo,ob,n = 6, min = NA, max = NA,verbose = T){

  cut_boundary <- function(x, n,
                           value = NA){
    if(n < 1) return(x)
    A       <- matrix(values(x),
                      ncol  = ncol(x),
                      nrow  = nrow(x),
                      byrow = T)
    A[1:n,] = value
    A[,1:n] = value
    A[(nrow(A)-n+1):nrow(A),] = value
    A[,(ncol(A)-n+1):ncol(A)] = value
    values(x) <- A
    return(x)
  }

  model <- cut_boundary(mo, n = n)
  obser <- interpolate(x = ob, y = mo, verbose = verbose)

  if(!is.na(min)){
    if(verbose) cat('seting min value to',min,'\n')
    model[model < min] = NA
    obser[obser < min] = NA
  }
  if(!is.na(max)){
    if(verbose) cat('seting max value to',max,'\n')
    model[model > max] = NA
    obser[obser > max] = NA
  }

  return(stats(mo = model, ob = obser))
}
