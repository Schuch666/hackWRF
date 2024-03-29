#' Filter table A to perform a fair comparison with table B
#'
#' @description Filter table A to perform a fair comparison with table B using row.names
#'
#' @param tabA Table A, from evaluate (output values)
#' @param tabB Table B, from evaluate (for row.names only)
#' @param use_names list of names to be used instead of row.names of Table B
#' @param summaryze logical, true to update or add a summary
#' @param formate argument passed to evaluation function, only works with summaryze = TRUE
#' @param verbose display additional information
#'
#' @note to perform a comparison check both tables using comparison(A,B) and comparison(B,A)
#'
#' @return Table A containing only the row.names in table B
#'
#' @export
#'
#' @examples
#' model <- 1:10
#' data  <- model + rnorm(10,0.2)
#' A     <- stats(mo = model, ob = data)
#' for(i in 2:10){
#'    data  <- model + rnorm(10,0.2)
#'    A     <- rbind(A,stats(mo = model, ob = data))
#' }
#' row.names(A) <- c('a0','a1','a3','b0','b1','b2','b3','d0','d1','d2')
#'
#' data   <- model + rnorm(10,0.2)
#' B      <- stats(mo = model, ob = data)
#' for(i in 2:5){
#'     data   <- model + rnorm(10,0.2)
#'     B      <- rbind(B,stats(mo = model, ob = data))
#'
#' }
#' row.names(B) <- c('a0','a1','z0','z1','z2')
#'
#' comparison(A,B,verbose = TRUE)
#' comparison(B,A,verbose = TRUE)

comparison <- function(tabA, tabB, use_names, summaryze = FALSE, formate = TRUE, verbose = FALSE){

  if(missing(tabB))
    tabB <- tabA   # usefull to summaryze

  if(!missing(use_names)){
    tabB  <- data.frame(NAs = rep(NA,length(use_names)),
                        row.names = use_names,
                        stringsAsFactors = T)
  }

  A <- row.names(tabA)
  B <- row.names(tabB)
  if(verbose){
    cat('table A:\n')
    print(A)
    cat('table B:\n')
    print(B)
  }
  new_table <- tabA[A %in% B,]
  if(summaryze){
    return(evaluation(table = new_table, summaryze = T, formate = formate, verbose = verbose))
  }else{
    return(new_table)
  }
}
