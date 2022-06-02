#' Function to add \%FIT to stats
#'
#' @description Function to read, add \%FIT and write stats
#'
#' @param file model data.frame
#' @param verbose display additional information
#'
#' @export
#'
add_FIT <- function(file = choose.files(), verbose = F){
  input <- read.stat(file = file)
  if('FIT (%)' %in% names(input)){
    cat(file,'already contains FIT (%)\n')
    return()
  }else{
    fit   <- input$FA2 * 100
    write.stat(stat = cbind(input,`FIT (%)` = fit),file = file)
  }
}
