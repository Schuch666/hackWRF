#' Functions to write stats and evaluation
#'
#' @description Functions to write stats and evaluation output
#'
#' @param file model data.frame
#' @param stat observed data.frame
#' @param csv logical, use write.csv instead of write.table
#' @param sep the field separator string, passed to write.table function
#' @param dec he string to use for decimal points, passed to write.table function
#' @param ... arguments passed to write.table functions
#' @param verbose display additional information
#'
#' @importFrom utils read.csv write.csv read.table write.table
#'
#' @export
#'
#' @examples
#'
#' sample <- read.stat(paste0(system.file("extdata", package = "hackWRF"),"/sample.csv"),
#'                     verbose = TRUE)
#' dir.create(file.path(tempdir(), "stats"))
#'
#' write.stat(file    = paste0(file.path(tempdir(), "stats"),'/sample.txt'),
#'            stat    = sample,
#'            csv     = FALSE,
#'            verbose = TRUE)
#'
#' write.stat(file    = paste0(file.path(tempdir(), "stats"),'/sample.csv'),
#'            stat    = sample,
#'            csv     = TRUE,
#'            verbose = TRUE)
#'
write.stat <- function(file, stat, csv = TRUE, sep = ';',dec = '.', verbose = FALSE, ...){
  if(verbose)
    cat('writing', file,'\n')
  if(csv){
    write.csv(x = stat, file = file)
  }else{
    write.table(x = stat, file = file, sep = sep, dec = dec, ...)
  }
}

#' Function to read stats and evaluation
#'
#' @description Function to read stats and evaluation output
#'
#' @param file model data.frame
#' @param sep the field separator string, passed to read.table function
#' @param dec he string to use for decimal points, passed to read.table function
#' @param ... arguments passed to read.table functions
#' @param verbose display additional information
#'
#' @export
#'
#' @examples
#' sample <- read.stat(file    = paste0(system.file("extdata", package = "hackWRF"),"/sample.txt"),
#'                     verbose = TRUE)
#'
#' sample <- read.stat(file    = paste0(system.file("extdata", package = "hackWRF"),"/sample.csv"),
#'                     verbose = TRUE)
#'
read.stat <- function(file, sep = ';',dec = '.',verbose = FALSE, ...){
  if(verbose)
    cat('reading', file,'\n')
  if(substr(file,nchar(file)-3,nchar(file)) == '.csv'){
    stat            <- read.csv(file = file)
    nomes           <- stat$X
    stat            <- stat[,-1]
    row.names(stat) <- nomes
    names(stat)     <- names(hackWRF::stats(mo = 1:10, ob = 1:10))
  }else{
    stat <- read.table(file = file, sep = sep, dec = dec, ...)
    names(stat) <- names(hackWRF::stats(mo = 1:10, ob = 1:10))
  }
  return(stat)
}
