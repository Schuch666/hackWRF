#' make a summary using row.names
#'
#' @description Function that open and summarize many stats by row.name
#'
#' @param files list of files generated using write.stat
#' @param verbose display additional information
#' @param rm.last passed to read.stat
#'
#' @export
#'
summarizer <- function(files,rm.last = T,verbose = T){
  x <- NULL
  for(i in files){
    c <- read.stat(file = i,rm.last = rm.last,verbose = verbose)
    c$station <- row.names(c)
    x <- rbind(x,c)
  }

  nomes     <- unique(x$station)
  if(verbose)
    cat('stations:',nomes,'\n')
  output    <- data.frame(matrix(NA,ncol = 15,nrow = length(nomes),byrow = F), stringsAsFactors = F)
  row.names(output) <- nomes
  colnames(output)  <- colnames(x)
  output$station    <- nomes

  for(i in nomes){
    # cat(i,'\n')
    output[output$station == i,1] = sum(x[x$station == i,1],na.rm = T)
    for(j in 2:14){
      output[output$station == i,j] = mean(x[x$station == i,j],na.rm = T)
    }
  }
  return(output)
}
