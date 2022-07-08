#' Function to format stat files
#'
#' @description Function to read, add \%FIT and write stats
#'
#' @param file data.frame with stats
#' @param select name of the column
#' @param rename vector with new name of each column
#' @param digits vector of digits for each column, "i" for integer
#' @param suffix file suffix
#' @param verbose display additional information
#'
#' @note rename can contains NA, for each NA the name don't change
#'
#' @export
#'
format_stats <- function(file = file.choose(),
                         select, rename = NULL,digits = NULL,
                         suffix = '_formatted',verbose = F){
  input  <- read.stat(file = file,verbose = verbose)
  input_names <- names(input)

  for(i in select){
    if(i %in% input_names){
      if(verbose)
        cat(i,'found\n')
    }else{
      cat('names in',file,'\n',input_names,'\n')
      stop(i,'not found in',file,'\n')
    }
  }

  output <- input[,select]

  if(!is.null(rename)){
    new_names <- names(output)
    for(i in seq_along(rename)){
      if(!is.na(rename[i])){
        if(verbose)
          cat('is valid\n')
        new_names[i] <- rename[i]
      }else{
        if(verbose)
          cat('is NA\n')
      }
    }
  }
  names(output) <- new_names

  if(!is.null(digits)){
    for(i in seq_along(digits)){
      if(digits[i] == 'i'){
        as.integer(output[,i])
      }else{
        format     <- paste0('%#.',digits[i],'f')
        output[,i] <- sprintf(output[,i], fmt = format)
      }
    }
  }

  term   <- substr(file,
                   start = nchar(file)-3,
                   stop  = nchar(file) )
  if(term == '.csv' | term == '.txt'){
    file_name <- paste0(substr(file,start = 1,stop = nchar(file)-4),suffix,term)
  }else{
    file_name <- paste0(file,suffix)
  }
  write.stat(output,file_name, verbose = verbose)
}
