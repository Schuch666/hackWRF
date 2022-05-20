#' Read and write metadata on a NetCDF file
#'
#' @description Read and write metadata information of a NetCDF files
#'
#' @param file file name
#' @param var variable name, 0 to global and "?" to ask all names
#' @param att attribute names (NA for get all attnames)
#' @param action Read or write attribute, get return the value
#' @param value value to write
#' @param verbose display additional information
#'
#' @import ncdf4
#'
#' @export
#'
#' @examples
#' nc <- paste0(system.file("extdata",package="hackWRF"),"/small.nc")
#' meta(nc)
#' meta(nc,att = "Title")
#' meta(nc,var = "?")
#' meta(nc,var = "so2")
#' meta(nc,var = "so2",att = "long_name")
#'

meta <- function(file = NA,var = 0, att = NA, action="read", value=NA, verbose=F){
  if(is.na(file[1])){
    cat("choose a file:\n") # nocov
    file <- file.choose()   # nocov
    cat(paste(file,"\n"))   # nocov
  }
  if(action != "read")
    to_write <- T
  else
    to_write <- F

  meta <- ncdf4::nc_open(filename = file, verbose = verbose, write = to_write)

  if(var == "?"){
    ncdf4::nc_close(meta)
    if(action == 'get'){
      return(names(meta$var))
    }else{
      return(cat(names(meta$var)))
    }
  }

  if(action == "read" | action == "get"){
    if(is.na(att)){
      ATR <- ncdf4::ncatt_get(meta,var,att,verbose=verbose)
      if(var==0)
        cat("global attributes:\n")
      else
        cat(paste0("variable ",var," attritutes:\n"))
      cat(paste(names(ATR),sep = ","))
    }else{
      ATR <- ncdf4::ncatt_get(meta,var,att,verbose=verbose)
      if(var == 0)
        var <- "global"
      cat(paste0(var," attribute ",att,":\n"))
      cat(paste0(ATR$value,"\n"))
    }
    if(action == 'get') return(ATR)
  }else{
    if(is.na(value))
      stop("nothing to write") # nocov
    if(var == 0){
      cat(paste0("writing \'",value,"\' on global attribute \'",att,"\'\nat file ",file,"\n"))
    }else{
      cat(paste0("writing \'",value,"\' on attribute \'",att," \' of ",var,"\nat file ",file,"\n"))
    }

    ncdf4::ncatt_put(meta,varid = var,attname = att,attval = value,verbose = verbose)
  }
  ncdf4::nc_close(meta)
}
