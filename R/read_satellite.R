#' Functions to read satellite products in NetCDF/HDF(4) format
#'
#' @description function to read MODIS NetCDF
#'
#' @return a raster object
#'
#' @param file file name
#' @param var variable name, it can open a menu, in read_g4 it can read the first variable by defoult
#' @param verbose set TRUE to display additional information
#' @name read_satellite
#'
#' @export
#'
#' @import raster ncdf4
#'
read_MODIS <- function(file, var, verbose = T){
  VAR <- wrf_get(file = file, name = var, verbose = T)
  if(verbose)  cat('creating raster for',var,'\n')
  VAR <- apply(VAR,2,rev)
  R   <- raster(VAR,xmn=-180, xmx=180, ymn=-90, ymx=90,crs = "+proj=longlat +datum=WGS84 +no_defs")
  names(R) <- paste0('MODIS_',var)
  return(R)
}

#' @description function to read MODIS products in HDF(4) format
#' @export
#' @name read_satellite
#' @import raster ncdf4

read_MODIS_HDF <- function(file, var, verbose = T){

  if(verbose) cat('opening',file,'...\n')
  nc <- nc_open(file)

  if(missingArg(var)){
    var <- names(nc$var)
    if(length(var) != 1){
      if(verbose) cat('returning var names\n')
      return(var)
    }
  }
  VAR <- ncvar_get(nc, var)
  VAR <- apply(VAR,1,rev)
  VAR <- apply(VAR,2,rev)
  r   <- raster(VAR,xmn=-180, xmx=180, ymn=-90, ymx=90,crs = "+proj=longlat +datum=WGS84 +no_defs")

  scale_factor <- ncatt_get(nc,var,attname = 'scale_factor')
  if(verbose)
    cat('scale factor =', scale_factor$value,'\n')
  r <- r / scale_factor$value

  names(r) <- paste0('MODIS_hdf_',var)
  return(r)
}

#' @description function to read satellite products in NetCDF format
#' @name read_satellite
#' @import raster ncdf4
#' @export

read_MERGE <- function(file,var = 'prec',verbose = T){

  VAR  <- wrf_get(file = file, name = var,verbose = verbose)
  lat  <- wrf_get(file = file, name = 'lat')
  lon  <- wrf_get(file = file, name = 'lon')
  R    <- raster(apply(VAR,1,rev),
                 xmn=min(lon), xmx=max(lon),ymn=min(lat), ymx=max(lat),
                 crs = "+proj=longlat +datum=WGS84 +no_defs")
  names(R) <- paste0('MERGE_',var)
  return(R)
}

#' @description function to read g4 selection in NetCDF format
#' @import raster ncdf4
#' @export
#' @name read_satellite
#'
read_g4 <- function(file, var = NA, verbose = T){

  if(is.na(var)){
    var <- meta(file = file,var = '?',action = 'get')[1]
  }
  VAR  <- wrf_get(file = file, name = var, verbose = verbose)
  lat  <- wrf_get(file = file, name = 'lat')
  lon  <- wrf_get(file = file, name = 'lon')
  cat('creating raster for',var,'\n')
  VAR <- apply(VAR,1,rev)
  R   <- raster(VAR,
                xmn=min(lon), xmx=max(lon),ymn=min(lat), ymx=max(lat),
                crs = "+proj=longlat +datum=WGS84 +no_defs")
  names(R) <- paste0('g4_',var)
  return(R)
}

#' @description function to read CERES products in NetCDF format
#' @param month integer 1-12 (CERES only)
#' @param year integer, YYYY format (CERES only)
#' @param k conversion factor, default is 2.69E16 (OMI only), see notes
#' @note Conversion from DU to molecules / cm 2
#' @export
#' @name read_satellite
#' @import raster ncdf4

read_CERES <- function(file, var, month = 1, year = 2012, verbose = T){

  index <- month + (year - 2012) * 12
  cat('month=',month,'year=', year,'index=',index,'\n')

  VAR <- wrf_get(file = file, name = var, verbose = verbose)
  VAR <- VAR[,,index]
  lat  <- wrf_get(file = file, name = 'lat')
  lon  <- wrf_get(file = file, name = 'lon')
  cat('creating raster for',var,'\n')
  VAR <- t( VAR )
  VAR <- apply(VAR,2,rev)
  R   <- raster(VAR,
                xmn=min(lon), xmx=max(lon),ymn=min(lat), ymx=max(lat),
                crs = "+proj=longlat +datum=WGS84 +no_defs")
  names(R) <- paste0('CERES_',var)
  return(rotate(R))
}

#' @description function to read OMI products in NetCDF format
#' @param k conversion factor, default is 2.69E16 (OMI only), see notes
#' @export
#' @import raster ncdf4
#' @name read_satellite

read_OMI <- function(file, var = NA, k = 2.69E16, verbose = T){
  VAR <- wrf_get(file = file, name = var, verbose = verbose)
  cat('creating raster for',var,'\n')
  VAR <- t(VAR)
  VAR <- apply(VAR,2,rev)
  R   <- raster(VAR,xmn=-180, xmx=180, ymn=-90, ymx=90,crs = "+proj=longlat +datum=WGS84 +no_defs")
  R[R < 0 ] = NA
  R   <- k * R   # convertion from DU to molecules / cm 2
  names(R) <- paste0('OMI_',var)
  return(R)
}

#' @description function to read AIRS products in NetCDF format
#' @param k conversion factor, default is 2.69E16 (OMI only), see notes
#' @export
#' @import raster ncdf4
#' @name read_satellite

read_AIRS <- function(file, var, k = 2.69E16, verbose = T){

  cat('creating raster for',var,'\n')

  VAR <- wrf_get(file, paste0(var,'_A'))
  VAR <- t(VAR)
  Ra  <- raster(VAR,xmn=-180, xmx=180, ymn=-90, ymx=90,crs = "+proj=longlat +datum=WGS84 +no_defs")

  VAR <- wrf_get(file,paste0(var,'_D'))
  VAR <- t(VAR)
  Rd  <- raster(VAR,xmn=-180, xmx=180, ymn=-90, ymx=90,crs = "+proj=longlat +datum=WGS84 +no_defs")

  R   <- merge(Ra,Rd)
  R   <- k * R
  names(R) <- paste0('AIRS_',var)
  return(R)
}
