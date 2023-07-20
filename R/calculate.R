#' Functions to calculate specific variables to model evaluation
#'
#' @description function read and calculate model layer thickness
#'
#' @return a raster object
#'
#' @param file file name
#' @param file2 file name (for PH)
#' @param verbose set TRUE to display additional information
#' @param as_raster to return a raster instead of a array
#'
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4
#'

calculate_DZ <- function(file, file2 = file, as_raster = FALSE, verbose = TRUE){
  PHB    <- wrf_get(file = file,'PHB', verbose = verbose)
  PH     <- wrf_get(file = file2,'PH', verbose = verbose)
  Z      <- (PHB+PH)/9.817
  dz     <- array(0,c(dim(Z)[1],dim(Z)[2],dim(Z)[3]-1))
  for(i in 1:(dim(Z)[3]-1)){
    dz[,,i] = Z[,,i+1] - Z[,,i]
  }
  if(as_raster){
    template <- wrf_raster(file = file,'PH',verbose = F)
    dz       <- raster::brick(apply(dz, c(1,3), rev),
                              xmn = extent(template)[1],
                              xmx = extent(template)[2],
                              ymn = extent(template)[3],
                              ymx = extent(template)[4],
                              crs = raster::crs(template,asText=TRUE))
    return(dz)
  }else{
    return(dz)
  }
}

#' @description functions read and to calculate Cloud water path
#' @param met file with meteorological variables (ALT, PH,PHB)
#' @param ... passed to calculate_DZ
#'
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4

calculate_CWP <- function(file, met, verbose = TRUE, ... ){

  QCLOUD <- wrf_get(file = file,'QCLOUD', verbose = verbose)
  ALT    <- wrf_get(file = met, 'ALT', version = verbose)
  dz     <- calculate_DZ(file = met, verbose = verbose, ...)
  lwp    <- 1000 * QCLOUD * (dz / ALT)

  template <- wrf_raster(file = file,'QCLOUD')
  R <- raster::brick(apply(lwp, c(1,3), rev),
                     xmn = extent(template)[1],
                     xmx = extent(template)[2],
                     ymn = extent(template)[3],
                     ymx = extent(template)[4],
                     crs = raster::crs(template,asText=TRUE))

  R <- calc(R,sum) # "g m-2"
  names(R) <- 'WRF_CWP'
  return(R)
}

#' @description functions read and to calculate Cloud droplet number concentration
#' @param met file with meteorological variables (ALT, PH,PHB)
#' @param ... passed to calculate_DZ
#'
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4

calculate_CDNC <- function(file, met, verbose = TRUE, ... ){

  QNDROP <- wrf_get(file = file,'QNDROP', verbose = T)
  dz     <- calculate_DZ(file = met, verbose = verbose, ...)
  cdnc   <- QNDROP * dz
  cdnc   <- cdnc * 1e-8

  template <- wrf_raster(file = file,'QNDROP')
  R <- raster::brick(apply(cdnc, c(1,3), rev),
                     xmn = extent(template)[1],
                     xmx = extent(template)[2],
                     ymn = extent(template)[3],
                     ymx = extent(template)[4],
                     crs = raster::crs(template,asText=TRUE))

  R <- calc(R,sum) # "g m-2"
  names(R) <- 'WRF_CDNC'
  return(R)
}

#' @description functions read and to calculate column sum concentration
#' @param var variable name
#' @param met file with meteorological variables (ALT, PH,PHB)
#' @param ... passed to calculate_DZ
#'
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4

calculate_column <- function(file, var, met, verbose = FALSE, ... ){

  avo     = 6.02E+23
  rfac    = 8.314
  dobfac  = 2.687E+16
  fac2    = avo/rfac * 1E-4 * 1E-6

  P       <- wrf_get(file = met,name = 'PB',verbose = verbose) +
             wrf_get(file = met,name = 'P', verbose = verbose)
  Temp    <- wrf_get(file = met,name = 'T',verbose = verbose)
  VAR     <- wrf_get(file = file,var, verbose = TRUE)

  Temp    <- (Temp+300)*((P/10000)^0.286)
  dz      <- calculate_DZ(file = met, verbose = verbose, ...)

  VAR     <- fac2 * VAR * dz * (P/Temp)

  template <- wrf_raster(file = file,var)
  R <- raster::brick(apply(VAR, c(1,3), rev),
                     xmn = extent(template)[1],
                     xmx = extent(template)[2],
                     ymn = extent(template)[3],
                     ymx = extent(template)[4],
                     crs = raster::crs(template,asText=TRUE))

  R <- calc(R,sum)
  names(R) <- paste0('WRF_column_',toupper(var))
  return(R)
}

#' @description functions read and to calculate column sum concentration
#' @param map a file with LAT, LON and other grid information
#' @param met file with meteorological variables (ALT, PH,PHBD)
#' @param last calculate using only the last time
#' @param ... passed to calculate_DZ
#'
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4

calculate_ACC_RAIN <- function(file,map = file,last = TRUE,verbose = TRUE){

  grid_info = map

  rain1 <- wrf_raster(file,'RAINC', verbose = verbose, map = grid_info ) # cumulus
  rain2 <- wrf_raster(file,'RAINNC',verbose = verbose, map = grid_info ) # grid scale
  rain  <- rain1 + rain2
  if(last){
    i = dim(rain)[3]
    cat('prossessing time:',i,'...\n')
    rain <- rain[[i]] - rain[[1]]
    n <- length(wrf_get(file,'time'))/24
    cat(n,'days of data\n')
    rain <- rain/n
    names(rain) <- paste0('ACC_RAIN')
  }else{
    for(i in dim(rain)[3]:1){
      cat('prossessing time:',i,'...\n')
      rain[[i]] = rain[[i]] - rain[[1]]
    }
    rain        <- calc(rain, mean)
    names(rain) <- paste0('ACC_RAIN')
  }
  return(rain)
}

#' @description functions read and to calculate Cloud optical thickness
#' @param file file with TAUCLDI variable (and/or TAUCLDC)
#' @param file2 file with TAUCLDC variable
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4

calculate_COT <- function(file, file2 = file,verbose = TRUE){
  TAUCLDI    <- wrf_raster(file = file, name = 'TAUCLDI',verbose = verbose)
  TAUCLDC    <- wrf_raster(file = file2,name = 'TAUCLDC',verbose = verbose)
  if(verbose) cat('calculating COT...\n')
  R          <- calc(TAUCLDI,sum) + calc(TAUCLDC,sum)
  names(R)   <- 'WRF_COT'
  return(R)
}

#' @description functions read and to calculate Cloud optical thickness
#' @param met file with meteorological variables (ALT, PH,PHB)
#'
#' @param file file with CCN5
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4

calculate_CCN <- function(file, met = file, verbose = TRUE, ... ){
  ccn5       <- wrf_raster(file = file,name = 'CCN5',verbose = verbose)
  dz         <- calculate_DZ(file = met, verbose = verbose,as_raster = T)
  dz         <- dz  * 100  # m -> cm
  ccn        <- calc(ccn5 * dz, sum)
  names(ccn) <- 'WRF_CCN'
  return(ccn)
}

#' @description functions read and to calculate Aerosol Optical Depth
#' @param met file with meteorological variables (ALT, PH,PHB)
#'
#' @param file file with TAUAER3
#' @export
#' @name calculate_var
#'
#' @import raster ncdf4

calculate_AOD <- function(file, met = file, verbose = TRUE, ... ){
  tau3       <- wrf_raster(file = file,name = 'TAUAER3',verbose = verbose)
  # dz       <- calculate_DZ(file = met, verbose = verbose,as_raster = T)
  # dz       <- dz  * 100  # m -> cm
  aod        <- calc(tau3,sum)
  names(aod) <- 'WRF_AOD'
  return(aod)
}
