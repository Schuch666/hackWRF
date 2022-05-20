#' Extract time series of wrf file list of lat/lon
#'
#' @description Read and extract data from a list of wrf output files and a table of lat/lon points based on the distance of the points and the center of model grid points, points outside the domain (and points on domain boundary) are not extracteds.
#'
#' @param filelist list of files to be read
#' @param point data.frame with lat/lon
#' @param variable variable name
#' @param field '4d' (defoult), '3d', '2d' or '2dz' see notes
#' @param prefix to output file, defolt is serie
#' @param new TRUE, FALSE of 'check' see notes
#' @param return.nearest return the data.frame of nearest points instead of extract the serie
#' @param fast faster calculation of grid distances but less precise
#' @param use_ij logical, use i and j from input instead of calculate
#' @param latitude name of latitude coordinade variable in the netcdf
#' @param longitude name of longitude coordinade variable in the netcdf
#' @param use_TFLAG use the variable TFLAG (CMAQ / smoke) instead of Times (WRF)
#' @param verbose display additional information
#'
#' @note The field argument '4d' or '2dz' is used to read a 4d/3d variable droping the 3rd dimention (z).
#'
#' @note new = TRUE create a new file, new = FALSE append the data in a old file, and new = 'check' check if the file exist and append if the file exist and create if the file doesnt exist
#'
#' @import ncdf4
#' @import eixport
#'
#' @export
#'
#' @examples
#' cat('Example 1: INMET stations for 2015\n')
#' stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/inmet_2015.Rds"))
#'
#' cat('Example 2: METAR stations of Brazil\n')
#' stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/metar-br.Rds"))
#'
#' cat('Example 3: METAR soundings over Brazil\n')
#' stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/soundings.Rds"))
#'
#' cat('Example 4: Brazilian Air Quality: CETESB (SP), RAMQAr (ES) and SMAC (RJ)\n')
#' stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/stations.Rds"))
#'
#' files    <- dir(path = system.file("extdata",package="hackWRF"),
#'                 pattern = 'wrf.day',
#'                 full.names = TRUE)
#' dir.create(file.path(tempdir(),"SERIE"))
#' folder <- file.path(tempdir(),"SERIE")
#'
#' # extract data for 3 locations
#' extract_serie(filelist = files, point = stations[1:3,],prefix = paste0(folder,'/serie'))
#'

extract_serie <- function(filelist, point, variable = 'o3',field = '4d',
                          prefix = 'serie',new = 'check', return.nearest = FALSE,
                          fast = FALSE, use_ij = FALSE,
                          latitude = 'XLAT',longitude = 'XLONG', use_TFLAG = F,
                          verbose = TRUE){

  output_file  <- paste0(prefix,'.',variable,'.Rds')

  if(new == 'check'){
    new = !file.exists(output_file)
  }

  if(verbose)
    cat('extracting series of',variable,'field',field,'for',nrow(point),'points\n')

  wrf   <- nc_open(filelist[1])
  lat   <- ncvar_get(wrf,latitude)
  lon   <- ncvar_get(wrf,longitude)
  if(verbose)
    cat('dim of lat/lon:',dim(lat),'\n')

  if(length(dim(lat)) == 3){
    lat   <- lat[,,1,drop = T]
    lon   <- lon[,,1,drop = T]
  }
  if(length(dim(lat)) == 4){
    lat   <- lat[,,1,1,drop = T]
    lon   <- lon[,,1,1,drop = T]
  }
  if(verbose)
    cat('used dim of lat/lon:',dim(lat),'\n')

  if(use_ij){
    stations <- point
    if(verbose)
      cat('using i & j to extract points:\n')
  }else{
    nearest <- function(point,lat,lon,fast){
      if(verbose)
        cat('calculating distances...\n')

      for(i in 1:nrow(point)){
        # OLD CODE
        if(fast){
          d <- ( (lat - point$lat[i])^2 + (lon - point$lon[i])^2 )^(0.5)
        }else{
          d <- lat                # to d get dimmention of lat
          for(k in 1:nrow(d)){    # using NEW CODE
            for(l in 1:ncol(d)){
              d[k,l] <- get_distances(lat1  = point$lat[i],
                                      long1 = point$lon[i],
                                      lat2  = lat[k,l],
                                      long2 = lon[k,l])
            }
          }
        }

        index <- which(d == min(d), arr.ind = TRUE)
        point$i[i] <- index[[1]]
        point$j[i] <- index[[2]]
      }
      return(point)
    }
    stations <- nearest(point,lat,lon,fast)
  }

  remove_outsiders <- function(stations,lat){
    j              <- 1
    station_inside <- stations[j,]

    for(i in 1:nrow(stations)){
      outside        = FALSE
      if(stations$i[i] == 1)         outside = TRUE
      if(stations$j[i] == 1)         outside = TRUE
      if(stations$i[i] == nrow(lat)) outside = TRUE
      if(stations$j[i] == ncol(lat)) outside = TRUE

      if(outside){
        cat('* station',rownames(stations)[i],'ouside the domain\n')
      }else{
        #cat('station',rownames(stations)[i],'inside the domain\n')
        station_inside[j,] <- stations[i,]
        rownames(station_inside)[j] <- rownames(stations)[i]
        j <- j + 1
      }
    }
    return(station_inside)
  }
  stations <- remove_outsiders(stations,lat)

  model_lat_lon <- function(point,lat,lon){
    for(i in 1:nrow(point)){
      point$model_lat[i] <- lat[point$i[i],point$j[i]]
      point$model_lon[i] <- lon[point$i[i],point$j[i]]
    }
    return(point)
  }

  stations <- model_lat_lon(stations,lat,lon)

  if(return.nearest){
    return(stations)
  }

  if(verbose){
    print(stations)
    cat('reading',variable,':',filelist[1],'file 1 of',length(filelist),'\n')
  }

  if(use_TFLAG){
    TFLAG <- eixport::wrf_get(file = wrf$filename,name = 'TFLAG')        # nocov
    TFLAG <- TFLAG[,1,,drop = T]                                         # nocov
    year  <- as.numeric(substr(x = TFLAG[1,],start = 1,stop = 4))        # nocov
    jday  <- as.numeric(substr(x = TFLAG[1,],start = 5,stop = 7))        # nocov
    day   <- as.Date(paste0(year,jday),format = '%Y%j')                  # nocov
    hour  <- as.numeric(TFLAG[2,]) / 10000                               # nocov
    hour  <- paste(formatC(hour,width = 2, format = "d", flag = "0"))    # nocov
    date_time <- paste0(as.character(day),' ',hour,':00:00')             # nocov
    times     <- as.POSIXlt(date_time, tz = "UTC",                       # nocov
                            format="%Y-%m-%d %H:%M:%OS", optional=FALSE) # nocov
  }else{
    times   <- eixport::wrf_get(wrf$filename,name = 'time',verbose = F)
  }

  if(field == '2d')
    contagem  = NA             # 2d Field (x,y)
  if(field == '2dz')
    contagem = c(-1,-1,1)      # 3d Field (x,y,z)
  if(field == '3d')
    contagem  = NA             # 3d Field (x,y,t)
  if(field == '4d')
    contagem = c(-1,-1,1,-1)   # 4d Field (x,y,z,t)
  var     <- ncvar_get(wrf,variable,count = contagem)
  nc_close(wrf)

  serie <- as.data.frame(times)
  if(length(times) > 1){
    for(i in 1:nrow(stations)){
      serie[,i+1] <- var[stations$i[i],stations$j[i],]
    }
  }else{
    for(i in 1:nrow(stations)){
      serie[i+1] <- var[stations$i[i],stations$j[i]]
    }
  }
  names(serie) <- c("date", row.names(stations))

  if(new){
    saveRDS(serie,output_file)
  }else{
    old <- readRDS(output_file)
    saveRDS(rbind(old,serie),output_file)
  }

  if(length(filelist) > 1){
    for(i in 2:length(filelist)){
      if(verbose)
        cat('reading',variable,':',filelist[i],'file',i,'of',length(filelist),'\n')

      wrf   <- nc_open(filelist[i])
      lat   <- ncvar_get(wrf,latitude)
      lon   <- ncvar_get(wrf,longitude)
      if(length(dim(lat)) == 3){
        lat   <- lat[,,1,drop = T]
        lon   <- lon[,,1,drop = T]
      }
      if(length(dim(lat)) == 4){
        lat   <- lat[,,1,1,drop = T]
        lon   <- lon[,,1,1,drop = T]
      }

      # stations <- nearest(point,lat,lon,fast)
      # stations <- remove_outsiders(stations,lat)

      if(use_TFLAG){
        TFLAG <- eixport::wrf_get(file = wrf$filename,name = 'TFLAG')        # nocov
        TFLAG <- TFLAG[,1,,drop = T]                                         # nocov
        year  <- as.numeric(substr(x = TFLAG[1,],start = 1,stop = 4))        # nocov
        jday  <- as.numeric(substr(x = TFLAG[1,],start = 5,stop = 7))        # nocov
        day   <- as.Date(paste0(year,jday),format = '%Y%j')                  # nocov
        hour  <- as.numeric(TFLAG[2,]) / 10000                               # nocov
        hour  <- paste(formatC(hour,width = 2, format = "d", flag = "0"))    # nocov
        date_time <- paste0(as.character(day),' ',hour,':00:00')             # nocov
        times     <- as.POSIXlt(date_time, tz = "UTC",                       # nocov
                                format="%Y-%m-%d %H:%M:%OS", optional=FALSE) # nocov
      }else{
        times   <- eixport::wrf_get(wrf$filename,name = 'time',verbose = F)
      }

      var      <- ncvar_get(wrf,variable,count = contagem)
      nc_close(wrf)

      serie <- as.data.frame(times)
      if(length(times) > 1){
        for(i in 1:nrow(stations)){
          serie[,i+1] <- var[stations$i[i],stations$j[i],]
        }
      }else{
        for(i in 1:nrow(stations)){
          serie[i+1] <- var[stations$i[i],stations$j[i]]
        }
      }
      names(serie) <- c("date", row.names(stations))

      old <- readRDS(output_file)
      saveRDS(rbind(old,serie),output_file)
    }
  }

  if(verbose)
    cat('output:',output_file,'\n')
}
