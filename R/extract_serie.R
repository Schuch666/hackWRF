#' Extract time series of wrf file list of lat/lon
#'
#' @description Read and extract data from a list of wrf output files and a table of lat/lon points based on the distance of the points and the center of model grid points, points outside the domain (and points on domain boundary) are not extracteds.
#'
#' @param filelist list of files to be read
#' @param point data.frame with lat/lon
#' @param variable variable name
#' @param field '4d' (defoult), '3d', '2d' or '2dz' see notes
#' @param prefix to output file, defolt is serie
#' @param new start a new file (defoult)
#' @param return.nearest return tha data.frame of nearest points instead of extract the serie
#' @param verbose display additional information
#'
#' @note The field argument '4d' or '2dz' is used to read a 4d/3d variable droping the 3rd dimention (z).
#'
#' @import ncdf4
#' @import eixport
#'
#' @export
#'
#' @examples
#' stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/stations.Rds"))
#' files    <- dir(path = system.file("extdata",package="hackWRF"),pattern = 'wrf',full.names = TRUE)
#' dir.create(file.path(tempdir(),"SERIE"))
#' folder <- file.path(tempdir(),"SERIE")
#' extract_serie(filelist = files, point = stations,prefix = paste0(folder,'/serie'))
#'

extract_serie <- function(filelist, point, variable = 'o3',field = '4d',
                          prefix = 'serie',new = TRUE, return.nearest = F, verbose = TRUE){

  output_file  <- paste0(prefix,'.',variable,'.Rds')

  if(verbose)
    cat('extracting series of',variable,'field',field,'for',nrow(point),'points\n')

  wrf   <- nc_open(filelist[1])
  lat   <- ncvar_get(wrf,"XLAT")
  lon   <- ncvar_get(wrf,"XLONG")
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

  nearest <- function(point,lat,lon){
    for(i in 1:nrow(point)){
      d <- ( (lat - point$lat[i])^2 + (lon - point$lon[i])^2 )^(0.5)

      # d <- ( (lat/2 - point$lat[i]/2)^2 + cos(pi * point$lat[i] / 180)^2 * (lon/2 - point$lon[i]/2)^2 )^(0.5)

      # ponto1 <- c(lon[1],lat[1])
      # ponto2 <- c(point$lat[i],point$lon[i])
      # print(ponto1)
      # print(ponto2)
      # d      <- geosphere::distHaversine(p1 = ponto1, p2 = ponto2)

      # d <- lat
      # for(j in 1:ncol(d)){
      #   for(k in 1:nrow(d)){
      #     d[j,k] <- ( (lat[j,k]/2 - point$lat[i]/2)^2 + cos(lat[j,k]) * cos(point$lat[i]) * (lon[j,k]/2 - point$lon[i]/2)^2 )^(0.5)
      #   }
      # }

      # cos_lat <- lat
      # for(i in 1:ncol(lat)){
      #   for(j in 1:nrow(lat)){
      #     cos_lat <- cos(lat[i,j])
      #   }
      # }
      # d <- ( (lat/2 - point$lat[i]/2)^2 + cos_lat * cos(point$lat[i]) * (lon/2 - point$lon[i]/2)^2 )^(0.5)

      index <- which(d == min(d), arr.ind = TRUE)
      point$i[i] <- index[[1]]
      point$j[i] <- index[[2]]
    }
    return(point)
  }
  stations <- nearest(point,lat,lon)

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

  times   <- eixport::wrf_get(wrf$filename,name = 'time')
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
      lat   <- ncvar_get(wrf,"XLAT")
      lon   <- ncvar_get(wrf,"XLONG")
      if(length(dim(lat)) == 3){
        lat   <- lat[,,1,drop = T]
        lon   <- lon[,,1,drop = T]
      }
      if(length(dim(lat)) == 4){
        lat   <- lat[,,1,1,drop = T]
        lon   <- lon[,,1,1,drop = T]
      }

      # stations <- nearest(point,lat,lon)
      # stations <- remove_outsiders(stations,lat)
      times    <- eixport::wrf_get(wrf$filename,name = 'time')
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
