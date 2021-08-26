#' Get latitude and longitude.
#'
#' @param data data.frame with row.names of correscponding locations
#' @param coord data.frame or 'BR-AQ','BR-METAR','BR-INMET' containing lat, lon and row.names of each location
#' @param proj projection string to CRS / proj4string (sp package), defoult in "+proj=longlat"
#' @param verbose to display additional information
#'
#' @import sp
#'
#' @examples
#' sample <- read.stat(paste0(system.file("extdata", package = "hackWRF"),
#'                            "/sample.csv"),verbose=TRUE)
#' row.names(sample) <- c("Americana","Campinas","Congonhas")
#'
#' stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/stations.Rds"))
#'
#' coord    <- latlon(sample, coord = stations)
#' print(coord)
#'
#' @export
#'

latlon <- function(data,coord = 'BR-AQ',proj = "+proj=longlat",verbose = T){

  if(class(coord) == 'character'){
    if(coord[1] == 'BR-AQ'){
      coord  <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/stations.Rds"))
    }else if(coord[1] == 'BR-METAR'){
      coord  <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/metar-br.Rds"))
      coord  <- coord[,c(2,3)]
    }else if(coord[1] == 'BR-INMET'){
      coord  <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/inmet_2015.Rds"))
      coord  <- coord[,c(4,5)]
    }else
      stop(coord,'data not found')
  }

  data$lat <- rep(0,nrow(data))
  data$lon <- rep(0,nrow(data))

  for(i in 1:length(row.names(data))){
    name <- row.names(data)[i]
    if(verbose)
      cat(name,'found\n')
    data$lat[i] = coord[row.names(coord) == name,names(coord) == 'lat'] #[[1]]
    data$lon[i] = coord[row.names(coord) == name,names(coord) == 'lon'] #[[2]]
  }
  pontos <- SpatialPointsDataFrame(coords   = matrix(c(data$lon, data$lat),
                                                     ncol = 2,byrow = F),
                                   data     = data,
                                   match.ID = FALSE)

  sp::proj4string(pontos) <- sp::CRS(proj)

  return(pontos)
}
