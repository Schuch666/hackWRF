#' @title PLot latitude and longitude
#'
#' @description function to plot cool latitude and longitude.
#' labels for latitude range from 180ºW to 0 to 180ºE and longitude from 90ºS to 0 to 90ºN.
#'
#' @param int interval in degrees
#' @param side side to plot, see axis
#' @param lmin minimum lat or lon
#' @param lmax maximum lat or lon
#' @param r a raster object (to projected versions)
#' @param lty line type for grid_projected (default is 3)
#' @param col line color for grid_projected (default is gray)
#' @param lat_min for rid_projected (default is -80)
#' @param lat_max for rid_projected (default is 30)
#' @param lon_min for rid_projected (default is -160)
#' @param lon_max for rid_projected (default is 160)
#' @param ... additional arguments passed to axis function
#'
#' @import sp raster
#' @importFrom stats approxfun
#'
#' @examples
#' library(raster)
#' br <- shapefile(paste0(system.file("extdata",package="hackWRF"),"/BR_states.shp"),verbose=FALSE)
#' plot(br)
#' box()
#' latitude()
#' longitude()
#' grid()
#'
#' @describeIn plot nice latitude axis
#' @export
longitude <- function(int = 10, side = 1,lmin = -180, lmax = 180, ...){
  vet_lat <- seq(lmin,lmax,by = int)
  lab_lat <- c(paste0(seq(-lmin,int,by=-int),"\u00baW"),'0',
               paste0(seq(int,lmax,by=int),"\u00baE"))

  axis(side,at = vet_lat,labels = lab_lat, ...)
}
#' @describeIn plot nice longitude axis
#' @export
latitude <- function(int = 10,side = 2,lmin = -90, lmax = 90, ...){
  vet_lon <- seq(lmin,lmax,by = int)
  lab_lon <- c(paste0(seq(-lmin,int,by=-int),"\u00baS"),'0',
               paste0(seq(int,lmax,by=int),"\u00baN"))

  axis(side,at = vet_lon,labels = lab_lon, ...)
}
#' @describeIn plot projected longitude axis
#' @export
longitude_proj <- function(r, int = 10, side = 1,lmin = -180, lmax = 180, ...){
  vet_lat <- seq(lmin,lmax,by = int)
  lab_lat <- c(paste0(seq(-lmin,int,by=-int),"\u00baW"),'0',
               paste0(seq(int,lmax,by=int),"\u00baE"))

  proj             <- raster::crs(r,asText=TRUE)

  usr <- par('usr')
  tn <- 100
  tx <- seq(usr[1], usr[2], length.out = tn)
  ty <- rep(usr[3], tn)

  pontos           <- cbind(x = tx, y = ty)
  firstPoints      <- SpatialPoints(coords = pontos)
  crs(firstPoints) <- proj
  tt               <- spTransform(x = firstPoints,
                                  CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  axis_coords      <- coordinates(tt)
  tfcn             <- approxfun(axis_coords[,1], tx)

  axis(side,at = tfcn(vet_lat),labels = lab_lat, ... )
}
#' @describeIn plot projected latitude axis
#' @export
latitude_proj <- function(r, int = 10,side = 2,lmin = -80, lmax = 80, ...){
  vet_lon <- seq(lmin,lmax,by = int)
  lab_lon <- c(paste0(seq(-lmin,int,by=-int),"\u00baS"),'0',
               paste0(seq(int,lmax,by=int),"\u00baN"))

  proj             <- raster::crs(r,asText=TRUE)

  usr <- par('usr')
  tn  <- 100
  tx  <- rep(usr[1], tn)
  ty  <- seq(usr[3], usr[4], length.out=tn)
  pontos           <- cbind(x = tx, y = ty)
  firstPoints      <- SpatialPoints(coords = pontos)
  crs(firstPoints) <- proj
  tt               <- spTransform(x = firstPoints,
                                  CRSobj = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  axis_coords      <- coordinates(tt)
  tfcn             <- approxfun(axis_coords[,2], ty)

  axis(side,at = tfcn(vet_lon),labels = lab_lon, ... )
}
#' @describeIn plot grid (lalitude and longitude) in a different projection
#' @export
grid_proj <- function(r, int = 10, lty = 3, col = "#666666",
                      lat_min = -80,  lat_max = 30,
                      lon_min = -160, lon_max = 160,
                      ...){
  proj         <- raster::crs(r,asText=TRUE)
  for(lat in seq(lat_min,lat_max,by = int)){
    M          <- matrix(data = lat, nrow = 1000/int, ncol = 2)
    M[,1]      <- seq(-160,160,along.with = M[,1])
    line1      <- Line(M)
    linea      <- Lines(line1, ID = "a")
    firstLine  <- SpatialLines(LinesList = list(linea))
    crs(firstLine) <- "+proj=longlat +datum=WGS84 +no_defs"
    firstLine_proj <- spTransform(x = firstLine, CRSobj = CRS(proj))
    lines(firstLine_proj, lty = lty, col = col,...)
  }
  for(lon in seq(lon_min,lon_max,by = int)){
    M          <- matrix(data = lon, nrow = 2000/int, ncol = 2)
    M[,2]      <- seq(-80,80,along.with = M[,2])
    line1      <- Line(M)
    linea      <- Lines(line1, ID = "a")
    firstLine  <- SpatialLines(LinesList = list(linea))
    crs(firstLine) <- "+proj=longlat +datum=WGS84 +no_defs"
    firstLine_proj <- spTransform(x = firstLine, CRSobj = CRS(proj))
    lines(firstLine_proj, lty = lty, col = col,...)
  }
}
