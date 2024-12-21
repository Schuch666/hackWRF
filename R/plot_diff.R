#' Plot raster difference
#'
#' @description Plot difference (absolute and relative) of A - B single layer raster objects
#'
#' @param a raster A
#' @param b raster B
#' @param file_a file for raster A
#' @param file_b file for raster B
#' @param var sub title 1 and variable to be open for file_a and file_b
#' @param map map passed to eixport::wrf_raster
#' @param operation function passed to calc, default is mean
#' @param col color scale
#' @param col_1 color for primary map
#' @param col_2 color for secondary map and grid
#' @param title main title
#' @param units_1 units suptitle for absolute difference
#' @param units_2 units suptitle for relative difference
#' @param lim_1 limits for absolute difference
#' @param lim_2 limits for relative difference
#' @param lines_1 primary map (sf class lines)
#' @param lines_2 secondary map (sf class lines)
#' @param save TRUE to save with automatic name, or a filename
#' @param w width for png function
#' @param h height for png function
#' @param pt fond size for png function
#' @param int interval for latitude and logitude axis
#' @param grid_int int passed to project_grid function
#' @param grid_lat_min min_lat passed to project_grid function
#' @param grid_lat_max max_lat passed to project_grid function
#' @param force_max module of upper and lower limits for percentage plot
#' @param ndig number of digits for lengend_range
#' @param ... arguments passed to raster_plot
#'
#' @importFrom eixport wrf_raster
#' @importFrom grDevices dev.off png
#' @import raster sf
#'
#' @export
#'
#' @examples
#' us    <- sf::read_sf(paste0(system.file("extdata",package="hackWRF"),"/us.shp"))
#' coast <- sf::read_sf(paste0(system.file("extdata",package="hackWRF"),"/coast.shp"))
#' A     <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/rasterA.Rds"))
#' B     <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/rasterB.Rds"))
#'
#' plot_diff(a = A, b = B,
#'           lines_1 = coast, lines_2 = us,
#'           var = 'E_PM25J', title = 'A - B',
#'           lim_1 = 0.00001,lim_2 = 1)
#'

plot_diff <- function(a,b,file_a,file_b,
                      var = '',map,operation = mean,
                      col,col_1,col_2,
                      title = '',
                      units_1 = expression(kton~yr^-1),
                      units_2 = expression("%"),
                      lim_1 = NA, lim_2 = NA,
                      lines_1, lines_2,
                      save,
                      w = 800, h = 1200, pt = 18,
                      int = 10,
                      grid_int = 15,
                      grid_lat_min = -80,
                      grid_lat_max = 80,
                      force_max,
                      ndig = 2,
                      ...){

  if(missingArg(col))
    col <- c("#1B2C62","#204385","#265CA9","#4082C2","#5DA9DB",
             "#80C4EA","#A4DDF7","#C1E7F8","#DEF2FA","#FFFFFF",
             "#FFFFFF","#FDF0B4","#FDDA7C","#FEBC48","#FB992F",
             "#F7762B","#E84E29","#D72828","#B81B22","#97161A")
  if(missingArg(col_1))
    col_1 = '#000000'
  if(missingArg(col_2))
    col_2 = '#666666'

  if(missingArg(a)){
    if(missingArg(map)){
      x  <- raster::calc(wrf_raster(file_a,var,verbose = T),operation)
    }else{
      x  <- raster::calc(wrf_raster(file_a,var,map = map,verbose = T),operation)
    }
  }else{
    x = a
  }

  if(missingArg(b)){
    if(missingArg(map)){
      y  <- raster::calc(wrf_raster(file_b,var,verbose = T),operation)
    }else{
      y  <- raster::calc(wrf_raster(file_b,var,map = map,verbose = T),operation)
    }
  }else{
    y = b
  }
  if(!missingArg(lines_1))
    map1_proj <- st_transform(lines_1, crs = crs(x, asText=T))
  if(!missingArg(lines_2))
    map2_proj <- st_transform(lines_2, crs = crs(x, asText=T))

  diff <- x - y
  rel  <- 100 * diff / (y + 0.00000000001)
  # rel  <- 100 * diff / (y + 1.00000000001)

  if(!missingArg(force_max)){
    rel[rel > force_max] = force_max
    # rel[rel < -force_max] = -force_max
  }

  if(is.na(lim_1[1]))
    lim_1 = cellStats(diff,'range')
  if(is.na(lim_2[1]))
    lim_2 = cellStats(rel,'range')

  lim_1[1] <- -max(abs(range(lim_1)))
  lim_1[2] <- max(abs(range(lim_1)))

  lim_2[1] <- -max(abs(range(lim_2)))
  lim_2[2] <- max(abs(range(lim_2)))

  if(!missingArg(save)){
    if(save[1] == TRUE){
      file_name = paste0('diff_',var,'.png')
    }else{
      file_name = save
    }
    cat('saving plot_diff in',file_name,'...\n')
    png(filename = file_name,
        width = w,
        height = h,
        pointsize = pt)
  }

  par(mfrow=c(2,1),mar=c(2.5,2.5,2,1))

  plot_raster(diff,axe = F, col = col,zlim = lim_1,...)
  longitude_proj(x,int = int);latitude_proj(x,int = int)
  grid_proj(x,lty = 2,col = col_2,
            int = grid_int,lat_min = grid_lat_min, lat_max = grid_lat_max)
  if(!missingArg(lines_2))
    lines(map2_proj,col = col_2)
  if(!missingArg(lines_1))
    lines(map1_proj, col = col_1)
  legend_range(diff,dig = c(ndig,ndig,ndig))
  mtext(units_1,adj = 1,cex = 1.2)
  mtext(bquote(.(var)),adj = 0,line = 0.25)
  title(title,line = 0.6)

  plot_raster(rel,axe = F, col = col,zlim = lim_2,...)
  longitude_proj(x,int = int);latitude_proj(x,int = int)
  grid_proj(x,lty = 2,col = col_2,
            int = grid_int,lat_min = grid_lat_min, lat_max = grid_lat_max)
  if(!missingArg(lines_2))
    lines(map2_proj,col = col_2)
  if(!missingArg(lines_1))
    lines(map1_proj, col = col_1)
  legend_range(rel,dig = c(ndig,ndig,ndig))
  mtext(units_2,adj = 1)
  mtext(bquote(.(var)),adj = 0, line = 0.25)

  if(!missingArg(save))
    garbage <- dev.off()
}
