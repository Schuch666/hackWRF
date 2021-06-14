#' Creata a NetCDF file with the surface maximum
#'
#' @description Read and calculate the maximum value of a variable from a list of wrf output files.
#'
#' @param filelist list of files to be read
#' @param variable variable name
#' @param field '4d' (default), '3d', '2d' or '2dz' see notes
#' @param prefix to output file, defolt is serie
#' @param units units on netcdf file (default is ppmv)
#' @param meta use Times, XLONG and XLAT data (only work with 2d variable for file)
#' @param verbose display additional information
#'
#' @note The field argument '4d' / '2dz' is used to read a 4d/3d variable droping the 3rd dimention (z).
#'
#' @import ncdf4
#' @import eixport
#'
#' @export
#'
#' @examples
#' dir.create(file.path(tempdir(), "MAX"))
#' folder <- system.file("extdata",package="hackWRF")
#' wrf_file <- paste0(folder,"/wrf.day1.o3.nc")
#' extract_max(filelist = wrf_file,prefix = paste0(file.path(tempdir(),"MAX"),'/mean'))
#'

extract_max <- function(filelist, variable = "o3", field = "4d",
                         prefix = "max", units = "ppmv", meta = T,verbose = TRUE){

  output_filename   <- paste0(prefix,'.',variable,'.nc')

  COMPRESS <- NA
  acu_times <- 0

  if(!meta)
    field = '2d'

  if(field == '2d')
    contagem  = NA             # 2d Field (x,y)
  if(field == '2dz')
    contagem = c(-1,-1,1)      # 3d Field (x,y,z)
  if(field == '3d')
    contagem  = NA             # 3d Field (x,y,t)
  if(field == '4d')
    contagem = c(-1,-1,1,-1)   # 4d Field (x,y,z,t)

  if(verbose){
    cat('extracting max of',variable,'field',field,'\n')
    cat('reading:',filelist[1],'file 1 of',length(filelist),'\n')
  }

  w     <- nc_open(filename = filelist[1])
  VAR   <- ncvar_get(w,variable,count = contagem)
  if(meta){
    times <- ncvar_get(w,"Times")
  }
  nc_close(w)

  if(meta){
    acu_times <- length(times)
  }else{
    acu_times <- 1
  }

  tmax <- function(var){
    if(length(dim(var)) == 2){
      cat('min:',min(var,na.rm = T),'mean:',mean(var,na.rm = T),'max:',max(var,na.rm = T),'\n')
      return(var)
    }

    t_max   <- var[,,1,drop = T]
    cat('min:',min(var,na.rm = T),'mean:',mean(var,na.rm = T),'max:',max(var,na.rm = T),'\n')

    for(i in 1:dim(var)[1]){
      for(j in 1:dim(var)[2]){
        t_max[i,j] <- max(var[i,j,], na.rm = T)
      }
    }

    return(t_max)
  }

  tmax2 <- function(var,var2){
    t_max <- var
    cat('min:',min(var2,na.rm = T),'mean:',mean(var2,na.rm = T),'max:',max(var2,na.rm = T),'\n')
    for(i in 1:dim(var)[1]){
      for(j in 1:dim(var)[2]){
        t_max[i,j] <- max(c(var[i,j],var2[i,j,]),na.rm = T)
      }
    }
    return(t_max)
  }

  MAX   <- tmax(VAR)

  if(length(filelist) > 1){
    for(i in 2:length(filelist)){
      cat('reading:',filelist[i],'file',i,'of',length(filelist),'\n')
      w    <- nc_open(filename = filelist[i])
      TEMP <- ncvar_get(w,variable,count = contagem)
      MAX  <- tmax2(MAX,TEMP)
      if(meta){
        times <- ncvar_get(w,"Times")
        acu_times = acu_times + length(times)
      }else{
        acu_times = acu_times + 1
      }
      nc_close(w)
    }
  }

  # some input
  wrfinput     <- nc_open(filelist[1])
  if(meta){
    input_time   <- ncdf4::ncvar_get(wrfinput, "Times")
    input_time   <- input_time[1] # first time / first file

    input_lat    <- ncdf4::ncvar_get(wrfinput, "XLAT")  # get lat / lon
    input_lon    <- ncdf4::ncvar_get(wrfinput, "XLONG")
    if(length(dim(input_lon)) > 2){
      input_lat    <- input_lat[,,1,drop = T] # drop time from original lat / lon
      input_lon    <- input_lon[,,1,drop = T]
    }

    g_atributos  <- ncdf4::ncatt_get(wrfinput, 0)
    g_atributos  <- c( list(TITLE = paste0('mean ',variable),
                            History = paste("created on",
                                            format(Sys.time(),
                                                   "%Y-%m-%d at %H:%M")),
                            Author = "Schuch"),
                       g_atributos[4:length(g_atributos)])

    #dimentions
    west_east <- ncdf4::ncdim_def("west_east",
                                  units = "",
                                  longname = "",
                                  vals = 1:g_atributos$`WEST-EAST_PATCH_END_UNSTAG`)
    south_north <- ncdf4::ncdim_def("south_north",
                                    units = "",
                                    longname = "",
                                    vals = 1:g_atributos$`SOUTH-NORTH_PATCH_END_UNSTAG`)
    bottom_top <- ncdf4::ncdim_def("bottom_top",
                                   units = "",
                                   longname = "",
                                   vals = 1)
    DateStrLen          <- ncdf4::ncdim_def("DateStrLen",
                                            units = "",
                                            longname = "",
                                            vals = 1:19)
    Time                <- ncdf4::ncdim_def("Time",
                                            units = "",
                                            longname = "",
                                            vals = 1,
                                            unlim = TRUE)
    # variables
    Times <- ncdf4::ncvar_def(name = "Times",
                              dim = list(DateStrLen,Time),
                              units = "",
                              prec = "char",
                              compression = COMPRESS)
    XLONG <- ncdf4::ncvar_def(name = "XLONG",
                              units = "",
                              dim = list(west_east,south_north),
                              prec = "float",
                              compression = COMPRESS)
    XLAT <- ncdf4::ncvar_def(name = "XLAT" ,
                             units = "",
                             dim = list(west_east, south_north),
                             prec = "float",
                             compression = COMPRESS)

    mean <- ncdf4::ncvar_def(name = variable,
                             units = "",
                             dim = list(west_east,
                                        south_north,
                                        bottom_top,
                                        Time),
                             prec="float",
                             compression = COMPRESS)

    output_file <- nc_create(filename = output_filename,
                             vars = c(list('Times' = Times,
                                           'XLAT'  = XLAT,
                                           'XLONG' = XLONG,
                                           variable = mean)))

    ncdf4::ncvar_def(name = variable,
                     units = "",
                     dim = list(west_east,
                                south_north,
                                bottom_top,
                                Time),
                     prec="float",
                     compression = COMPRESS)

    for(i in 1:length(g_atributos)){
      ncdf4::ncatt_put(output_file,
                       varid = 0,
                       attname = names(g_atributos)[i],
                       attval = g_atributos[[i]])
    }
    # values for the basic variables
    ncdf4::ncvar_put(output_file,
                     "Times",
                     input_time)
    ncdf4::ncvar_put(output_file,
                     "XLONG",
                     input_lon)
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "MemoryOrder",
                     attval = "XY")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "description",
                     attval = "LONGITUDE, WEST IS NEGATIVE")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "units",
                     attval = "degree east")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "stagger",
                     attval = "")
    ncdf4::ncatt_put(output_file,
                     varid = "XLONG",
                     attname = "FieldType",
                     attval = 104)
    ncdf4::ncvar_put(output_file,
                     "XLAT",
                     input_lat)
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "MemoryOrder",
                     attval = "XY")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "description",
                     attval = "LATITUDE, SOUTH IS NEGATIVE")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "units",
                     attval = "degree north")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "stagger",
                     attval = "")
    ncdf4::ncatt_put(output_file,
                     varid = "XLAT",
                     attname = "FieldType",
                     attval = 104)
    # to the variable
    ncdf4::ncvar_put(output_file,
                     varid = variable,
                     MAX)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "MemoryOrder",
                     attval = "XYZ")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "description",
                     attval = "mean value")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "units",
                     attval = units)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "stagger",
                     attval = "Z")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "FieldType",
                     attval = 104)
  }else{
    # global attributes
    g_atributos  <- ncdf4::ncatt_get(wrfinput, 0)
    g_atributos  <- c( list(TITLE = paste0('mean ',variable),
                            History = paste("created on",
                                            format(Sys.time(),
                                                   "%Y-%m-%d at %H:%M")),
                            Author = "Schuch"),
                       g_atributos[4:length(g_atributos)])

    #distentions
    west_east <- ncdf4::ncdim_def("west_east",
                                  units = "",
                                  longname = "",
                                  vals = 1:dim(MAX)[1])
    south_north <- ncdf4::ncdim_def("south_north",
                                    units = "",
                                    longname = "",
                                    vals = 1:dim(MAX)[2])
    bottom_top <- ncdf4::ncdim_def("bottom_top",
                                   units = "",
                                   longname = "",
                                   vals = 1)
    DateStrLen          <- ncdf4::ncdim_def("DateStrLen",
                                            units = "",
                                            longname = "",
                                            vals = 1:19)
    Time                <- ncdf4::ncdim_def("Time",
                                            units = "",
                                            longname = "",
                                            vals = 1,
                                            unlim = TRUE)
    # variables
    mean <- ncdf4::ncvar_def(name = variable,
                             units = "",
                             dim = list(west_east,
                                        south_north,
                                        bottom_top,
                                        Time),
                             prec="float",
                             compression = COMPRESS)

    output_file <- nc_create(filename = output_filename,
                             vars = c(list(variable = mean)))

    ncdf4::ncvar_def(name = variable,
                     units = "",
                     dim = list(west_east,
                                south_north,
                                bottom_top,
                                Time),
                     prec="float",
                     compression = COMPRESS)

    for(i in 1:length(g_atributos)){
      ncdf4::ncatt_put(output_file,
                       varid = 0,
                       attname = names(g_atributos)[i],
                       attval = g_atributos[[i]])
    }
    # to the variable
    ncdf4::ncvar_put(output_file,
                     varid = variable,
                     MAX)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "MemoryOrder",
                     attval = "XYZ")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "description",
                     attval = "mean value")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "units",
                     attval = units)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "stagger",
                     attval = "Z")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "FieldType",
                     attval = 104)

  }


  ncdf4::nc_close(output_file)

  if(verbose){
    cat('total times:',acu_times,'\n')
    cat('output:',output_filename,'\n')
  }

}
