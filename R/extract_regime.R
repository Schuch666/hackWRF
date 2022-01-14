#' Creata a NetCDF file with the ratio of FORM/(NO2 + O3)
#'
#' @description Read and calculate the mean value of a variable from a list of wrf output files.
#'
#' @param filelist list of files to be read
#' @param variable_mon variable name for nominador
#' @param variable_dem variable name for denominador
#' @param field '4d' (default), '3d', '2d' or '2dz' see notes
#' @param prefix to output file, defolt is serie
#' @param units units on netcdf file (default is ppmv)
#' @param meta use Times, XLONG and XLAT data (only work with 2d variable for file)
#' @param filename name for the file, in this case prefix is not used
#' @param verbose display additional information
#'
#' @note The field argument '4d' / '2dz' is used to read a 4d/3d variable droping the 3rd dimention (z).
#' @note ratio < 0.28, VOC-limited, +VOC emissions -> +O3 concentration
#' @note ratio > 0.28, NOx-limited, +NOx emissions -> +O3 concentration
#'
#' @references Sillman, S. (1995). The use of NO y, H2O2, and HNO3 as indicators
#' for ozone‐NO x‐hydrocarbon sensitivity in urban locations. Journal of
#' Geophysical Research: Atmospheres, 100(D7), 14175-14188.
#'
#' @import ncdf4
#' @import eixport
#'
#' @export
#'

extract_regime <- function(filelist, variable_mon = "FORM",
                           variable_dem = c('no2','o3'), field = "4d",
                           prefix = "ratio", units = "", meta = T,
                           filename, verbose = TRUE){

  variable <- 'ratio'

  if(missing(filename)){
    output_filename   <- paste0(prefix,'.','.nc')
  }else{
    output_filename   <- filename
  }

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
    cat('extracting mean of FORM / (NO2 + O3)\n')
    cat('reading:',filelist[1],'file 1 of',length(filelist),'\n')
  }

  w     <- nc_open(filename = filelist[1])
  # this part need to improve
  NOM   <- ncvar_get(w,variable_mon,   count = contagem)
  DEN_1 <- ncvar_get(w,variable_dem[1],count = contagem)
  DEN_2 <- ncvar_get(w,variable_dem[2],count = contagem)
  # ration of FORM / (NO2 + O3)
  VAR   <- NOM / (DEN_1 + DEN_2)

  if(meta){
    times <- ncvar_get(w,"Times")
  }
  nc_close(w)

  if(meta){
    acu_times <- length(times)
  }else{
    acu_times <- 1
  }

  tsum <- function(var){
    if(length(dim(var)) == 2){
      cat('min:',min(var,na.rm = T),'mean:',mean(var,na.rm = T),'max:',max(var,na.rm = T),'\n')
      return(var)
    }

    t_sum   <- var[,,1,drop = T]
    cat('min:',min(var,na.rm = T),'mean:',mean(var,na.rm = T),'max:',max(var,na.rm = T),'\n')

    for(i in 1:dim(var)[1]){
      for(j in 1:dim(var)[2]){
        t_sum[i,j] <- sum(var[i,j,], na.rm = T)
      }
    }

    return(t_sum)
  }

  SUM   <- tsum(VAR)

  if(length(filelist) > 1){
    for(i in 2:length(filelist)){
      cat('reading:',filelist[i],'file',i,'of',length(filelist),'\n')

      w    <- nc_open(filename = filelist[i])
      # TEMP <- ncvar_get(w,variable,count = contagem)
      NOM   <- ncvar_get(w,variable_mon,   count = contagem)
      DEN_1 <- ncvar_get(w,variable_dem[1],count = contagem)
      DEN_2 <- ncvar_get(w,variable_dem[2],count = contagem)
      # ration of FORM / (NO2 + O3)
      TEMP  <- NOM / (DEN_1 + DEN_2)

      INC  <- tsum(TEMP)
      if(meta){
        times <- ncvar_get(w,"Times")
        acu_times = acu_times + length(times)
      }else{
        acu_times = acu_times + 1
      }
      nc_close(w)
      SUM <- SUM + INC
    }
  }

  MEAN <- SUM / acu_times

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
                     MEAN)
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
    g_atributos  <- c( list(TITLE = paste0('average of ',variable),
                            History = paste("created on",
                                            format(Sys.time(),
                                                   "%Y-%m-%d at %H:%M")),
                            Author = "Schuch"),
                       g_atributos[4:length(g_atributos)])

    #distentions
    west_east <- ncdf4::ncdim_def("west_east",
                                  units = "",
                                  longname = "",
                                  vals = 1:dim(MEAN)[1])
    south_north <- ncdf4::ncdim_def("south_north",
                                    units = "",
                                    longname = "",
                                    vals = 1:dim(MEAN)[2])
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
                     MEAN)
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "MemoryOrder",
                     attval = "XYZ")
    ncdf4::ncatt_put(output_file,
                     varid = variable,
                     attname = "description",
                     attval = "average")
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
