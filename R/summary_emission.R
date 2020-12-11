#' Summary the emission from a emission file from WRF-Chem model
#'
#' @description Calculate the summary for each specie of gas and aerosol in a given emission file
#'
#' @param file file patch and name
#' @param chem_opt chemical mechanism option, default is CB05
#' @param remove_zeros remove empty values (all zeros) from table, default is TRUE
#' @param remove_dim remove dimension variables XLAT, XLON and Times from table,default is TRUE
#' @param pm calculate total PM, default is FALSE
#' @param voc calculate total voc, nox and nox/voc ratio and nox/voc ratio in propylene-eq
#' @param mask sf object MULTIPOLYGON from a shapefile, see notes and example
#' @param verbose display additional information
#'
#' @return a data.frame containing the polutant name, minimum, average, maximum, units, molar weight and total (in kt/year)
#'
#' @import ncdf4 units sf raster eixport
#'
#' @note If the mask argument is provided, only the inner region of the shapefile is used to determine the total emissions.
#'
#' @export
#'
#' @examples
#' # sample shapefile for filter
#' BR         <- sf::read_sf(paste0(system.file("extdata",package="hackWRF"),"/BR.shp"))
#' BR_regions <- sf::read_sf(paste0(system.file("extdata",package="hackWRF"),"/BR_regions.shp"))
#'
#' \dontrun{
#' file <- choose.files()
#'
#' # fast check for the entire file
#' summary_emission(file = file)
#'
#' # check using the BR
#' summary_emission(file = file,mask = BR)
#'
#' # check using the midwest region from BR_regions
#' midwest <- BR_regions[2,]
#' summary_emission(file = file,mask = midwest)
#' }
#'
summary_emission <- function(file,
                             chem_opt = 'cb05',
                             remove_zeros = TRUE,
                             remove_dim = TRUE,
                             pm = FALSE,
                             voc = FALSE,
                             mask = NULL,
                             verbose = F){

  make_mask <- function(r,s){
    return(raster::mask(r,s))
  }

  if(chem_opt == 'cb05'){
    MW <- sysdata$cb05
  }else{
    cat('option not suported!\n')
    return()
  }
  em   <- ncdf4::nc_open(file)
  var  <- names(em$var)
  if(remove_dim){
    var  <- grep('Times', var, invert = T, value = T)
    var  <- grep('XLAT' , var, invert = T, value = T)
    var  <- grep('XLONG', var, invert = T, value = T)
  }else{
    cat('under construction...\n')

    var  <- grep('Times', var, invert = T, value = T)
    var  <- grep('XLAT' , var, invert = T, value = T)
    var  <- grep('XLONG', var, invert = T, value = T)
  }
  n_times <- length( ncdf4::ncvar_get(nc = em, varid = 'Times') )
  dx      <- ncdf4::ncatt_get(em,varid = 0,attname = "DX")$value / 1000 # to km
  area    <- dx * dx # km2
  area_m2 <- dx * dx * 1000 * 1000
  if(verbose)
    cat(n_times,'times,',area,'km2\n')

  table <- data.frame(pollutant = var,
                      min   = rep(NA,length(var)),
                      med   = rep(NA,length(var)),
                      max   = rep(NA,length(var)),
                      units = rep(NA,length(var)),
                      mw    = rep(NA,length(var)),
                      total = rep(NA,length(var)),
                      stringsAsFactors = F)

  for(i in var){
    if(is.null(mask)){
      emiss_val  <- ncdf4::ncvar_get(nc = em, varid = i)
    }else{
      emiss_val  <- eixport::wrf_get(file,i,as_raster = T)
      emiss_val  <- make_mask(emiss_val,mask)
      emiss_val  <- raster::as.array(emiss_val)
    }

    media      <- mean(emiss_val, na.rm = T)
    maximo     <- max(emiss_val,  na.rm = T)
    minimo     <- min(emiss_val, na.rm = T)
    emiss_unit <- ncdf4::ncatt_get(nc = em, varid = i, attname = 'units')
    if(verbose)
      cat(i,'units',emiss_unit$value,'\n')
    if(maximo == 0){
      if(verbose)
        cat('no emission\n')
      table[table$pollutant == i,]$min    <- 0
      table[table$pollutant == i,]$med    <- 0
      table[table$pollutant == i,]$max    <- 0
      table[table$pollutant == i,]$units  <- emiss_unit$value
      table[table$pollutant == i,]$mw     <- '-'
      table[table$pollutant == i,]$total  <- set_units(0,'t year-1')
    }else{
      if(emiss_unit$value == "mol km^-2 hr^-1"){
        mw    <- MW$g_mol[MW$group == i]
        if(verbose)
          cat('molar mass:',mw,'\n')
        emission <- emiss_val * mw * area              # [MOL km-2 h-1 * g/MOL * km2] = [g / h]
        total    <- sum(emission, na.rm = T) / n_times # total [g/h]
        total    <- total / (1000 * 1000 * 1000)       # [g/h] to [kt/h]
        total    <- total * 24 * 365                   # [t/h] to [kt/year]
        total    <- as_units(total,'kt year-1')
      }else{
        if(verbose)
          cat('aerossol\n')
        mw       <- 1
        emission <- emiss_val * area_m2                # [ug m-2 s-1 * m2] = [ug/s]
        emission <- emission * 60 * 60                 # [ug/s] to [ug/h]
        emission <- emission / (1000 * 1000)           # [ug/h] to [g/h]
        total    <- sum(emission, na.rm = T) / n_times # total [g/h]
        total    <- total / (1000 * 1000 * 1000)       # [g/h] to [kt/h]
        total    <- total * 24 * 365                   # [kt/h] to [kt/year]
        total    <- as_units(total,'kt year-1')
      }
      cat(paste0('total ',i,': '))
      print(total)

      table[table$pollutant == i,]$min    <- minimo
      table[table$pollutant == i,]$med    <- media
      table[table$pollutant == i,]$max    <- maximo
      table[table$pollutant == i,]$units  <- emiss_unit$value
      table[table$pollutant == i,]$mw     <- sprintf("%.1f",mw)
      table[table$pollutant == i,]$total  <- total
      rm(media,total,minimo,maximo,mw)
    }
  }
  ncdf4::nc_close(em)

  table$total_units <- rep('Kt/year',nrow(table))

  # to check with Mario script
  # table$mario       <- table$total/365  # to match Mario script
  # table$mario_units <- rep('Kt/day',nrow(table))

  if(pm){
    total_pm   <- sum(table[table$units == 'ug m^-2 s^-1',]$total) # all emissions for aerosol
    pm25       <- table[table$units == 'ug m^-2 s^-1',]$pollutant  # selecting all aerosol
    pm25       <- grep("E_PM10",pm25,value = T,invert = T)         # excluding pm10
    pm25       <- grep("E_PM_10",pm25,value = T,invert = T)
    if(verbose)
      cat('using:',pm25,'for PM25\n')
    total_pm25 <- sum(table[table$pollutant %in% pm25,]$total)

    table_pm   <-data.frame(pollutant = c('total_pm25','total_pm'),
                            min   = c(NA,NA),
                            med   = c(NA,NA),
                            max   = c(NA,NA),
                            units = c('-','-'),
                            mw    = c('-','-'),
                            total = c(total_pm25,total_pm),
                            total_units = c('Kt/year','Kt/year'),
                            stringsAsFactors = F)

    table <- rbind(table,table_pm)
  }

  if(voc){
    total_nox  <- table[table$pollutant == 'E_NO',]$total + table[table$pollutant == 'E_NO2',]$total
    voc        <- table[table$units == 'mol km^-2 hr^-1',]$pollutant # select all emission for gases
    voc        <- grep("E_CO",voc,value = T,invert = T)              # exclude inorganic species of cb05
    voc        <- grep("E_NO",voc,value = T,invert = T)
    voc        <- grep("E_NO2",voc,value = T,invert = T)
    voc        <- grep("E_SO2",voc,value = T,invert = T)
    voc        <- grep("E_NH3",voc,value = T,invert = T)
    voc        <- grep("E_HCL",voc,value = T,invert = T)
    voc        <- grep("E_PSULF",voc,value = T,invert = T)
    if(verbose)
      cat('using:',voc,'for VOC\n')
    total_voc    <- sum(table[table$pollutant %in% voc,]$total)
    total_voc_eq <- 0
    for(i in voc){
      if(verbose)
        cat(i,
            'total',
            table[table$pollutant == i,]$total,
            'prop_eq',
            MW[MW$group == i,]$prop_eq,
            '\n')
      total_voc_eq = total_voc_eq + table[table$pollutant == i,]$total * MW[MW$group == i,]$prop_eq
    }

    table_voc   <-data.frame(pollutant = c('total_NOx',
                                           'total_VOC','total_VOCeq',
                                           'VOC_NOx','VOCeq_NOx'),
                             min   = c(NA,NA,NA,NA,NA),
                             med   = c(NA,NA,NA,NA,NA),
                             max   = c(NA,NA,NA,NA,NA),
                             units = c('-','-','-','-','-'),
                             mw    = c('-','-','-','-','-'),
                             total = c(total_nox,
                                       total_voc,total_voc_eq,
                                       total_voc/total_nox,total_voc_eq/total_nox),
                             total_units = c('Kt/year','Kt/year','Kt/year','-','-'),
                             stringsAsFactors = F)

    table <- rbind(table,table_voc)
  }

  if(remove_zeros){
    table <- table[table$total != 0,]
  }

  return(table)
}
