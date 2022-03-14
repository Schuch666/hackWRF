args <- commandArgs(trailingOnly = TRUE)

library(hackWRF)

dir  <- args[1]

var  <- args[2]
if(length(args) > 2){
   ndim <- args[3]
}else{
   ndim <- '4d'
}

if(ndim == '&')
   ndim <- '4d'

stations <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/metar-dust2400.Rds"))
# stations <- readRDS("metar-dust2400.Rds")

files    <- dir(path = dir, pattern = "wrfout_d01",full.names = T)
extract_serie(filelist = files, 
              new      = T,
              point    = stations,
              variable = var, 
              field    = ndim, 
              prefix   = "metar.d01")

files    <- dir(path = dir, pattern = "wrfout_d02",full.names = T)
extract_serie(filelist = files, 
              new      = T,
              point    = stations,
              variable = var, 
              field    = ndim, 
              prefix   = "metar.d02")

files    <- dir(path = dir, pattern = "wrfout_d03",full.names = T)
extract_serie(filelist = files, 
              new      = T,
              point    = stations,
              variable = var, 
              field    = ndim, 
              prefix   = "metar.d03")
