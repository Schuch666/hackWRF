library(hackWRF)
library(openair)

WRF_folder   = 'METAR/WRF/'
METAR_folder = 'METAR/DATA/'

case         = 'jan_2022'
to.plot      =  TRUE
r_limit      =  NA
IOA_limit    =  NA   # only for wind direction

source("table_metar_T2.R")
source("table_metar_Q.R")
source("table_metar_Wind.R")
