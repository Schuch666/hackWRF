library(hackWRF)

f          <- 'C:/Users/schuch/Downloads/'
file_waccm <- dir(path = f,pattern = '00000.nc',full.names = T)

cat('example of 3 CETESB stations at Sao Paulo-Brazil\n')
site_list <- data.frame(lat = c(-22.72500,-22.94673,-23.61632),
                        lon = c(-47.34800,-47.11928,-46.66347),
                        row.names = c('Americana','Campinas','Congonhas'),
                        stringsAsFactors = F)
print(site_list)

cat('longitude in WACCM is 0 to 360\n')
site_list$lon = site_list$lon + 180

VAR = 'CO'
extract_serie(filelist     = file_waccm,        # list of WACCM files
              point        = site_list,         # data.frame w site list
              variable     = VAR,               # variable name
              latitude     = 'lat',             # name of lat coord
              longitude    = 'lon',             # name of lon coord
              use_datesec  = T,                 # variables to date / time
              prefix       = paste0(f,'serie'), # for the output file
              new          = T)                 # flag to create a new file

cat('\nopening and printing the series:\n\n')
TEST <- readRDS(paste0(f,'serie.',VAR,'.Rds'))  # reading the serie
print(TEST)                                     # printing the serie
