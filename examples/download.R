library('riem')

dir.create('DATA',showWarnings = F)

sites <- c("OAHR","OAIX","OAJL","OAKB","OAKN")

for(site in sites){
  cat('downloading METAR from:',site,'...\n')

  DATA <- riem_measures(station    = site,
                        date_start = "2017-01-01",
                        date_end   = "2022-02-01")

  DATA <- as.data.frame(DATA)

  new_data <- DATA$valid
  for(i in 1:length(DATA$valid)){
    min <- as.numeric(format(DATA$valid[i], format = '%M'))
    if(min >= 30){
      new_data[i] <- DATA$valid[i] - min * 60
    }else{
      new_data[i] <- DATA$valid[i] + min * 60
    }
  }

  DATA2 <- data.frame(date    = new_data,             # processed date
                      o_date  = DATA$valid,           # original date
                      station = DATA$station,         # station code
                      lon     = DATA$lon,             # longitude
                      lat     = DATA$lat,             # latitude
                      T2      = 5/9 * (DATA$tmpf-32), # Fahrenheit to Celcius
                      TD      = 5/9 * (DATA$dwpf-32), # Fahrenheit to Celcius
                      feel    = 5/9 * (DATA$feel-32), # Fahrenheit to Celcius
                      RH      = DATA$relh,            # relative humidity
                      WS      = 0.514444 * DATA$sknt, # Knots to m/s
                      WD      = DATA$drct,            # wind direction degrees N
                      P       = DATA$mslp,            # pressure
                      rain    = DATA$p01i)            # precipitation

  saveRDS(object = DATA2,file = paste0('DATA/METAR.',site,'.Rds'))
}

cat('done!')
