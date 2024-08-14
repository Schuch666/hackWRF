## evaluation script for precipitation using METAR for US
## modified by Eeshan Basu 2024-08-14
## Thanks for the contribution

library(hackWRF)
library(openair)
library(dplyr)
library(lubridate)

WRF_folder   = 'E:/Eeshan/METAR/WRFCHEMGHG/'
METAR_folder = 'E:/Eeshan/METAR/DATA/'
#cases = c("DJF","MAM","JJA","SON") # for seasonal evaluation
case = c("Annual")
years = c("2012", "2013", "2014", "2015", "2016", "2017")

for (year in years) {

  setwd(paste0(WRF_folder,case))
  print(case)
  print(year)

  to.plot      =  TRUE
  r_limit      =  NA
  IOA_limit    =  NA

  # reading in the rainc data
  model_d01 <- readRDS(paste0('metar.d01.',year,'.RAINC.Rds'))
  model_d02 <- readRDS(paste0('metar.d02.',year,'.RAINC.Rds'))
  model_d03 <- readRDS(paste0('metar.d03.',year,'.RAINC.Rds'))

  # reading in the rainnc data
  model_d01_rainnc <- readRDS(paste0('metar.d01.',year,'.RAINNC.Rds'))
  model_d02_rainnc <- readRDS(paste0('metar.d02.',year,'.RAINNC.Rds'))
  model_d03_rainnc <- readRDS(paste0('metar.d03.',year,'.RAINNC.Rds'))

  rain <- function(x){
    rain    <- x
    for(i in 1:(length(x)-1)){
      rain[i+1] = x[i+1] - x[i]
    }
    rain[1] <- NA
    rain[ rain < 0 ] = NA # to remove negative values
    return(rain)
  }

  for(i in colnames(model_d01)[-1]){
    model_d01[,i] <- rain(model_d01[,i]) + rain(model_d01_rainnc[,i])
  }

  for(i in colnames(model_d02)[-1]){
    model_d02[,i] <- rain(model_d02[,i]) + rain(model_d02_rainnc[,i])
  }

  for(i in colnames(model_d03)[-1]){
    model_d03[,i] <- rain(model_d03[,i]) + rain(model_d03_rainnc[,i])
  }

  plot(model_d01[,3], ty = 'l') #checking whether there are no negative values

  files_obs <- dir(path = METAR_folder,
                   pattern = 'METAR',
                   full.names = T) #listing all the file names

  obs <- data.frame(date = model_d01$date,
                    stringsAsFactors = T) #creating a dataframe with just the date

  for(i in 1:length(files_obs)){
    cat('open',files_obs[i],i,'of',length(files_obs),'\n')
    new        <- readRDS(files_obs[i])
    name       <- new$station[1]
    new        <- new[,c(1,13)] #selecting the start date and the rain from the observations
    names(new) <- c('date',name)

    new        <- selectByDate(new,
                               start = as.Date(model_d01$date[1]),
                               end   = as.Date(last(model_d01$date)))

    new <- new[!duplicated(new$date), ]
    obs <- merge(obs,
                 new,
                 by = "date",
                 all.x = T)
  }

  names(obs) <- c('date',
                  substr(files_obs,
                         nchar(METAR_folder)+7,
                         nchar(files_obs)-4))
  observed   <- obs

  observed[-1] <- 25.4 * observed[-1] # convert inches to mm

  # skip the time average step to evaluate hourly precipitation
  # currently the script is measuring daily precipitation
  observed <- timeAverage(observed,avg.time = 'day')
  observed <- as.data.frame(observed)

  # Function to convert to numeric in case something gets messed up
  # # Convert all columns except the first to numeric
  # observed[, -1] <- lapply(observed[, -1], function(x) {
  #   if (is.factor(x)) {
  #     as.numeric(as.character(x))
  #   } else if (is.character(x)) {
  #     as.numeric(x)
  #   } else {
  #     x  # Return as-is if already numeric
  #   }
  # })

  model_d01    <- timeAverage(model_d01,avg.time = 'day')
  model_d01 <- as.data.frame(model_d01)
  model_d02    <- timeAverage(model_d02,avg.time = 'day')
  model_d02 <- as.data.frame(model_d02)
  model_d03   <- timeAverage(model_d03,avg.time = 'day')
  model_d03 <- as.data.frame(model_d03)

  cat("rain:\n")

  mod_stats_d01 <- evaluation(model_d01,
                              observed,
                              names(model_d01)[2])

  for(i in names(model_d01)[c(-1,-2)]){
    mod_stats_d01 <- evaluation(model_d01,
                                observed,
                                table = mod_stats_d01,
                                station = i,
                                clean = T)
  }

  mod_stats_d01   <- evaluation(model_d01,
                                observed,
                                table = mod_stats_d01,
                                station = 'ALL')

  if(!is.na(r_limit))
  mod_stats_d01   <- mod_stats_d01[mod_stats_d01$r >= r_limit,]
  mod_stats_d01   <- evaluation(table = mod_stats_d01,
                                summaryze = T)
  cat('...\n')
  print(tail(mod_stats_d01))
  cat('\n')

  cat("Precipitation for d02:\n")
  mod_stats_d02 <- evaluation(model_d02,
                              observed,
                              names(model_d02)[2])

  for(i in names(model_d02)[c(-1,-2)]){
    mod_stats_d02 <- evaluation(model_d02,
                                observed,
                                table = mod_stats_d02,
                                station = i,
                                clean = T)
  }

  mod_stats_d02   <- evaluation(model_d02,
                                observed,
                                table = mod_stats_d02,
                                station = 'ALL')

  if(!is.na(r_limit))
  mod_stats_d02   <- mod_stats_d02[mod_stats_d02$r >= r_limit,]
  mod_stats_d02   <- evaluation(table = mod_stats_d02,
                                summaryze = T)
  cat('...\n')
  print(tail(mod_stats_d02))
  cat('\n')

  cat("Precipitation for d03:\n")
  mod_stats_d03 <- evaluation(model_d03,
                              observed,
                              names(model_d03)[2])

  for(i in names(model_d03)[c(-1,-2)]){
    mod_stats_d03 <- evaluation(model_d03,
                                observed,
                                table = mod_stats_d03,
                                station = i,
                                clean = T)
  }

  mod_stats_d03   <- evaluation(model_d03,
                                observed,
                                table = mod_stats_d03,
                                station = 'ALL')

  mod_stats_d03   <- evaluation(table = mod_stats_d03,
                                summaryze = T)
  cat('...\n')
  print(tail(mod_stats_d03))
  cat('\n')

  write.stat(stat = mod_stats_d01,
             file = paste0(WRF_folder,case,'/stats.metar.',year,'.RAIN.d01.csv'))
  write.stat(stat = mod_stats_d02,
             file = paste0(WRF_folder,case,'/stats.metar.',year,'.RAIN.d02.csv'))
  write.stat(stat = mod_stats_d03,
             file = paste0(WRF_folder,case,'/stats.metar.',year,'.RAIN.d03.csv'))

  # new summary + fair comparison for d01 / d02 / d03
  summary_stats <- rbind('d01 in d01' = evaluation(model_d01,observed,fair = model_d01),
                         'd01 in d02' = evaluation(model_d01,observed,fair = model_d02),
                         'd02 in d02' = evaluation(model_d02,observed,fair = model_d02),
                         'd01 in d03' = evaluation(model_d01,observed,fair = model_d03),
                         'd02 in d03' = evaluation(model_d02,observed,fair = model_d03),
                         'd03 in d03' = evaluation(model_d03,observed,fair = model_d03))

  print(summary_stats)
  summary_stats <- round(summary_stats, digits = 2) #rounding it to 2 places of decimal

  write.stat(stat = summary_stats,
             file = paste0(WRF_folder,case,'/stats.metar.',year,'.RAIN.all.csv'))

  #RDS for time series plots
  saveRDS(observed,file = paste0("observed.",year,".RAIN.Rds"))
  saveRDS(model_d01,file = paste0("model_d01.",year,".RAIN.Rds"))
  saveRDS(model_d02,file = paste0("model_d02.",year,".RAIN.Rds"))
  saveRDS(model_d03,file = paste0("model_d03.",year,".RAIN.Rds"))
  saveRDS(mod_stats_d01,file = paste0("mod_stats_d01.",year,".RAIN.Rds"))
  saveRDS(mod_stats_d02,file = paste0("mod_stats_d02.",year,".RAIN.Rds"))
  saveRDS(mod_stats_d03,file = paste0("mod_stats_d03.",year,".RAIN.Rds"))

}
