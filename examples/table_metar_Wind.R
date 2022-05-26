# library(hackWRF)
# library(openair)
#
# WRF_folder   = 'METAR/WRF/'
# METAR_folder = 'METAR/DATA/'
#
# case         = 'jan_2022'
# to.plot      =  TRUE
# r_limit      =  NA
# IOA_limit    =  NA   # only for wind direction

setwd(paste0(WRF_folder,case))

cat('calculating WS / WD for d01...\n')
U <- readRDS('metar.d01.U10.Rds')
V <- readRDS('metar.d01.V10.Rds')
model_d01_WS <- U  # get dim and times
model_d01_WD <- U  # get dim and times
WS <- U[,-1]
WD <- U[,-1]
U  <- U[,-1]
V  <- V[,-1]
# wind_factor <- log(20) / log(100) # d = 0, z0 = 0.1, z1 = 10, z2 = 10
for(i in 1:ncol(WS)){
  for(j in 1:nrow(WS)){
    WS[j,i] <- (U[j,i]^2 + V[j,i]^2)^(0.5)
    # WS[j,i] = WS[j,i] * wind_factor
    WD[j,i] <- (180/pi) * atan2(U[j,i],V[j,i]) + 180
  }
}
model_d01_WS[,-1]  <- WS
model_d01_WD[,-1]  <- WD

cat('calculating WS / WD for d02...\n')
U <- readRDS('metar.d02.U10.Rds')
V <- readRDS('metar.d02.V10.Rds')
model_d02_WS <- U  # get dim and times
model_d02_WD <- U  # get dim and times
WS <- U[,-1]
WD <- U[,-1]
U  <- U[,-1]
V  <- V[,-1]
# wind_factor <- log(20) / log(100) # d = 0, z0 = 0.1, z1 = 10, z2 = 10
for(i in 1:ncol(WS)){
  for(j in 1:nrow(WS)){
    WS[j,i] <- (U[j,i]^2 + V[j,i]^2)^(0.5)
    # WS[j,i] = WS[j,i] * wind_factor
    WD[j,i] <- (180/pi) * atan2(U[j,i],V[j,i]) + 180
  }
}
model_d02_WS[,-1]  <- WS
model_d02_WD[,-1]  <- WD

cat('calculating WS / WD for d03...\n')
U <- readRDS('metar.d03.U10.Rds')
V <- readRDS('metar.d03.V10.Rds')
model_d03_WS <- U  # get dim and times
model_d03_WD <- U  # get dim and times
WS <- U[,-1]
WD <- U[,-1]
U  <- U[,-1]
V  <- V[,-1]
# wind_factor <- log(20) / log(100) # d = 0, z0 = 0.1, z1 = 10, z2 = 10
for(i in 1:ncol(WS)){
  for(j in 1:nrow(WS)){
    WS[j,i] <- (U[j,i]^2 + V[j,i]^2)^(0.5)
    # WS[j,i] = WS[j,i] * wind_factor
    WD[j,i] <- (180/pi) * atan2(U[j,i],V[j,i]) + 180
  }
}
model_d03_WS[,-1]  <- WS
model_d03_WD[,-1]  <- WD

cat('opening Wind speed METAR:\n')
files_obs <- dir(path = METAR_folder,pattern = 'METAR',full.names = T)
obs       <- data.frame(date = model_d01_WS$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat('open',files_obs[i],i,'of',length(files_obs),'\n')
  new        <- readRDS(files_obs[i])
  name       <- new$station[1]
  new        <- new[,c(1,10)]
  names(new) <- c('date',name)

  new        <- selectByDate(new,
                             start = as.Date(model_d01_WS$date[1]),
                             end   = as.Date(last(model_d01_WS$date)))

  new <- new[!duplicated(new$date), ]
  obs <- merge(obs, new, by = "date",all.x = T)
}
names(obs) <- c('date',substr(files_obs,nchar(METAR_folder)+7,nchar(files_obs)-4))
observed   <- obs

# remove obs > 25 m/s (90 km/h)
DATA            <- observed[-1]
DATA[DATA < 0 ] <- NA
DATA[DATA > 20] <- NA
observed[-1]    <- DATA
rm(DATA)

cat("WS for d01:\n")
mod_stats_d01 <- evaluation(model_d01_WS,observed,names(model_d01_WS)[2])
for(i in names(model_d01_WS)[c(-1,-2)]){
  mod_stats_d01 <- evaluation(model_d01_WS,observed,
                              table = mod_stats_d01, station = i,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d01   <- mod_stats_d01[mod_stats_d01$r >= r_limit,]
mod_stats_d01   <- evaluation(table = mod_stats_d01,summaryze = T)
cat('...\n')
print(tail(mod_stats_d01))
cat('\n')

cat("WS for d02:\n")
mod_stats_d02 <- evaluation(model_d02_WS,observed,names(model_d02_WS)[2])
for(i in names(model_d02_WS)[c(-1,-2)]){
  mod_stats_d02 <- evaluation(model_d02_WS,observed,
                              table = mod_stats_d02, station = i,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d02   <- mod_stats_d02[mod_stats_d02$r >= r_limit,]
mod_stats_d02   <- evaluation(table = mod_stats_d02,summaryze = T)
cat('...\n')
print(tail(mod_stats_d02))
cat('\n')

cat("WS for d03:\n")
mod_stats_d03 <- evaluation(model_d03_WS,observed,names(model_d03_WS)[2])
for(i in names(model_d03_WS)[c(-1,-2)]){
  mod_stats_d03 <- evaluation(model_d03_WS,observed,
                              table = mod_stats_d03, station = i,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d03 <- mod_stats_d03[mod_stats_d03$r >= r_limit,]
mod_stats_d03   <- evaluation(table = mod_stats_d03,summaryze = T)
cat('...\n')
print(tail(mod_stats_d03))
cat('\n')

write.stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,case,'/stats.metar.WS.d01.csv'))
write.stat(stat = mod_stats_d02,
           file = paste0(WRF_folder,case,'/stats.metar.WS.d02.csv'))
write.stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,case,'/stats.metar.WS.d03.csv'))

summary_stats <- rbind('d01 in d01' = last(mod_stats_d01),
                       'd01 in d02' = last(comparison(tabA = mod_stats_d01,
                                                      tabB = mod_stats_d02,
                                                      summaryze = T)),
                       'd02 in d02' = last(mod_stats_d02),
                       'd01 in d03' = last(comparison(tabA = mod_stats_d01,
                                                      tabB = mod_stats_d03,
                                                      summaryze = T)),
                       'd02 in d03' = last(comparison(tabA = mod_stats_d02,
                                                      tabB = mod_stats_d03,
                                                      summaryze = T)),
                       'd03 in d03' = last(mod_stats_d03))

print(summary_stats)

write.stat(stat = summary_stats,
           file = paste0(WRF_folder,case,'/stats.metar.WS.all.csv'))

if(to.plot){
  par(mar = c(2.1, 4.2, 2, 1),mfrow=c(3,1), cex = 0.8)

  cor <- c("#1641CC", "#1E9FA6EB", "#21A118")

  site = "OAHR"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01_WS[site][,1],model_d02_WS[site][,1],model_d03_WS[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'WS [m/s]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01_WS$date,model_d01_WS[site][,1], col = cor[1],lwd = 2)
  lines(model_d02_WS$date,model_d02_WS[site][,1], col = cor[2],lwd = 2)
  lines(model_d03_WS$date,model_d03_WS[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)

  site = "OAIX"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01_WS[site][,1],model_d02_WS[site][,1],model_d03_WS[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'WS [m/s]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01_WS$date,model_d01_WS[site][,1], col = cor[1],lwd = 2)
  lines(model_d02_WS$date,model_d02_WS[site][,1], col = cor[2],lwd = 2)
  lines(model_d03_WS$date,model_d03_WS[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)

  site = "OAJL"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01_WS[site][,1],model_d02_WS[site][,1],model_d03_WS[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'WS [m/s]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01_WS$date,model_d01_WS[site][,1], col = cor[1],lwd = 2)
  lines(model_d02_WS$date,model_d02_WS[site][,1], col = cor[2],lwd = 2)
  lines(model_d03_WS$date,model_d03_WS[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)
}

cat('opening Wind direction METAR:\n')
files_obs <- dir(path = METAR_folder,pattern = 'METAR',full.names = T)
obs       <- data.frame(date = model_d01_WS$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat('open',files_obs[i],i,'of',length(files_obs),'\n')
  new        <- readRDS(files_obs[i])
  name       <- new$station[1]
  new        <- new[,c(1,11)]
  names(new) <- c('date',name)

  new        <- selectByDate(new,
                             start = as.Date(model_d01_WS$date[1]),
                             end   = as.Date(last(model_d01_WS$date)))

  new <- new[!duplicated(new$date), ]
  obs <- merge(obs, new, by = "date",all.x = T)
}
names(obs) <- c('date',substr(files_obs,nchar(METAR_folder)+7,nchar(files_obs)-4))
observed   <- obs

cat("WD for d01:\n")
mod_stats_d01 <- evaluation(model_d01_WD,observed,names(model_d01_WD)[2],cutoff = c(15,345),wd = T)
for(i in names(model_d01_WD)[c(-1,-2)]){
  mod_stats_d01 <- evaluation(model_d01_WD,observed,
                              table = mod_stats_d01, station = i,cutoff = c(15,345),wd = T,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d01   <- mod_stats_d01[mod_stats_d01$r >= r_limit,]
if(!is.na(IOA_limit))
  mod_stats_d01   <- mod_stats_d01[mod_stats_d01$IOA >= IOA_limit,]
mod_stats_d01   <- evaluation(table = mod_stats_d01,summaryze = T)
cat('...\n')
print(tail(mod_stats_d01))
cat('\n')

cat("WD for d02:\n")
mod_stats_d02 <- evaluation(model_d02_WD,observed,names(model_d02_WD)[2],cutoff = c(15,345),wd = T)
for(i in names(model_d02_WD)[c(-1,-2)]){
  mod_stats_d02 <- evaluation(model_d02_WS,observed,
                              table = mod_stats_d02, station = i,cutoff = c(15,345),wd = T,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d02   <- mod_stats_d02[mod_stats_d02$r >= r_limit,]
if(!is.na(IOA_limit))
  mod_stats_d02   <- mod_stats_d02[mod_stats_d02$IOA >= IOA_limit,]
mod_stats_d02   <- evaluation(table = mod_stats_d02,summaryze = T)
cat('...\n')
print(tail(mod_stats_d02))
cat('\n')

cat("WD for d03:\n")
mod_stats_d03 <- evaluation(model_d03_WD,observed,names(model_d03_WD)[2],cutoff = c(15,345),wd = T)
for(i in names(model_d03_WD)[c(-1,-2)]){
  mod_stats_d03 <- evaluation(model_d03_WS,observed,
                              table = mod_stats_d03, station = i,cutoff = c(15,345),wd = T,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d03 <- mod_stats_d03[mod_stats_d03$r >= r_limit,]
if(!is.na(IOA_limit))
  mod_stats_d03 <- mod_stats_d03[mod_stats_d03$IOA >= IOA_limit,]
mod_stats_d03   <- evaluation(table = mod_stats_d03,summaryze = T)
cat('...\n')
print(tail(mod_stats_d03))
cat('\n')

write.stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,case,'/stats.metar.WD.d01.csv'))
write.stat(stat = mod_stats_d02,
           file = paste0(WRF_folder,case,'/stats.metar.WD.d02.csv'))
write.stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,case,'/stats.metar.WD.d03.csv'))

summary_stats <- rbind('d01 in d01' = last(mod_stats_d01),
                       'd01 in d02' = last(comparison(tabA = mod_stats_d01,
                                                      tabB = mod_stats_d02,
                                                      summaryze = T)),
                       'd02 in d02' = last(mod_stats_d02),
                       'd01 in d03' = last(comparison(tabA = mod_stats_d01,
                                                      tabB = mod_stats_d03,
                                                      summaryze = T)),
                       'd02 in d03' = last(comparison(tabA = mod_stats_d02,
                                                      tabB = mod_stats_d03,
                                                      summaryze = T)),
                       'd03 in d03' = last(mod_stats_d03))

print(summary_stats)

write.stat(stat = summary_stats,
           file = paste0(WRF_folder,case,'/stats.metar.WD.all.csv'))

if(to.plot){
  par(mar = c(2.1, 4.2, 2, 1),mfrow=c(3,1), cex = 0.8)

  cor <- c("#1641CC", "#1E9FA6EB", "#21A118")

  site = "OAHR"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01_WD[site][,1],model_d02_WD[site][,1],model_d03_WD[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'WD [degrees]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01_WD$date,model_d01_WD[site][,1], col = cor[1],lwd = 2)
  lines(model_d02_WD$date,model_d02_WD[site][,1], col = cor[2],lwd = 2)
  lines(model_d03_WD$date,model_d03_WD[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)

  site = "OAIX"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01_WD[site][,1],model_d02_WD[site][,1],model_d03_WD[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'WD [degrees]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01_WD$date,model_d01_WD[site][,1], col = cor[1],lwd = 2)
  lines(model_d02_WD$date,model_d02_WD[site][,1], col = cor[2],lwd = 2)
  lines(model_d03_WD$date,model_d03_WD[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)

  site = "OAJL"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01_WD[site][,1],model_d02_WD[site][,1],model_d03_WD[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'WD [degrees]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01_WD$date,model_d01_WD[site][,1], col = cor[1],lwd = 2)
  lines(model_d02_WD$date,model_d02_WD[site][,1], col = cor[2],lwd = 2)
  lines(model_d03_WD$date,model_d03_WD[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)
}
cat('done!\n')
