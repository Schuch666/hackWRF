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

model_d01     <- readRDS('metar.d01.T2.Rds')
model_d02     <- readRDS('metar.d02.T2.Rds')
model_d03     <- readRDS('metar.d03.T2.Rds')
model_d01[-1] <- model_d01[-1] - 273.15
model_d02[-1] <- model_d02[-1] - 273.15
model_d03[-1] <- model_d03[-1] - 273.15

cat('opening TEMP:\n')
files_obs <- dir(path = METAR_folder,pattern = 'METAR',full.names = T)
obs       <- data.frame(date = model_d01$date, stringsAsFactors = T)

for(i in 1:length(files_obs)){
  cat('open',files_obs[i],i,'of',length(files_obs),'\n')
  new        <- readRDS(files_obs[i])
  name       <- new$station[1]
  new        <- new[,c(1,6)]
  names(new) <- c('date',name)

  new        <- selectByDate(new,
                             start = as.Date(model_d01$date[1]),
                             end   = as.Date(last(model_d01$date)))

  new <- new[!duplicated(new$date), ]
  obs <- merge(obs, new, by = "date",all.x = T)
}
names(obs) <- c('date',substr(files_obs,nchar(METAR_folder)+7,nchar(files_obs)-4))
observed   <- obs

# observed[-1] <- observed[-1] + 273.15 # Celcius to Kelvin

cat("Temperature for d01:\n")
mod_stats_d01 <- evaluation(model_d01,observed,names(model_d01)[2])
for(i in names(model_d01)[c(-1,-2)]){
  mod_stats_d01 <- evaluation(model_d01,observed,
                              table = mod_stats_d01, station = i,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d01   <- mod_stats_d01[mod_stats_d01$r >= r_limit,]
mod_stats_d01   <- evaluation(table = mod_stats_d01,summaryze = T)
mod_stats_d01   <- evaluation(model_d01,observed,'ALL',table = mod_stats_d01) # NEW d01
cat('...\n')
print(tail(mod_stats_d01))
cat('\n')

cat("Temperature for d02:\n")
mod_stats_d02 <- evaluation(model_d02,observed,names(model_d02)[2])
for(i in names(model_d02)[c(-1,-2)]){
  mod_stats_d02 <- evaluation(model_d02,observed,
                              table = mod_stats_d02, station = i,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d02   <- mod_stats_d02[mod_stats_d02$r >= r_limit,]
mod_stats_d02   <- evaluation(table = mod_stats_d02,summaryze = T)
mod_stats_d02   <- evaluation(model_d02,observed,'ALL',table = mod_stats_d02) # NEW d02
cat('...\n')
print(tail(mod_stats_d02))
cat('\n')

cat("Temperature for d03:\n")
mod_stats_d03 <- evaluation(model_d03,observed,names(model_d03)[2])
for(i in names(model_d03)[c(-1,-2)]){
  mod_stats_d03 <- evaluation(model_d03,observed,
                              table = mod_stats_d03, station = i,
                              clean = T)
}
if(!is.na(r_limit))
  mod_stats_d03 <- mod_stats_d03[mod_stats_d03$r >= r_limit,]
mod_stats_d03   <- evaluation(table = mod_stats_d03,summaryze = T)
mod_stats_d03   <- evaluation(model_d03,observed,'ALL',table = mod_stats_d03) # NEW d03
cat('...\n')
print(tail(mod_stats_d03))
cat('\n')

write.stat(stat = mod_stats_d01,
           file = paste0(WRF_folder,case,'/stats.metar.T2.d01.csv'))
write.stat(stat = mod_stats_d02,
           file = paste0(WRF_folder,case,'/stats.metar.T2.d02.csv'))
write.stat(stat = mod_stats_d03,
           file = paste0(WRF_folder,case,'/stats.metar.T2.d03.csv'))

# new summary + fair comparison for d01 / d02 / d03
summary_stats <- rbind('d01 in d01' = evaluation(model_d01,observed,'ALL',fair = model_d01),
                       'd01 in d02' = evaluation(model_d01,observed,'ALL',fair = model_d02),
                       'd02 in d02' = evaluation(model_d02,observed,'ALL',fair = model_d02),
                       'd01 in d03' = evaluation(model_d01,observed,'ALL',fair = model_d03),
                       'd02 in d03' = evaluation(model_d02,observed,'ALL',fair = model_d03),
                       'd03 in d03' = evaluation(model_d03,observed,'ALL',fair = model_d03))

print(summary_stats)

write.stat(stat = summary_stats,
           file = paste0(WRF_folder,case,'/stats.metar.T2.all.csv'))

if(to.plot){
  par(mar = c(2.1, 4.2, 2, 1),mfrow=c(3,1), cex = 0.8)

  cor <- c("#1641CC", "#1E9FA6EB", "#21A118")

  site = "OAHR"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01[site][,1],model_d02[site][,1],model_d03[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'T [K]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01$date,model_d01[site][,1], col = cor[1],lwd = 2)
  lines(model_d02$date,model_d02[site][,1], col = cor[2],lwd = 2)
  lines(model_d03$date,model_d03[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)

  site = "OAIX"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01[site][,1],model_d02[site][,1],model_d03[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'T [K]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01$date,model_d01[site][,1], col = cor[1],lwd = 2)
  lines(model_d02$date,model_d02[site][,1], col = cor[2],lwd = 2)
  lines(model_d03$date,model_d03[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)

  site = "OAJL"
  plot(observed$date,observed[site][,1],
       ylim = range(observed[site][,1],model_d01[site][,1],model_d02[site][,1],model_d03[site][,1],na.rm = T),
       pch = 19, col = 'red',ylab = 'T [K]',xlab = '',main = paste0('METAR - ',site,' station'))
  lines(model_d01$date,model_d01[site][,1], col = cor[1],lwd = 2)
  lines(model_d02$date,model_d02[site][,1], col = cor[2],lwd = 2)
  lines(model_d03$date,model_d03[site][,1], col = cor[3],lwd = 2)
  legend('bottomleft',
         legend = c('OBS','MODEL 27km','MODEL 9km','MODEL 3km'),
         pch = c(19,NA_integer_,NA_integer_,NA_integer_),
         lty = c(0,1,1,1), col = c('red',cor),bty = 'o',lwd = 2)
}
cat('done!\n')
