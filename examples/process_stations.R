library(maps) # only for the plot

METAR_folder = 'D:/Globus/dubai-stuff/METAR/DATA/' # folder containing the downloaded metar data

files_obs <- dir(path = METAR_folder,pattern = 'METAR',full.names = T)

cat('open',files_obs[1],1,'of',length(files_obs),'\n')
obs        <- readRDS(files_obs[1])
names(obs[,c(2,3,4)])
metar_sites <- data.frame(lon     = as.numeric(obs$lon[1]),
                          lat     = as.numeric(obs$lat[1]),
                          stringsAsFactors = F)

row.names(metar_sites) <- obs$station[1]

for(i in 2:length(files_obs)){
  cat('open',files_obs[i],i,'of',length(files_obs),'\n')
  new        <- readRDS(files_obs[i])
  new_site   <- data.frame(lon     = as.numeric(new$lon[1]),
                           lat     = as.numeric(new$lat[1]),
                           stringsAsFactors = F)
  row.names(new_site) <- new$station[1]
  metar_sites <- rbind(metar_sites,new_site)
}
print(metar_sites)

saveRDS(metar_sites,'metar-sites.Rds')

# map('world',xlim = c(30,75), ylim = c(5,45))
map('world')
points(x = metar_sites$lon, y = metar_sites$lat, col = 'blue', pch = 19)
box()
hackWRF::longitude()
hackWRF::latitude()
title('METAR stations')
