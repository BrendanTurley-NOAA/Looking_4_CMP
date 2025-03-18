
library(terra)
library(sf)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/cb_2018_us_county_20m")
us_cnty <- vect('cb_2018_us_county_20m.shp')
us_cnty_sf <- st_as_sf(us_cnty)

st_fps <- read.csv('st_fps.txt', header = F)
st_fps$V1 <- sprintf('%02d', st_fps$V1)
states <- data.frame(state.abb,toupper(state.name))

us_cnty_sf$STATEFP <- st_fps$V2[match(us_cnty_sf$STATEFP, st_fps$V1)]
us_cnty_sf$STATE <- states$state.abb[match(us_cnty_sf$STATEFP, states$toupper.state.name.)]

# plot(us_cnty)

us_cnty_sf$NAME <- toupper(us_cnty_sf$NAME)

names(mck_st_cnty)[2] <- 'NAME'
names(mck_st_cnty)[1] <- 'STATE'
us_cnty_mck <- merge(us_cnty_sf, mck_st_cnty, by = c('STATE','NAME'))
# st_fps <- c('01', '12', '28', '22', '48')
# us_cnty_sf <- us_cnty_sf[is.element(us_cnty_sf$STATEFP, st_fps),]
us_cnty_mck$landings_rank <- max(rank(us_cnty_mck$live_lbs))-rank(us_cnty_mck$live_lbs)+1
top <- us_cnty_mck[order(us_cnty_mck$landings_rank, decreasing = F),]

# plot(st_geometry(us_cnty_mck))

us_cnty_mck$log_live_lbs <- log10(us_cnty_mck$live_lbs)
plot(us_cnty_mck["live_lbs"])
plot(us_cnty_mck["live_lbs"],logz=T)


setwd("~/R_projects/misc-noaa-scripts/figs")
png('mck_county.png', width = 10, height = 7, res = 300, units = 'in')
plot(st_geometry(us_cnty_sf), border = 'gray40', xlim = ext(us_cnty_mck)[1:2], ylim = ext(us_cnty_mck)[3:4])
plot(st_geometry(us_cnty_mck), add = T, lwd = 2)
plot(st_geometry(st_centroid(us_cnty_mck)), pch = 21, col = 'gray', bg = 1, add = TRUE,
     cex = log(us_cnty_mck$live_lbs)/4)
text(vect(st_centroid(top[1:10,])),top$landings_rank[1:10],col='white')
text(vect(st_centroid(top[11:20,])),top$landings_rank[11:20],col='yellow')
dev.off()

     

us_cnty_mck$dealr_rank <- max(rank(us_cnty_mck$num_dealr))-rank(us_cnty_mck$num_dealr)+1
top <- us_cnty_mck[order(us_cnty_mck$dealr_rank, decreasing = F),]

# plot(st_geometry(us_cnty_mck))

plot(us_cnty_mck["dealr_rank"])


setwd("~/R_projects/misc-noaa-scripts/figs")
png('mck_county.png', width = 10, height = 7, res = 300, units = 'in')
plot(st_geometry(us_cnty_sf), border = 'gray40', xlim = ext(us_cnty_mck)[1:2], ylim = ext(us_cnty_mck)[3:4])
plot(st_geometry(us_cnty_mck), add = T, lwd = 2)
plot(st_geometry(st_centroid(us_cnty_mck)), pch = 21, col = 'gray', bg = 1, add = TRUE,
     cex = log10(us_cnty_mck$num_dealr))
text(vect(st_centroid(top[1:10,])),top$dealr_rank[1:10],col='white')
text(vect(st_centroid(top[11:20,])),top$dealr_rank[11:20],col='yellow')
dev.off()

