library(cmocean)
library(terra)
library(sf)

kmk <- vect('king_mackerel_po.shp')
plot(kmk)
st_as_sf(kmk)


# setwd("C:/Users/brendan.turley/Downloads/data-and-exercises-for-qgis-training-october-2019/data/GIS Outputs")
setwd("C:/Users/brendan.turley/Documents/CMP/data/data-and-exercises-for-qgis-training-october-2019/data/GIS Outputs")
test <- rast('Distn Now (092) King mackerel (Scomberomorus cavalla).png')


setwd("C:/Users/brendan.turley/Documents/CMP/data/data-and-exercises-for-qgis-training-october-2019/data/UBC Results")
dat <- read.csv('SPECIES_DISTRIBUTION_NOW.csv')

cols <- cmocean('solar')(49)

cuts <- cut(dat$Scomberomorus.cavalla, breaks = seq(0,1,.02))


plot(dat$Lon, dat$Lat, cex = dat$Scomberomorus.cavalla, asp = 1)
hist(dat$Scomberomorus.cavalla)

plot(dat$Lon, dat$Lat, col = cols[cuts], pch = 16, cex = .1, asp = 1)


setwd("C:/Users/brendan.turley/Documents/CMP/data/data-and-exercises-for-qgis-training-october-2019/data/")
dat <- vect('Catch Records.shp')


setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_occurance")
kk <- read.csv('Occurrence.csv')
hist(kk$depth)
quantile(kk$depth, c(.05,.95),na.rm=T)
