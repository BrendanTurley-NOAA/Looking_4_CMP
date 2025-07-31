
rm(list=ls())
gc()
library(cmocean)
library(terra)
library(sf)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GSHHS_shp/i")
world <- vect('GSHHS_i_L1.shp')

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/king_mackerel")
kmk <- vect('king_mackerel_po.shp')
plot(kmk)
st_as_sf(kmk)


# setwd("C:/Users/brendan.turley/Downloads/data-and-exercises-for-qgis-training-october-2019/data/GIS Outputs")
setwd("C:/Users/brendan.turley/Documents/CMP/data/data-and-exercises-for-qgis-training-october-2019/data/GIS Outputs")
test <- rast('Distn Now (092) King mackerel (Scomberomorus cavalla).png')
plot(test)

### now
setwd("C:/Users/brendan.turley/Documents/CMP/data/data-and-exercises-for-qgis-training-october-2019/data/UBC Results")
dat <- read.csv('SPECIES_DISTRIBUTION_NOW.csv')

kmk <- data.frame(x = dat$Lon,
                  y = dat$Lat,
                  value = dat$Scomberomorus.cavalla,
                  xy = T)
my_points_sf <- vect(kmk, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84")
r_template <- rast(
  xmin = min(kmk$x), xmax = max(kmk$x),
  ymin = min(kmk$y), ymax = max(kmk$y),
  resolution = 0.15, # Example resolution (adjust as needed)
  crs = "+proj=longlat +datum=WGS84"
)
my_raster <- rasterize(my_points_sf, r_template, field = "value", fun = mean)
values(my_raster)[which(values(my_raster)==0)] <- NA

cols2 <- cmocean('rain')(60)
plot(my_raster, col = cols2)

hist(dat$Scomberomorus.cavalla, breaks = seq(0,1,.05))
brks <- seq(0,.8,.05)
cuts <- cut(dat$Scomberomorus.cavalla, breaks = brks)
cols <- cmocean('dense')(length(brks)-1)

plot(dat$Lon, dat$Lat, col = cols[cuts], pch = 16, cex = .1, asp = 1)
# plot(dat$Lon, dat$Lat, col = cols[cuts], pch = 16, cex = .1, asp = 1,
     # xlim = c(-100, -75), ylim = c(20, 33))

dat2 <- dat
thres <- .3
dat2[which(dat2$Scomberomorus.cavalla<thres),] <- NA
hist(dat2$Scomberomorus.cavalla, breaks = seq(0,1,.05))
brks2 <- seq(thres,.8,.05)
cuts2 <- cut(dat2$Scomberomorus.cavalla, breaks = brks2)
cols2 <- cmocean('dense')(length(brks2)-1)

plot(dat2$Lon, dat2$Lat, col = cols2[cuts2], pch = 16, cex = .1, asp = 1)


setwd("C:/Users/brendan.turley/Documents/CMP/data/data-and-exercises-for-qgis-training-october-2019/data/UBC Results")
dat_2100 <- read.csv('SPECIES_DISTRIBUTION_ENDCENTURY.csv')

kmk_2100 <- data.frame(x = dat_2100$Lon,
                       y = dat_2100$Lat,
                       value = dat_2100$Scomberomorus.cavalla,
                       xy = T)
kmk_2100_sf <- vect(kmk_2100, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84")
r_template2 <- rast(
  xmin = min(kmk_2100$x), xmax = max(kmk_2100$x),
  ymin = min(kmk_2100$y), ymax = max(kmk_2100$y),
  resolution = 0.15, # Example resolution (adjust as needed)
  crs = "+proj=longlat +datum=WGS84"
)
kmk2100_raster <- rasterize(kmk_2100_sf, r_template2, field = "value", fun = mean)
values(kmk2100_raster)[which(values(kmk2100_raster)==0)] <- NA

endcentury <- kmk2100_raster - my_raster

cols3 <- cmocean('delta')(60)

setwd("~/R_projects/Looking_4_CMP/figs")
png('kmk_future.png', width = 10, height = 9, units = 'in', res = 300)
plot(endcentury, col = cols3, range = c(-.2,.2), fill_range = T,
     main = 'KMK distribution (2100 - now)',
     xlim = c(-100, -74.5), ylim = c(17.5, 33), las = 1)
plot(world, add = T, col = 'gray80')
dev.off()


setwd("C:/Users/brendan.turley/Documents/CMP/data/data-and-exercises-for-qgis-training-october-2019/data/")
dat <- vect('Catch Records.shp')


setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_occurance")
kk <- read.csv('Occurrence.csv')
hist(kk$depth)
quantile(kk$depth, c(.05,.95),na.rm=T)
