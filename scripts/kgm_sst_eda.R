
rm(list=ls())
gc()
dev.off()
library(abind)
library(fields)
library(cmocean)
library(ncdf4)
library(lubridate)
library(terra)
library(sf)
library(dplyr)

chomp <- function(xext, yext, dat) {
  bite <- as.polygons(ext(xext, yext))
  
  mean_values <- extract(dat, bite, fun = mean, na.rm = TRUE, ID = F) |>
    unlist()
  attr(mean_values,'names') <- NULL 
  
  mean_values#[-1]  
}


# define years  --------------------------------
styear <- 1982
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31


setwd("~/data/shapefiles/GSHHS_shp/i")
world <- vect('GSHHS_i_L1.shp')
world <- crop(world, ext(min_lon, max_lon, min_lat, max_lat))


# load shapefile to subset  --------------------------------
### shapefiles downloaded from marineregions.org (future goal implement mregions2 R package for shapefile)
setwd("~/data/shapefiles/gulf_eez")
eez <- vect('eez.shp') |> makeValid()

setwd("~/data/shapefiles/gulf_iho")
iho <- vect('iho.shp') |> makeValid()
iho_sf <- st_as_sf(iho)

gulf_eez <- terra::intersect(eez, iho)

rm(eez, iho)
gc()

### bathy for continential shape
# setwd("C:/Users/brendan.turley/Documents/data/bathy")
setwd("~/data/bathy")

bdat <- nc_open('etopo1.nc')
ln <- ncvar_get(bdat, 'lon')
ln_i <- which(ln>=min_lon & ln<=max_lon)
lt <- ncvar_get(bdat, 'lat')
lt_i <- which(lt>=min_lat & lt<=max_lat)

bathy <- ncvar_get(bdat, 'Band1',
                   start = c(ln_i[1],lt_i[1]),
                   count = c(length(ln_i), length(lt_i)))
nc_close(bdat)
rm(bdat)

bathy2 <- rast('etopo1.nc')

### isolate shelf
bathy2[values(bathy2$Band1) > (0)] <- NA
bathy2[values(bathy2$Band1) < (-100)] <- NA


### set all values to 1; then make into shapefile
bathy2[!is.na(values(bathy2$Band1))] <- 1
bathys <- as.polygons(bathy2)

bathy_c <- crop(bathys,
                ext(iho_sf)) |>
  st_as_sf() |>
  st_transform(st_crs(iho_sf)) |>
  st_simplify(dTolerance = 0)

kmk_shp <- st_intersection(bathy_c, iho_sf)
rm(bathy_c, bathy2, bathys)
gc()


### load sst data and combine
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")

# for(i in styear:enyear){
#   cat(i, '\n')
#   tmp <- paste0('anom_',i) |> readRDS()
#   
#   tmp$anom[which(tmp$anom==-999)] <- NA
#   
#   if(i==styear){
#     sst_a <- tmp$anom
#     dates <- tmp$time
#   } else {
#     sst_a <- abind(sst_a,
#                    tmp$anom,
#                    along = 3)
#     dates <- c(dates,
#                tmp$time)
#   }
# }
# 
# sst_a <- aperm(sst_a, c(2,1,3))
# sst_r <- rast(sst_a[dim(sst_a)[1]:1,,], crs="EPSG:4326") 
# ext(sst_r) <- c(min_lon, max_lon, min_lat, max_lat)
# time(sst_r) <- as.Date(dates)
# 
# setwd("~/R_projects/Looking_4_CMP/data")
# writeCDF(sst_r, 'oisst_anom_wgulf.nc',overwrite=TRUE)

dat <- rast('oisst_anom_wgulf.nc')
kmk_shp <- vect(kmk_shp)
dat_shelf <- crop(dat, kmk_shp, mask = T)
time <- time(dat_shelf)
rm(dat)

# 
# dat <- nc_open("oisst_anom_wgulf.nc")
# ssta <- ncvar_get(dat, "oisst_anom_wgulf")
# time <- ncvar_get(dat, "time") |> as.Date(origin = "1970-01-01")
# lon <- ncvar_get(dat, "longitude")
# lat <- ncvar_get(dat, "latitude")
# nc_close(dat)
# rm()

### define regions to examine
cb_lon <- c(-90.5, -86)
cb_lat <- c(21, 24)

sg_lon <- c(-95, -90.5)
sg_lat <- c(18, 23)

swg_lon <- c(-98, -95)
swg_lat <- c(18.5, 26)

tx_lon <- c(-98, -94)
tx_lat <- c(26, 30)

la_lon <- c(-94, -89)
la_lat <- c(27.5, 30)

latx_lon <- c(-98, -89)
latx_lat <- c(26, 30)

neg_lon <- c(-89, -82)
neg_lat <- c(28, 30.5)

wfl_lon <- c(-85, -81)
wfl_lat <- c(24, 28)


# imagePlot(lon, rev(lat), 
#           apply(ssta,c(1,2), mean, na.rm = T)[,dim(ssta)[2]:1],
#           breaks = seq(-1.5,1.5,.1), 
#           col = cmocean('balance')(length(seq(-1.5,1.5,.1))-1),
#           asp = 1)
# contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2, col = 'gray40')
# rect(cb_lon[1], cb_lat[1], cb_lon[2], cb_lat[2], lwd = 2)
# rect(swg_lon[1], swg_lat[1], swg_lon[2], swg_lat[2], lwd = 2)
# rect(sg_lon[1], sg_lat[1], sg_lon[2], sg_lat[2], lwd = 2)
# rect(tx_lon[1], tx_lat[1], tx_lon[2], tx_lat[2], lwd = 2)
# rect(la_lon[1], la_lat[1], la_lon[2], la_lat[2], lwd = 2)
# rect(neg_lon[1], neg_lat[1], neg_lon[2], neg_lat[2], lwd = 2)
# rect(wfl_lon[1], wfl_lat[1], wfl_lon[2], wfl_lat[2], lwd = 2)

### only since 2000
st_yr <- 2000
end_yr <- 2025


### extract sst timeseries using the regions defined
time_extract <- time[year(time) >= st_yr & year(time) <= end_yr]


# cb_ssta <- apply(ssta[lon >= cb_lon[1] & lon <= cb_lon[2],
#                       lat >= cb_lat[1] & lat <= cb_lat[2],
#                       year(time) >= st_yr & year(time) <= end_yr],
#                  3, mean, na.rm = TRUE)
# sg_ssta <- apply(ssta[lon >= sg_lon[1] & lon <= sg_lon[2],
#                       lat >= sg_lat[1] & lat <= sg_lat[2],
#                       year(time) >= st_yr & year(time) <= end_yr],
#                  3, mean, na.rm = TRUE)
# swg_ssta <- apply(ssta[lon >= swg_lon[1] & lon <= swg_lon[2],
#                        lat >= swg_lat[1] & lat <= swg_lat[2],
#                        year(time) >= st_yr & year(time) <= end_yr],
#                   3, mean, na.rm = TRUE)
# tx_ssta <- apply(ssta[lon >= tx_lon[1] & lon <= tx_lon[2],
#                      lat >= tx_lat[1] & lat <= tx_lat[2],
#                      year(time) >= st_yr & year(time) <= end_yr],
#                 3, mean, na.rm = TRUE)
# la_ssta <- apply(ssta[lon >= la_lon[1] & lon <= la_lon[2],
#                       lat >= la_lat[1] & lat <= la_lat[2],
#                       year(time) >= st_yr & year(time) <= end_yr],
#                  3, mean, na.rm = TRUE)
# neg_ssta <- apply(ssta[lon >= neg_lon[1] & lon <= neg_lon[2],
#                       lat >= neg_lat[1] & lat <= neg_lat[2],
#                       year(time) >= st_yr & year(time) <= end_yr],
#                  3, mean, na.rm = TRUE)
# wfl_ssta <- apply(ssta[lon >= wfl_lon[1] & lon <= wfl_lon[2],
#                       lat >= wfl_lat[1] & lat <= wfl_lat[2],
#                       year(time) >= st_yr & year(time) <= end_yr],
#                  3, mean, na.rm = TRUE)
# 
# plot(time_extract, cb_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for CB Region",
#      xaxt = 'n')
# points(time_extract[which(cb_ssta>0)], cb_ssta[which(cb_ssta>0)], type = "h", col = 2)
# points(time_extract[which(cb_ssta<0)], cb_ssta[which(cb_ssta<0)], type = "h", col = 4)
# axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
# abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
# lines(time_extract, lowess(cb_ssta, f = 1/104)$y, lwd = 4, col = 1)
# 
# plot(time_extract, sg_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for SG Region",
#      xaxt = 'n')
# points(time_extract[which(sg_ssta>0)], sg_ssta[which(sg_ssta>0)], type = "h", col = 2)
# points(time_extract[which(sg_ssta<0)], sg_ssta[which(sg_ssta<0)], type = "h", col = 4)
# axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
# abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
# lines(time_extract, lowess(sg_ssta, f = 1/104)$y, lwd = 4, col = 1)
# 
# plot(time_extract, swg_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for SWG Region",
#      xaxt = 'n')
# points(time_extract[which(swg_ssta>0)], swg_ssta[which(swg_ssta>0)], type = "h", col = 2)
# points(time_extract[which(swg_ssta<0)], swg_ssta[which(swg_ssta<0)], type = "h", col = 4)
# axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
# abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
# lines(time_extract, lowess(swg_ssta, f = 1/104)$y, lwd = 4, col = 1)
# 
# plot(time_extract, tx_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for TX Region",
#      xaxt = 'n')
# points(time_extract[which(tx_ssta>0)], tx_ssta[which(tx_ssta>0)], type = "h", col = 2)
# points(time_extract[which(tx_ssta<0)], tx_ssta[which(tx_ssta<0)], type = "h", col = 4)
# axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
# abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
# lines(time_extract, lowess(tx_ssta, f = 1/104)$y, lwd = 4, col = 1)
# 
# plot(time_extract, la_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for LA Region",
#      xaxt = 'n')
# points(time_extract[which(la_ssta>0)], la_ssta[which(la_ssta>0)], type = "h", col = 2)
# points(time_extract[which(la_ssta<0)], la_ssta[which(la_ssta<0)], type = "h", col = 4)
# axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
# abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
# lines(time_extract, lowess(la_ssta, f = 1/104)$y, lwd = 4, col = 1)
# 
# plot(time_extract, neg_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for NEG Region",
#      xaxt = 'n')
# points(time_extract[which(neg_ssta>0)], neg_ssta[which(neg_ssta>0)], type = "h", col = 2)
# points(time_extract[which(neg_ssta<0)], neg_ssta[which(neg_ssta<0)], type = "h", col = 4)
# axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
# abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
# lines(time_extract, lowess(neg_ssta, f = 1/104)$y, lwd = 4, col = 1)
# 
# plot(time_extract, wfl_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for WFL Region",
#      xaxt = 'n')
# points(time_extract[which(wfl_ssta>0)], wfl_ssta[which(wfl_ssta>0)], type = "h", col = 2)
# points(time_extract[which(wfl_ssta<0)], wfl_ssta[which(wfl_ssta<0)], type = "h", col = 4)
# axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
# abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
# lines(time_extract, lowess(wfl_ssta, f = 1/104)$y, lwd = 4, col = 1)



### subset for shelf only

cb_ts <- chomp(cb_lon, cb_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]
sg_ts <- chomp(sg_lon, sg_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]
swg_ts <- chomp(swg_lon, swg_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]
# tx_ts <- chomp(tx_lon, tx_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]
# la_ts <- chomp(la_lon, la_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]
latx_ts <- chomp(latx_lon, latx_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]
neg_ts <- chomp(neg_lon, neg_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]
wfl_ts <- chomp(wfl_lon, wfl_lat, dat_shelf)[year(time) >= st_yr & year(time) <= end_yr]


setwd("~/R_projects/Looking_4_CMP/figs")

png('ssta_cb_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, cb_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n', las = 2)
points(time_extract[which(cb_ts>0)], cb_ts[which(cb_ts>0)], type = "h", col = 2)
points(time_extract[which(cb_ts<0)], cb_ts[which(cb_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(cb_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for Campeche Bank", font = 2)
dev.off()

png('ssta_sg_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, sg_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n')
points(time_extract[which(sg_ts>0)], sg_ts[which(sg_ts>0)], type = "h", col = 2)
points(time_extract[which(sg_ts<0)], sg_ts[which(sg_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(sg_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for Bay of Campeche", font = 2)
dev.off()

png('ssta_swg_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, swg_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n')
points(time_extract[which(swg_ts>0)], swg_ts[which(swg_ts>0)], type = "h", col = 2)
points(time_extract[which(swg_ts<0)], swg_ts[which(swg_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(swg_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for TAM-VER Shelf", font = 2)
dev.off()

png('ssta_tx_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, tx_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n')
points(time_extract[which(tx_ts>0)], tx_ts[which(tx_ts>0)], type = "h", col = 2)
points(time_extract[which(tx_ts<0)], tx_ts[which(tx_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(tx_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for TX Shelf", font = 2)
dev.off()

png('ssta_la_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, la_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n')
points(time_extract[which(la_ts>0)], la_ts[which(la_ts>0)], type = "h", col = 2)
points(time_extract[which(la_ts<0)], la_ts[which(la_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(la_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for LA Shelf", font = 2)
dev.off()

png('ssta_latx_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, latx_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n')
points(time_extract[which(latx_ts>0)], latx_ts[which(latx_ts>0)], type = "h", col = 2)
points(time_extract[which(latx_ts<0)], latx_ts[which(latx_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(latx_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for LA-TX Shelf", font = 2)
dev.off()

png('ssta_neg_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, neg_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n')
points(time_extract[which(neg_ts>0)], neg_ts[which(neg_ts>0)], type = "h", col = 2)
points(time_extract[which(neg_ts<0)], neg_ts[which(neg_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(neg_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for NE Gulf", font = 2)
dev.off()

png('ssta_wfl_ts.png', width = 7, height = 4, units = 'in', pointsize = 10, res = 300)
plot(time_extract, wfl_ts, type = "n", xlab = "", ylab = "SST (°C)",
     xaxt = 'n')
points(time_extract[which(wfl_ts>0)], wfl_ts[which(wfl_ts>0)], type = "h", col = 2)
points(time_extract[which(wfl_ts<0)], wfl_ts[which(wfl_ts<0)], type = "h", col = 4)
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025, las = 2)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(wfl_ts, f = 1/104)$y, lwd = 4, col = 1)
mtext("SST anomaly for West Florida", font = 2)
dev.off()

### load SSTs
# setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files")
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
dat <- rast('oisst_wgulf.nc')
# kmk_shp <- vect(kmk_shp)
iho <- vect(iho_sf)
sst_wgulf <- crop(dat, iho, mask = T)
time <- time(sst_wgulf)
rm(dat)

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
dat <- nc_open("oisst_wgulf.nc")
sst <- ncvar_get(dat, "oisst_wgulf")
time <- ncvar_get(dat, "time") |> as.Date(origin = "1970-01-01")
lon <- ncvar_get(dat, "longitude")
lat <- ncvar_get(dat, "latitude")

### extract sst timeseries using the regions defined
time_extract <- time[year(time) >= st_yr & year(time) <= end_yr]
sst_ex <- sst[,,year(time) >= st_yr & year(time) <= end_yr]

# y <- 1:length(time)
y <- 1:length(time_extract)

array_lm <- function (x){
  if(is.na(all(x))){
    NA
  } else {
    coef(lm(x~y))[2]
  }
}

all_trend <- apply(sst_ex, c(1,2), array_lm)
all_trend <- all_trend*length(y)
hist(all_trend)
range(all_trend, na.rm = T)

setwd("~/R_projects/Looking_4_CMP/figs")
png('sst_trend.png', width = 7, height = 5.5, units = 'in', pointsize = 12, res = 300)
imagePlot(lon, rev(lat), 
          all_trend[,dim(sst)[2]:1],
          breaks = seq(-2,2,.05),
          col = cmocean('balance')(length(seq(-2,2,.05))-1),
          xlab = '', ylab = '',
          asp = 1, las = 1)
mtext('SST trend 2000-2025 (°C)', font = 2)
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2, col = 'white')
rect(cb_lon[1], cb_lat[1], cb_lon[2], cb_lat[2], lwd = 2)
rect(swg_lon[1], swg_lat[1], swg_lon[2], swg_lat[2], lwd = 2)
rect(sg_lon[1], sg_lat[1], sg_lon[2], sg_lat[2], lwd = 2)
# rect(tx_lon[1], tx_lat[1], tx_lon[2], tx_lat[2], lwd = 2)
# rect(la_lon[1], la_lat[1], la_lon[2], la_lat[2], lwd = 2)
rect(latx_lon[1], latx_lat[1], latx_lon[2], latx_lat[2], lwd = 2)
rect(neg_lon[1], neg_lat[1], neg_lon[2], neg_lat[2], lwd = 2)
rect(wfl_lon[1], wfl_lat[1], wfl_lon[2], wfl_lat[2], lwd = 2)
plot(world, add = T, col = 'gray')
dev.off()


sst_b15 <- apply(sst_ex[,, year(time_extract) <= 2015],
      c(1,2), mean, na.rm = T)

sst_a15 <- apply(sst_ex[,,year(time_extract) > 2015],
      c(1,2), mean, na.rm = T)

imagePlot(sst_b15[,ncol(sst_b15):1], asp = 1)
imagePlot(sst_a15[,ncol(sst_a15):1], asp = 1)

range(sst_a15[,ncol(sst_a15):1] - sst_b15[,ncol(sst_b15):1],na.rm = T)

png('sst_break2015.png', width = 7, height = 5.5, units = 'in', pointsize = 12, res = 300)
imagePlot(lon, rev(lat),
          sst_a15[,ncol(sst_a15):1] - sst_b15[,ncol(sst_b15):1], 
          breaks = seq(-1.1,1.1,.01),
          col = cmocean('balance')(length(seq(-1.1,1.1,.01))-1),
          xlab = '', ylab = '',
          asp = 1)
mtext('SST mean after 2016 - before 2016 (°C)', font = 2)
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2)
plot(world, add = T, col = 'gray')
dev.off()


### annual sst

# sst_dm <- apply(sst,3,mean,na.rm=T)
# 
# sst_ann <- aggregate(sst_dm, by=list(year(time)),mean,na.rm=T) |> 
#   setNames(c('year','sst_c'))

time_means <- global(sst_wgulf, fun = "mean", na.rm = TRUE) |>
  unlist()

sst_ann <- aggregate(time_means, by=list(year(time)),mean,na.rm=T) |> 
  setNames(c('year','sst_c'))

setwd("~/R_projects/Looking_4_CMP/figs")
png('gulf_sst_ann.png', width = 7, height = 4, res = 300, units = 'in')
plot(sst_ann$year, sst_ann$sst_c, typ = 'o', pch = 16, las = 1,
     xlab = '', ylab = 'SST (°C)', xaxt = 'n',
     panel.first = list(grid()))
axis(1, (1982:2025), las = 2)
abline(h = mean(sst_ann$sst_c), lty = 5)
lines(lowess(sst_ann, f = 1/7), lwd = 2, col = 2)
# abline(v = seq(1985,2025,5), lty = 5)
dev.off()

# ### define regions to examine
# cb_lon <- c(-90.5, -86.5)
# cb_lat <- c(21, 24)
# 
# sg_lon <- c(-95, -90.5)
# sg_lat <- c(18, 22)
# 
# swg_lon <- c(-98, -95)
# swg_lat <- c(18.5, 26)
# 
# tx_lon <- c(-98, -94)
# tx_lat <- c(26, 30)
# 
# la_lon <- c(-94, -89)
# la_lat <- c(28, 30)
# 
# neg_lon <- c(-89, -83)
# neg_lat <- c(28, 30.5)
# 
# wfl_lon <- c(-85, -81)
# wfl_lat <- c(24.5, 28)
# 
# ### only since 2000
# st_yr <- 2000
# end_yr <- 2025
# 
# 
# image(lon,rev(lat),sst[,dim(sst)[2]:1,1])
# rect(cb_lon[1], cb_lat[1], cb_lon[2], cb_lat[2], lwd = 2)
# rect(swg_lon[1], swg_lat[1], swg_lon[2], swg_lat[2], lwd = 2)
# rect(sg_lon[1], sg_lat[1], sg_lon[2], sg_lat[2], lwd = 2)
# rect(tx_lon[1], tx_lat[1], tx_lon[2], tx_lat[2], lwd = 2)
# rect(la_lon[1], la_lat[1], la_lon[2], la_lat[2], lwd = 2)
# rect(neg_lon[1], neg_lat[1], neg_lon[2], neg_lat[2], lwd = 2)
# rect(wfl_lon[1], wfl_lat[1], wfl_lon[2], wfl_lat[2], lwd = 2)


### extract sst timeseries using the regions defined
time_extract <- time[year(time) >= st_yr & year(time) <= end_yr]

cb_sst <- apply(sst[lon >= cb_lon[1] & lon <= cb_lon[2],
                    lat >= cb_lat[1] & lat <= cb_lat[2],
                    year(time) >= st_yr & year(time) <= end_yr],
                3, mean, na.rm = TRUE)
cb_yrly <- aggregate(cb_sst, by = list(year(time_extract)), mean, na.rm = T)

plot(time_extract, cb_sst, type = "l", xlab = "Time", ylab = "SST (°C)", main = "SST Time Series for CB Region", col = 4)
abline(h = mean(cb_sst,na.rm=T), lty = 5)
abline(v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(seq.Date(as.Date('2000-06-15'),as.Date('2025-06-15'),by = 'year'),cb_yrly$x)

plot(cb_yrly$Group.1, cb_yrly$x, typ = 'o')


sg_sst <- apply(sst[lon >= sg_lon[1] & lon <= sg_lon[2],
                    lat >= sg_lat[1] & lat <= sg_lat[2],
                    year(time) >= st_yr & year(time) <= end_yr],
                3, mean, na.rm = TRUE)
sg_yrly <- aggregate(sg_sst, by = list(year(time_extract)), mean, na.rm = T)

plot(time_extract, sg_sst, type = "l", xlab = "Time", ylab = "SST (°C)", main = "SST Time Series for SG Region", col = 4)
abline(h = mean(sg_sst,na.rm=T), lty = 5)
abline(v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(seq.Date(as.Date('2000-06-15'),as.Date('2025-06-15'),by = 'year'),sg_yrly$x)

plot(sg_yrly$Group.1, sg_yrly$x, typ = 'o')


swg_sst <- apply(sst[lon >= swg_lon[1] & lon <= swg_lon[2],
                     lat >= swg_lat[1] & lat <= swg_lat[2],
                     year(time) >= st_yr & year(time) <= end_yr],
                 3, mean, na.rm = TRUE)
swg_yrly <- aggregate(swg_sst, by = list(year(time_extract)), mean, na.rm = T)

plot(time_extract, swg_sst, type = "l", xlab = "Time", ylab = "SST (°C)", main = "SST Time Series for SWG Region", col = 4)
abline(h = mean(swg_sst,na.rm=T), lty = 5)
abline(v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(seq.Date(as.Date('2000-06-15'),as.Date('2025-06-15'),by = 'year'),swg_yrly$x)

plot(swg_yrly$Group.1, swg_yrly$x, typ = 'o')


plot(cb_yrly$Group.1, cb_yrly$x, typ = 'l', lwd = 2,
     ylim = c(25.5,28), las = 1, xaxt = 'n',
     panel.first = c(grid()),
     xlab = '', ylab = 'SST (°C)')
points(sg_yrly$Group.1, sg_yrly$x, typ = 'l', lwd = 2, col = 2)
points(swg_yrly$Group.1, swg_yrly$x, typ = 'l', lwd = 2, col = 4)
axis(1,2000:2025)
legend('bottomright', c('Campeche Bank','Bay of Campeche','TAM-VER'), fill = c(1,2,4), cex = .8, bty = 'n')
abline(lm(cb_yrly$x~cb_yrly$Group.1), lty = 5)
abline(lm(sg_yrly$x~sg_yrly$Group.1), lty = 5, col = 2)
abline(lm(swg_yrly$x~swg_yrly$Group.1), lty = 5, col = 4)


### make vector of months for ploting ydays
seq_months <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "month")
wh_yr <- 2016

### CB
cb_yday_m <- aggregate(cb_sst, by = list(yday(time_extract)), FUN = mean, na.rm = TRUE) |>
  setNames(c("yday", "mean_sst"))

plot(yday(time_extract), cb_sst, type = "n", xaxt = "n",
     xlab = "Day of Year", ylab = "SST (°C)", main = "SST Time Series for CB Region by Day of Year")
axis(1, at = yday(seq_months), labels = month(seq_months, label = TRUE))
for(i in 1:length(unique(year(time_extract)))) {
  year_i <- unique(year(time_extract))[i]
  lines(yday(time_extract[year(time_extract) == year_i]), cb_sst[year(time_extract) == year_i], col = 'gray')
}
lines(yday(time_extract[year(time_extract) == wh_yr]), cb_sst[year(time_extract) == wh_yr], col = 'red', lwd = 2)
# lines(cb_yday_m$yday,cb_yday_m$mean_sst, lwd = 2)
lines(lowess(cb_yday_m$mean_sst, f = 1/7), lwd = 2)

### SG
sg_yday_m <- aggregate(sg_sst, by = list(yday(time_extract)), FUN = mean, na.rm = TRUE) |>
  setNames(c("yday", "mean_sst"))

plot(yday(time_extract), sg_sst, type = "n", xaxt = 'n',
     xlab = "Day of Year", ylab = "SST (°C)", main = "SST Time Series for SG Region by Day of Year")
axis(1, at = yday(seq_months), labels = month(seq_months, label = TRUE))
for(i in 1:length(unique(year(time_extract)))) {
  year_i <- unique(year(time_extract))[i]
  lines(yday(time_extract[year(time_extract) == year_i]), sg_sst[year(time_extract) == year_i], col = 'gray')
}
lines(yday(time_extract[year(time_extract) == wh_yr]), sg_sst[year(time_extract) == wh_yr], col = 'red', lwd = 2)
# lines(sg_yday_m$yday,sg_yday_m$mean_sst, lwd = 2)
lines(lowess(sg_yday_m$mean_sst, f = 1/7), lwd = 2)

### SWG
swg_yday_m <- aggregate(swg_sst, by = list(yday(time_extract)), FUN = mean, na.rm = TRUE) |>
  setNames(c("yday", "mean_sst"))

plot(yday(time_extract), swg_sst, type = "n",  xaxt = 'n',
     xlab = "Day of Year", ylab = "SST (°C)", main = "SST Time Series for SWG Region by Day of Year")
axis(1, at = yday(seq_months), labels = month(seq_months, label = TRUE))
for(i in 1:length(unique(year(time_extract)))) {
  year_i <- unique(year(time_extract))[i]
  lines(yday(time_extract[year(time_extract) == year_i]), swg_sst[year(time_extract) == year_i], col = 'gray')
}
lines(yday(time_extract[year(time_extract) == wh_yr]), swg_sst[year(time_extract) == wh_yr], col = 'red', lwd = 2)
# # lines(swg_yday_m$ydayw,swg_yday_m$mean_sst, lwd = 2)
lines(lowess(swg_yday_m$mean_sst, f = 1/7), lwd = 2)


### interactive line plot with plotly using the yday
library(plotly)

plot_ly(x = ~time_extract, y = ~cb_sst, type = 'scatter', mode = 'lines') %>%
  layout(title = "SST Time Series for CB Region",
         xaxis = list(title = "Time"),
         yaxis = list(title = "SST (°C)"))

### make each year a different color and removable
plot_ly(x = ~yday(time_extract), y = ~cb_sst, type = 'scatter', mode = 'lines', 
        color = ~as.factor(year(time_extract)), colors = 'YlOrRd') %>%
  layout(title = "SST Time Series for CB Region by Day of Year",
         xaxis = list(title = "Day of Year"),
         yaxis = list(title = "SST (°C)"),
         legend = list(title = list(text = "Year")))

### add daily mean sst line
### take array of sst with dimensions of lon, lat, time; convert to matrix of lon*lat, time; then take mean across lon*lat for each time point
cb_yday_m <- aggregate(cb_sst, by = list(yday(time_extract)), FUN = mean, na.rm = TRUE) |>
  setNames(c("yday", "mean_sst"))
### add cb_yday_m$mean_sst as a line to the plot
plot_ly(x = ~yday(time_extract), y = ~cb_sst, type = 'scatter', mode = 'lines', 
        color = ~as.factor(year(time_extract)), colors = 'YlOrRd') %>%
  add_lines(y = cb_yday_m$mean_sst, name = "Mean SST", line = list(color = 'black', dash = 'dash'))



