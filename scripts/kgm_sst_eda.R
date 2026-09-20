
library(abind)
library(fields)
library(cmocean)
library(ncdf4)
library(lubridate)
library(terra)

### load sst data and combine
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")

# define years  --------------------------------
styear <- 1982
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

for(i in styear:enyear){
  cat(i, '\n')
  tmp <- paste0('anom_',i) |> readRDS()
  
  tmp$anom[which(tmp$anom==-999)] <- NA
  
  if(i==styear){
    sst_a <- tmp$anom
    dates <- tmp$time
  } else {
    sst_a <- abind(sst_a,
                   tmp$anom,
                   along = 3)
    dates <- c(dates,
               tmp$time)
  }
}

sst_a <- aperm(sst_a, c(2,1,3))
sst_r <- rast(sst_a[dim(sst_a)[1]:1,,], crs="EPSG:4326") 
ext(sst_r) <- c(min_lon, max_lon, min_lat, max_lat)
time(sst_r) <- as.Date(dates)

setwd("~/R_projects/Looking_4_CMP/data")
writeCDF(sst_r, 'oisst_anom_wgulf.nc',overwrite=TRUE)

dat <- nc_open("oisst_anom_wgulf.nc")
ssta <- ncvar_get(dat, "oisst_anom_wgulf")
time <- ncvar_get(dat, "time") |> as.Date(origin = "1970-01-01")
lon <- ncvar_get(dat, "longitude")
lat <- ncvar_get(dat, "latitude")

### define regions to examine
cb_lon <- c(-90.5, -86.5)
cb_lat <- c(21, 24)

sg_lon <- c(-95, -90.5)
sg_lat <- c(18, 22)

swg_lon <- c(-98, -95)
swg_lat <- c(18.5, 26)

tx_lon <- c(-98, -94)
tx_lat <- c(26, 30)

la_lon <- c(-94, -89)
la_lat <- c(28, 30)

neg_lon <- c(-89, -83)
neg_lat <- c(28, 30.5)

wfl_lon <- c(-85, -81)
wfl_lat <- c(24.5, 28)

### only since 2000
st_yr <- 2000
end_yr <- 2025


### extract sst timeseries using the regions defined
time_extract <- time[year(time) >= st_yr & year(time) <= end_yr]

cb_ssta <- apply(ssta[lon >= cb_lon[1] & lon <= cb_lon[2],
                      lat >= cb_lat[1] & lat <= cb_lat[2],
                      year(time) >= st_yr & year(time) <= end_yr],
                 3, mean, na.rm = TRUE)
sg_ssta <- apply(ssta[lon >= sg_lon[1] & lon <= sg_lon[2],
                      lat >= sg_lat[1] & lat <= sg_lat[2],
                      year(time) >= st_yr & year(time) <= end_yr],
                 3, mean, na.rm = TRUE)
swg_ssta <- apply(ssta[lon >= swg_lon[1] & lon <= swg_lon[2],
                       lat >= swg_lat[1] & lat <= swg_lat[2],
                       year(time) >= st_yr & year(time) <= end_yr],
                  3, mean, na.rm = TRUE)
tx_ssta <- apply(ssta[lon >= tx_lon[1] & lon <= tx_lon[2],
                     lat >= tx_lat[1] & lat <= tx_lat[2],
                     year(time) >= st_yr & year(time) <= end_yr],
                3, mean, na.rm = TRUE)
la_ssta <- apply(ssta[lon >= la_lon[1] & lon <= la_lon[2],
                      lat >= la_lat[1] & lat <= la_lat[2],
                      year(time) >= st_yr & year(time) <= end_yr],
                 3, mean, na.rm = TRUE)
neg_ssta <- apply(ssta[lon >= neg_lon[1] & lon <= neg_lon[2],
                      lat >= neg_lat[1] & lat <= neg_lat[2],
                      year(time) >= st_yr & year(time) <= end_yr],
                 3, mean, na.rm = TRUE)
wfl_ssta <- apply(ssta[lon >= wfl_lon[1] & lon <= wfl_lon[2],
                      lat >= wfl_lat[1] & lat <= wfl_lat[2],
                      year(time) >= st_yr & year(time) <= end_yr],
                 3, mean, na.rm = TRUE)

plot(time_extract, cb_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for CB Region",
     xaxt = 'n')
points(time_extract[which(cb_ssta>0)], cb_ssta[which(cb_ssta>0)], type = "h", col = 'red')
points(time_extract[which(cb_ssta<0)], cb_ssta[which(cb_ssta<0)], type = "h", col = 'blue')
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(cb_ssta, f = 1/50)$y, lwd = 4, col = 1)

plot(time_extract, sg_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for SG Region",
     xaxt = 'n')
points(time_extract[which(sg_ssta>0)], sg_ssta[which(sg_ssta>0)], type = "h", col = 'red')
points(time_extract[which(sg_ssta<0)], sg_ssta[which(sg_ssta<0)], type = "h", col = 'blue')
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(sg_ssta, f = 1/50)$y, lwd = 4, col = 1)

plot(time_extract, swg_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for SWG Region",
     xaxt = 'n')
points(time_extract[which(swg_ssta>0)], swg_ssta[which(swg_ssta>0)], type = "h", col = 'red')
points(time_extract[which(swg_ssta<0)], swg_ssta[which(swg_ssta<0)], type = "h", col = 'blue')
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(swg_ssta, f = 1/50)$y, lwd = 4, col = 1)

plot(time_extract, tx_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for TX Region",
     xaxt = 'n')
points(time_extract[which(tx_ssta>0)], tx_ssta[which(tx_ssta>0)], type = "h", col = 'red')
points(time_extract[which(tx_ssta<0)], tx_ssta[which(tx_ssta<0)], type = "h", col = 'blue')
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(tx_ssta, f = 1/50)$y, lwd = 4, col = 1)

plot(time_extract, la_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for LA Region",
     xaxt = 'n')
points(time_extract[which(la_ssta>0)], la_ssta[which(la_ssta>0)], type = "h", col = 'red')
points(time_extract[which(la_ssta<0)], la_ssta[which(la_ssta<0)], type = "h", col = 'blue')
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(la_ssta, f = 1/50)$y, lwd = 4, col = 1)

plot(time_extract, neg_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for NEG Region",
     xaxt = 'n')
points(time_extract[which(neg_ssta>0)], neg_ssta[which(neg_ssta>0)], type = "h", col = 'red')
points(time_extract[which(neg_ssta<0)], neg_ssta[which(neg_ssta<0)], type = "h", col = 'blue')
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(neg_ssta, f = 1/50)$y, lwd = 4, col = 1)

plot(time_extract, wfl_ssta, type = "n", xlab = "Time", ylab = "SST (°C)", main = "SSTa Time Series for WFL Region",
     xaxt = 'n')
points(time_extract[which(wfl_ssta>0)], wfl_ssta[which(wfl_ssta>0)], type = "h", col = 'red')
points(time_extract[which(wfl_ssta<0)], wfl_ssta[which(wfl_ssta<0)], type = "h", col = 'blue')
axis(1, seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year"), 2000:2025)
abline(h = 0, v = seq.Date(as.Date("2000-01-01"), as.Date("2025-12-31"), by = "year")[seq(1,25,2)], lty = 5)
lines(time_extract, lowess(wfl_ssta, f = 1/50)$y, lwd = 4, col = 1)


### load SSTs
setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files")
dat <- nc_open("oisst_wgulf.nc")

sst <- ncvar_get(dat, "oisst_wgulf")
time <- ncvar_get(dat, "time") |> as.Date(origin = "1970-01-01")
lon <- ncvar_get(dat, "longitude")
lat <- ncvar_get(dat, "latitude")

y <- 1:dim(sst)[3]

array_lm <- function (x){
  if(is.na(all(x))){
    NA
  } else {
    coef(lm(x~y))[2]
  }
}

all_trend <- apply(sst, c(1,2), array_lm)
all_trend <- all_trend*length(y)
hist(all_trend)
range(all_trend, na.rm = T)
imagePlot(all_trend[,dim(sst)[2]:1],breaks=seq(-2,2,.05),
          col=cmocean('balance')(length(seq(-2,2,.05))-1))

### define regions to examine
cb_lon <- c(-90.5, -86.5)
cb_lat <- c(21, 24)

sg_lon <- c(-95, -90.5)
sg_lat <- c(18, 22)

swg_lon <- c(-98, -95)
swg_lat <- c(18.5, 26)

tx_lon <- c(-98, -94)
tx_lat <- c(26, 30)

la_lon <- c(-94, -89)
la_lat <- c(28, 30)

neg_lon <- c(-89, -83)
neg_lat <- c(28, 30.5)

wfl_lon <- c(-85, -81)
wfl_lat <- c(24.5, 28)

### only since 2000
st_yr <- 2000
end_yr <- 2025


image(lon,rev(lat),sst[,dim(sst)[2]:1,1])
rect(cb_lon[1], cb_lat[1], cb_lon[2], cb_lat[2], lwd = 2)
rect(swg_lon[1], swg_lat[1], swg_lon[2], swg_lat[2], lwd = 2)
rect(sg_lon[1], sg_lat[1], sg_lon[2], sg_lat[2], lwd = 2)
rect(tx_lon[1], tx_lat[1], tx_lon[2], tx_lat[2], lwd = 2)
rect(la_lon[1], la_lat[1], la_lon[2], la_lat[2], lwd = 2)
rect(neg_lon[1], neg_lat[1], neg_lon[2], neg_lat[2], lwd = 2)
rect(wfl_lon[1], wfl_lat[1], wfl_lon[2], wfl_lat[2], lwd = 2)


### extract sst timeseries using the regions defined
time_extract <- time[year(time) >= st_yr & year(time) <= end_yr]

cb_sst <- apply(sst[lon >= cb_lon[1] & lon <= cb_lon[2],
                    lat >= cb_lat[1] & lat <= cb_lat[2],
                    year(time) >= st_yr & year(time) <= end_yr],
                3, mean, na.rm = TRUE)
plot(time_extract, cb_sst, type = "l", xlab = "Time", ylab = "SST (°C)", main = "SST Time Series for CB Region")

sg_sst <- apply(sst[lon >= sg_lon[1] & lon <= sg_lon[2],
                    lat >= sg_lat[1] & lat <= sg_lat[2],
                    year(time) >= st_yr & year(time) <= end_yr],
                3, mean, na.rm = TRUE)
plot(time_extract, sg_sst, type = "l", xlab = "Time", ylab = "SST (°C)", main = "SST Time Series for SG Region")

swg_sst <- apply(sst[lon >= swg_lon[1] & lon <= swg_lon[2],
                     lat >= swg_lat[1] & lat <= swg_lat[2],
                     year(time) >= st_yr & year(time) <= end_yr],
                 3, mean, na.rm = TRUE)
plot(time_extract, swg_sst, type = "l", xlab = "Time", ylab = "SST (°C)", main = "SST Time Series for SWG Region")


### make vector of months for ploting ydays
seq_months <- seq.Date(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "month")


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
lines(yday(time_extract[year(time_extract) == 2018]), cb_sst[year(time_extract) == 2018], col = 'red', lwd = 2)
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
lines(yday(time_extract[year(time_extract) == 2018]), sg_sst[year(time_extract) == 2018], col = 'red', lwd = 2)
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
lines(yday(time_extract[year(time_extract) == 2018]), swg_sst[year(time_extract) == 2018], col = 'red', lwd = 2)
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
  add_lines(y = mean_sst, name = "Mean SST", line = list(color = 'black', dash = 'dash'))



