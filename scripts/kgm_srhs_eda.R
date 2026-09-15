### this script is to explore the SRHS database, the headboat dataset that has been extracted to only pull king mackerel in the Gulf

library(lubridate)
library(readxl)
library(terra)
library(sf)

setwd('~/data/shapefiles/cflp_statgrid')
stat_grid <- vect('CFLP_StatGrid_2013_v20140210.shp') |> 
  st_as_sf()
plot(stat_grid['STZ_ID'])
table(stat_grid$STZ_ID)

setwd("~/CMP/data/SRHS_logbooks")
dat <- read_xlsx('Gulf_KM_logbooks.xlsx')
dat$lon.dec[which(dat$lon.dec=='NA.NA')] <- NA
dat$lat.dec[which(dat$lat.dec=='NA.NA')] <- NA
dat <- type.convert(dat)
datpre <- subset(dat, year<2013)
datpost <- subset(dat, year>=2013)

dat$location |> nchar()

ifelse(nchar(dat$location)>=4, substr(dat$location,1,4), NA) |> table()

for(i in sort(unique(dat$year))){
  tmp <- subset(dat, year==i)
  paste("Year:", i, "has", length(which(!is.na(tmp$lon.dec) & !is.na(tmp$lat.dec))), "records with lat/lon out of", nrow(tmp), "total records") |>
    print()
} ### 2013 onward all have lon/lats

cut(dat$lon.dec, breaks = seq(-98,-79,1), labels = seq(97,79,-1))
cut(dat$lat.dec, breaks = seq(24,31,1), labels = seq(23,31,-1))

###LA-TX areas (ordered east-west): 28, 24-27, but not 29
### these don't change so the whole time series can be used

latx <- subset(dat, area>=24 & area<29)
latx <- subset(datpost, area>=24 & area<29)

### plot seasonality starting at 27 moving up to 24 and end at 28

par(mfrow=c(2,1))
for(i in c(27:24,28)){
  tmp <- subset(latx, area == i)
  tag <- aggregate(caught ~ month, data = tmp, sum, na.rm=T)
  boxplot(tmp$caught/tmp$anglers ~ tmp$month,
          outline = F)
  mtext(i)
  barplot(tag$caught, names.arg = tag$month)
}

### everything west of 92
w92 <- subset(dat, lon.dec<(-92))

lower <- seq(25,29,1)
par(mfrow=c(2,1))
for(i in lower){
  tmp <- subset(dat, lat.dec>i & lat.dec<i+1)
  tag <- aggregate(caught ~ month, data = tmp, sum, na.rm=T)
  boxplot(tmp$caught/tmp$anglers ~ tmp$month,
          outline = F)
  mtext(i)
  barplot(tag$caught, names.arg = tag$month)
  mtext(paste('n =',nrow(tmp)))
}
