
library(lubridate)
library(readxl)
library(terra)

setwd("C:/Users/brendan.turley/Documents/data/Fishery_observer_data/ReefExtraction for Coastal Pelagics (i.e. Trollin20240924044830")
data <- read_xlsx('Trolling Reef Observer Program Request (09_24_24).xlsx')

table(data$DATA_SOURCE)
data <- subset(data, DATA_SOURCE=='RFOP')
length(unique(data$TRIPNUMBER))
length(unique(data$VESSEL_ID))

addmargins(table(year(data$DEPART_DATE), month(data$DEPART_DATE)))

sort(table(data$DEPART_CITY_NAME))
sort(table(data$DEPART_ST_ABRV))
table(data$VESSEL_STATE)

hist(data$FISHING_DEPTH_M)
summary(data$FISHING_DEPTH_M)

plot(data$LON_BEGIN_SET, data$LAT_BEGIN_SET, asp = 1)

sort(table(data$SCIENTIFIC_NAME))
sort(table(data$COMMON_NAME))
sort(table(data$SPECIES_ITIS))

kmk <- subset(data, SPECIES_ITIS=='172435')
length(unique(kmk$TRIPNUMBER))
length(unique(kmk$VESSEL_ID))

hist(kmk$FISHING_DEPTH_M)

plot(kmk$FISHING_DEPTH_M, kmk$LENGTH_MM)
plot(kmk$WEIGHT_KG,kmk$LENGTH_MM)
boxplot(kmk$LENGTH_MM~year(kmk$SET_DATE))
boxplot(kmk$WEIGHT_KG~year(kmk$SET_DATE))

trip_dates <- aggregate(DEPART_DATE ~ TRIPNUMBER, data = kmk, function(x) x[1])
addmargins(table(year(trip_dates$DEPART_DATE), month(trip_dates$DEPART_DATE)))

trip_dates <- aggregate(DEPART_DATE ~ TRIPNUMBER, data = data, function(x) x[1])
addmargins(table(year(trip_dates$DEPART_DATE), month(trip_dates$DEPART_DATE)))

trip_sum <- aggregate(cbind(DEPART_CITY_NAME, DEPART_ST_ABRV) ~ TRIPNUMBER, data = kmk, function(x) x[1])
sort(table(trip_sum$DEPART_CITY_NAME))

trip_sum <- aggregate(cbind(DEPART_CITY_NAME, DEPART_ST_ABRV) ~ TRIPNUMBER, data = data, function(x) x[1])
sort(table(trip_sum$DEPART_CITY_NAME))



plot(kmk$LON_BEGIN_SET, kmk$LAT_BEGIN_SET, asp = 1)

aggregate(SPECIES_ITIS ~ TRIPNUMBER, kmk, length) |>
  merge(aggregate(DEPART_CITY_NAME ~ TRIPNUMBER, kmk, function(x) x[1]), by = 'TRIPNUMBER') |>
  merge(aggregate(DEPART_DATE ~ TRIPNUMBER, kmk, function(x) x[1]), by = 'TRIPNUMBER')

aggregate(data$SPECIES_ITIS, by = list(data$TRIPNUMBER), function(x) length(which(x=='172435')))


trip_dates <- aggregate(DEPART_DATE ~ TRIPNUMBER, data = data, function(x) x[1])

addmargins(table(year(trip_dates$DEPART_DATE), month(trip_dates$DEPART_DATE)))

trip_sum <- aggregate(cbind(DEPART_CITY_NAME, DEPART_ST_ABRV) ~ TRIPNUMBER, data = data, function(x) x[1])

sort(table(trip_sum$DEPART_CITY_NAME))
sort(table(trip_sum$DEPART_ST_ABRV))


trip_sum <- aggregate(cbind(DEPART_CITY_NAME, DEPART_ST_ABRV, LAND_MONTH, LAND_YEAR) ~ TRIPNUMBER, data = kmk, function(x) x[1])
trip_sum <- type.convert(trip_sum)

sort(table(trip_sum$DEPART_CITY_NAME))
sort(table(trip_sum$DEPART_ST_ABRV))
table(trip_sum$DEPART_ST_ABRV, trip_sum$LAND_MONTH)
table(trip_sum$DEPART_ST_ABRV, trip_sum$LAND_YEAR)

with(subset(data, LAND_YEAR<=2018),
     plot(LON_BEGIN_SET, LAT_BEGIN_SET, pch = 16, asp = 1))
with(subset(data, LAND_YEAR>2018),
     points(LON_BEGIN_SET, LAT_BEGIN_SET, pch = 16, col = 2))
with(subset(data, SPECIES_ITIS=='172435'),
     points(LON_BEGIN_SET, LAT_BEGIN_SET, pch = 1, col = 3, lwd = 2))

plot(data$LON_BEGIN_SET, data$LAT_BEGIN_SET, 
     col = as.factor(data$DEPART_ST_ABRV), pch = 16, asp = 1)


trip_sum <- aggregate(DEPART_DATE ~ TRIPNUMBER, data = data, function(x) x[1])

with(subset(trip_sum, year(DEPART_DATE)<=2018),
     hist(month(DEPART_DATE), breaks = seq(.5,12.5,1)))
with(subset(trip_sum, year(DEPART_DATE)>2018),
     hist(month(DEPART_DATE), breaks = seq(.5,12.5,1)))

with(subset(data, LAND_YEAR>2018),
     sort(table(COMMON_NAME)))
with(subset(data, LAND_YEAR<=2018),
     sort(table(COMMON_NAME)))

merge(with(subset(data, LAND_YEAR<=2018),
           sort(table(COMMON_NAME))),
      with(subset(data, LAND_YEAR>2018),
           sort(table(COMMON_NAME))), by = 'COMMON_NAME', all = T)

merge(with(subset(data, LAND_YEAR<=2018),
           (table(COMMON_NAME))),
      with(subset(data, LAND_YEAR>2018),
           (table(COMMON_NAME))), by = 'COMMON_NAME', all = T)

with(unique(data[,c(1,5,10)]),
     table(LAND_YEAR,DEPART_ST_ABRV))

table(data$VESSEL_ID, data$LAND_YEAR)
table(data$VESSEL_ID)
unique(data$VESSEL_ID)


table(kmk$LAND_MONTH, kmk$DEPART_ST_ABRV)
table(kmk$LAND_YEAR, kmk$LAND_MONTH)


