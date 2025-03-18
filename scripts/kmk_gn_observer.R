
library(dplyr)
library(fields)
library(lubridate)
library(readxl)
library(terra)
library(maps)
library(marmap)
library(cmocean)

bathy <- getNOAA.bathy(-99,-75,23,40,resolution = 1)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/Florida_Shoreline__1_to_40_2C000_Scale_-shp")
fl_shp <- vect('Florida_Shoreline__1_to_40_2C000_Scale_.shp')

### load observer data
setwd("C:/Users/brendan.turley/Documents/data/Fishery_observer_data/GillnetExtraction for Coastal Pelagics20240924044848/Gillnet Request (09_24_24)/Gillnet Request (09_24_24)")

trip <- read_excel('TRIP_SUMMARY.xlsx')

haul <- read_excel('HAUL_LOG.xlsx')
haul <- utils::type.convert(haul)

catch <- read_excel('CATCH_LOG.xlsx')

gear <- read_excel('GEAR_LOG.xlsx')
panel <- read_excel('PANEL_LOG.xlsx')

animal <- read_excel('ANIMAL_LOG.xlsx')


### subset for KMK hauls and pull unique trip IDs
kmk_h <- subset(haul, TARGET_SPECIES=='King Mackerel' &
                  GEAR_TYPE=='GILL NETS, DRIFT, RUNAROUND')
kmk_id <- sort(unique(kmk_h$TRIP_ID))


#### ---- ####
library(leaflet)

kmk_locs <- data.frame(long = -kmk_h$HAUL_BEGIN_LONG_CONV,
                       lat = kmk_h$HAUL_BEGIN_LAT_CONV)

leaflet(kmk_locs) %>% addTiles() %>% addMarkers()
#### ---- ####

### number of unique trips = 78
length(kmk_id)

# plot(-kmk_h$SET_END_LONG_CONV, kmk_h$SET_END_LAT_CONV, asp = 1)
# plot(fl_shp, add=T)

### number of unique sets = 94
addmargins(table(month(kmk_h$SET_BEGIN_DATE), year(kmk_h$SET_BEGIN_DATE), useNA = 'always'))


hist(kmk_h$WAVE_HEIGHT_FT)
hist(kmk_h$WIND_SPEED_KTS)
hist(as.numeric(kmk_h$WIND_DIRECTION_DEG))
hist(kmk_h$SET_BEGIN_TIME)

hist(kmk_h$SET_BEGIN_TEMP, breaks = seq(58,81,1))
hist(kmk_h$HAUL_BEGIN_TEMP, breaks = seq(60, 82, 1))
boxplot(kmk_h$SET_BEGIN_TEMP, kmk_h$HAUL_BEGIN_TEMP)

barplot(table(kmk_h$BOTTOM_TYPE_DESC))

hist(kmk_h$BOTTOM_DEPTH_FT)

hist(kmk_h$GEAR_SOAK_HOURS)
summary(kmk_h$GEAR_SOAK_HOURS)

plot(kmk_h$WAVE_HEIGHT_FT, kmk_h$WIND_SPEED_KTS)
plot(kmk_h$GEAR_SOAK_HOURS, kmk_h$WIND_SPEED_KTS)
plot(as.numeric(kmk_h$WIND_DIRECTION_DEG), kmk_h$WIND_SPEED_KTS)

### was there a change in depth fished over time?
plot(year(kmk_h$SET_BEGIN_DATE), -kmk_h$BOTTOM_DEPTH_FT)
lines(lowess(year(kmk_h$SET_BEGIN_DATE), -kmk_h$BOTTOM_DEPTH_FT), col = 2, lwd = 2)
lines(loess(-kmk_h$BOTTOM_DEPTH_FT ~ year(kmk_h$SET_BEGIN_DATE), na.action = 'na.exclude'), col = 4, lwd = 2)

### is fishing earlier or later over time?
plot(year(kmk_h$SET_BEGIN_DATE), yday(kmk_h$SET_BEGIN_DATE))
lines(lowess(year(kmk_h$SET_BEGIN_DATE), yday(kmk_h$SET_BEGIN_DATE)), col = 2, lwd = 2)
lines(loess(yday(kmk_h$SET_BEGIN_DATE) ~ year(kmk_h$SET_BEGIN_DATE), na.action = 'na.exclude'), col = 4, lwd = 2)

### is water temperature changing over time?
plot(year(kmk_h$SET_BEGIN_DATE), kmk_h$SET_BEGIN_TEMP)
lines(lowess(year(kmk_h$SET_BEGIN_DATE), kmk_h$SET_BEGIN_TEMP), col = 2, lwd = 2)
lines(loess(kmk_h$SET_BEGIN_TEMP ~ year(kmk_h$SET_BEGIN_DATE), na.action = 'na.exclude'), col = 4, lwd = 2)


### kmk trip summary
kmk_t <- subset(trip, is.element(TRIP_ID, kmk_id))

### number of vessels = 14
length(unique(kmk_t$VESSEL_NAME))
addmargins(table(kmk_t$VESSEL_NAME))
addmargins(table(kmk_t$LANDING_CITY))

barplot(table(kmk_t$SEA_DAYS))

addmargins(table(kmk_t$VESSEL_NAME, kmk_t$SEA_DAYS))
addmargins(table(year(kmk_t$DATE_LANDED), kmk_t$VESSEL_NAME))
addmargins(table(year(kmk_t$DATE_LANDED), kmk_t$SEA_DAYS))
addmargins(table(year(kmk_t$DATE_LANDED), kmk_t$SETS_QUANTITY))
addmargins(table(year(kmk_t$DATE_LANDED), month(kmk_t$DATE_LANDED)))
addmargins(table(year(kmk_t$DATE_LANDED), yday(kmk_t$DATE_LANDED)))

### number of vessels per year
aggregate(kmk_t$VESSEL_NAME, by = list(year(kmk_t$DATE_LANDED)),
          function(X) c(num = length(unique(X)), pro = length(unique(X))/14)) |>
  setNames(c('year','vessel'))
### number of seadays per year
aggregate(cbind(kmk_t$SEA_DAYS, kmk_t$SETS_QUANTITY), by = list(year(kmk_t$DATE_LANDED)), sum, na.rm = T)  |>
  setNames(c('year','seadays','sets'))
### number of seadays per vessel per year
aggregate(kmk_t$SEA_DAYS, by = list(kmk_t$VESSEL_NAME, year(kmk_t$DATE_LANDED)), sum)

addmargins(table(kmk_t$SETS_QUANTITY, kmk_t$SEA_DAYS))

plot(ymd(kmk_t$DATE_LANDED), kmk_t$SEA_DAYS)
plot(ymd(kmk_t$DATE_LANDED), kmk_t$SETS_QUANTITY)
plot(kmk_t$SETS_QUANTITY, kmk_t$SEA_DAYS)


kmk_c <- subset(catch, is.element(TRIP_ID, kmk_id))

kmk_catch <- subset(kmk_c, SP_COMMON_NAME=='King Mackerel') %>%
  group_by(TRIP_ID) %>%
  summarise_at(c('COUNT_00_30','COUNT_30_60','COUNT_60_90',
                 'COUNT_90_120','COUNT_120_150','COUNT_150_180',
                 'COUNT_180_210','COUNT_210_240'), sum, na.rm = T)

sapply(kmk_catch[,-1],sum)
rowSums(kmk_catch[,-1])
kmk_c$NUMBER_CAUGHT[which(kmk_c$SP_COMMON_NAME=='King Mackerel')]

kmk_c %>%
group_by(SP_COMMON_NAME) %>%
  summarise_at(c('COUNT_00_30','COUNT_30_60','COUNT_60_90',
                 'COUNT_90_120','COUNT_120_150','COUNT_150_180',
                 'COUNT_180_210','COUNT_210_240'), sum, na.rm = T)

kmk_c %>%
  group_by(SP_COMMON_NAME) %>%
  summarise_at(c('NUMBER_CAUGHT'), sum, na.rm = T)


aggregate(cbind(kmk_c$NUMBER_CAUGHT, kmk_c$TOTAL_NUM_KEPT, kmk_c$WEIGHT_LANDED),
          by = list(kmk_c$SP_COMMON_NAME), sum, na.rm = T)

aggregate(kmk_c$NUMBER_CAUGHT, by = list(kmk_c$TRIP_ID,kmk_c$SP_COMMON_NAME), sum, na.rm = T)
aggregate(kmk_c$WEIGHT_LANDED, by = list(kmk_c$TRIP_ID,kmk_c$SP_COMMON_NAME), sum, na.rm = T)



kmk_c_merge <- merge(subset(kmk_c, SP_COMMON_NAME=='King Mackerel'), haul, by = 'TRIP_ID', ALL.x = T)

hist(kmk_c_merge$NUMBER_CAUGHT/ kmk_c_merge$GEAR_SOAK_HOURS)

data.frame(TRIP_ID = kmk_c_merge$TRIP_ID,
           cpue = kmk_c_merge$NUMBER_CAUGHT/ kmk_c_merge$GEAR_SOAK_HOURS )

plot(year(kmk_c_merge$HAUL_END_DATE), kmk_c_merge$NUMBER_CAUGHT/ kmk_c_merge$GEAR_SOAK_HOURS, typ = 'n')
boxplot(kmk_c_merge$NUMBER_CAUGHT/kmk_c_merge$GEAR_SOAK_HOURS ~ year(kmk_c_merge$HAUL_END_DATE), varwidth=T ,
        at = sort(unique(year(kmk_c_merge$HAUL_END_DATE))), outline = F, lty = 1, add = T, staplewex = 0)
points(jitter(year(kmk_c_merge$HAUL_END_DATE)), kmk_c_merge$NUMBER_CAUGHT/ kmk_c_merge$GEAR_SOAK_HOURS,
       pch = 16)

plot(year(kmk_c_merge$HAUL_END_DATE), kmk_c_merge$NUMBER_CAUGHT, typ = 'n')
boxplot(kmk_c_merge$NUMBER_CAUGHT ~ year(kmk_c_merge$HAUL_END_DATE), varwidth=T ,
        at = sort(unique(year(kmk_c_merge$HAUL_END_DATE))), outline = F, lty = 1, add = T, staplewex = 0)
points(jitter(year(kmk_c_merge$HAUL_END_DATE)), kmk_c_merge$NUMBER_CAUGHT,
       pch = 16)

plot(year(kmk_c_merge$HAUL_END_DATE), kmk_c_merge$GEAR_SOAK_HOURS, typ = 'n')
boxplot(kmk_c_merge$GEAR_SOAK_HOURS ~ year(kmk_c_merge$HAUL_END_DATE), varwidth=T ,
        at = sort(unique(year(kmk_c_merge$HAUL_END_DATE))), outline = F, lty = 1, add = T, staplewex = 0)
points(jitter(year(kmk_c_merge$HAUL_END_DATE)), kmk_c_merge$GEAR_SOAK_HOURS,
       pch = 16)



### Lets standardize
#haul -
names(kmk_h)[-c(2:3)]
kmk_merge <- merge(kmk_h[,-2], kmk_c, by = c('TRIP_ID', 'HAUL_NBR', 'GEAR_NBR'), all = T) |>
  merge(gear[,-2], by = c('TRIP_ID', 'GEAR_NBR')) |>
  merge(panel[,-2], by = c('TRIP_ID', 'GEAR_NBR', 'PANEL_NBR'))

 kmk_merge$cpue <- kmk_merge$NUMBER_CAUGHT / kmk_merge$LENGTH * kmk_merge$DEPTH / kmk_merge$GEAR_SOAK_HOURS

kmk_merge_kmk <- subset(kmk_merge, SP_COMMON_NAME=='King Mackerel')
 
plot(year(kmk_merge_kmk$HAUL_END_DATE), kmk_merge_kmk$cpue, typ = 'n')
boxplot(kmk_merge_kmk$cpue ~ year(kmk_merge_kmk$HAUL_END_DATE), varwidth=T ,
        at = sort(unique(year(kmk_merge_kmk$HAUL_END_DATE))), outline = F, lty = 1, add = T, staplewex = 0)
points(jitter(year(kmk_merge_kmk$HAUL_END_DATE)), kmk_merge_kmk$cpue,
       pch = 16)

boxplot(kmk_merge_kmk$LENGTH ~ year(kmk_merge_kmk$HAUL_END_DATE), varwidth=T ,
        at = sort(unique(year(kmk_merge_kmk$HAUL_END_DATE))), outline = F, lty = 1, add = F, staplewex = 0)

boxplot(kmk_merge_kmk$MESHSIZE ~ year(kmk_merge_kmk$HAUL_END_DATE), varwidth=T ,
        at = sort(unique(year(kmk_merge_kmk$HAUL_END_DATE))), outline = F, lty = 1, add = F, staplewex = 0)

### scratch ---------------- 


table(trip_sum$TRIP_TYPE)

table(trip_sum$TRIP_TARGET) # unreliable; most fall into teleost
table(trip_sum$FALSE_STRIKE)

table(trip_sum$SETS_QUANTITY)
summary(trip_sum$SETS_QUANTITY)
hist(trip_sum$SETS_QUANTITY)

sum(trip_sum$SEA_DAYS, na.rm=T)
table(trip_sum$SEA_DAYS)
summary(trip_sum$SEA_DAYS)

table(trip_sum$SEA_DAYS, trip_sum$SETS_QUANTITY)

addmargins(table(year(trip_sum$DATE_LEFT), month(trip_sum$DATE_LEFT)))

image(1998:2024,1:12,table(year(trip_sum$DATE_LEFT), month(trip_sum$DATE_LEFT)))


barplot(table(trip_sum$TRIP_TARGET))
barplot(table(trip_sum$DEPARTURE_STATE))
barplot(table(year(trip_sum$DATE_LEFT)), las = 2)
barplot(table(month(trip_sum$DATE_LEFT)), las = 2)


setwd("C:/Users/brendan.turley/Documents/data/Fishery_observer_data/GillnetExtraction for Coastal Pelagics20240924044848/Gillnet Request (09_24_24)/Gillnet Request (09_24_24)")
haul <- read_excel('HAUL_LOG.xlsx')
haul <- utils::type.convert(haul)

table(haul$TARGET_SPECIES)

hist(haul$WAVE_HEIGHT_FT)
hist(haul$WIND_SPEED_KTS)
hist(haul$SET_BEGIN_TIME)

barplot(table(haul$BOTTOM_TYPE_DESC))

hist(haul$BOTTOM_DEPTH_FT)

hist(haul$GEAR_SOAK_HOURS)
summary(haul$GEAR_SOAK_HOURS)

plot(-haul$SET_BEGIN_LONG_CONV, haul$SET_BEGIN_LAT_CONV, ylim = c(24,45), asp = 1)

with(subset(haul, TARGET_SPECIES=='King Mackerel'),
     plot(-SET_BEGIN_LONG_CONV, SET_BEGIN_LAT_CONV, asp = 1))

with(subset(haul, TARGET_SPECIES=='Mixed Species'),
     plot(-SET_BEGIN_LONG_CONV, SET_BEGIN_LAT_CONV, asp = 1))

plot(haul$HAUL_BEGIN_TIME, haul$GEAR_SOAK_HOURS)


setwd("C:/Users/brendan.turley/Documents/data/Fishery_observer_data/GillnetExtraction for Coastal Pelagics20240924044848/Gillnet Request (09_24_24)/Gillnet Request (09_24_24)")
catch <- read_excel('CATCH_LOG.xlsx')

sort(table(catch$SP_COMMON_NAME))

kmk_catch <- subset(catch, SP_COMMON_NAME=='King Mackerel')
kmk_trips <- sort(unique(kmk_catch$TRIP_ID))

kmk_haul_ex <- subset(haul, is.element(haul$TRIP_ID, kmk_trips))
plot(-kmk_haul_ex$SET_BEGIN_LONG_CONV, kmk_haul_ex$SET_BEGIN_LAT_CONV, asp = 1, col = 4)
map('state', add = T, resolution = 0)

table(year(ymd(kmk_haul_ex$SET_BEGIN_DATE)), month(ymd(kmk_haul_ex$SET_BEGIN_DATE)))




kmk_haul <- subset(haul, TARGET_SPECIES=='King Mackerel' & SET_BEGIN_LAT_CONV<29)
kmk_haul$SET_BEGIN_DATE <- ymd(kmk_haul$SET_BEGIN_DATE)
addmargins(table(month(kmk_haul$SET_BEGIN_DATE), year(kmk_haul$SET_BEGIN_DATE)))
table(yday(kmk_haul$SET_BEGIN_DATE), year(kmk_haul$SET_BEGIN_DATE))

length(unique(kmk_haul$TRIP_ID))

plot(-kmk_haul$SET_BEGIN_LONG_CONV, kmk_haul$SET_BEGIN_LAT_CONV, asp = 1)

plot(year(kmk_haul$SET_BEGIN_DATE), yday(kmk_haul$SET_BEGIN_DATE))
boxplot(yday(kmk_haul$SET_BEGIN_DATE) ~ year(kmk_haul$SET_BEGIN_DATE))
boxplot((kmk_haul$HAUL_BEGIN_TEMP) ~ year(kmk_haul$SET_BEGIN_DATE))
boxplot((kmk_haul$HAUL_BEGIN_TEMP) ~ yday(kmk_haul$SET_BEGIN_DATE))
boxplot((kmk_haul$BOTTOM_DEPTH_FT) ~ yday(kmk_haul$SET_BEGIN_DATE))
plot(kmk_haul$BOTTOM_DEPTH_FT, kmk_haul$HAUL_BEGIN_TEMP)
barplot(table(year(kmk_haul$SET_BEGIN_DATE)))
table(kmk_haul$GEAR_TYPE)
table(kmk_haul$TRIP_TYPE)
plot(-kmk_haul$SET_BEGIN_LONG_CONV, kmk_haul$SET_BEGIN_LAT_CONV, asp = 1)
plot(year(kmk_haul$SET_BEGIN_DATE), kmk_haul$SET_BEGIN_LAT_CONV)
plot(year(kmk_haul$SET_BEGIN_DATE), kmk_haul$SET_BEGIN_LONG_CONV)
plot(year(kmk_haul$SET_BEGIN_DATE), kmk_haul$BOTTOM_DEPTH_FT)
plot(yday(kmk_haul$SET_BEGIN_DATE), kmk_haul$SET_BEGIN_LAT_CONV)
hist(kmk_haul$SET_BEGIN_TEMP)
hist(kmk_haul$HAUL_BEGIN_TEMP)
summary(kmk_haul$HAUL_BEGIN_TEMP)

kmk_id <- unique(kmk_haul$TRIP_ID)

kmk_trip <- trip_sum[is.element(trip_sum$TRIP_ID, kmk_id),]

sort(table(kmk_trip$VESSEL_NAME))
table(kmk_trip$SEA_DAYS)
sort(table(kmk_trip$DEPARTURE_CITY))
sort(table(kmk_trip$LANDING_CITY))
table(kmk_trip$DEPARTURE_CITY, kmk_trip$LANDING_CITY)
table(kmk_trip$FALSE_STRIKE)
table(kmk_trip$SEA_DAYS, kmk_trip$SETS_QUANTITY)
table(kmk_trip$DEPARTURE_CITY, kmk_trip$SETS_QUANTITY)
table(kmk_trip$LANDING_CITY, kmk_trip$SETS_QUANTITY)
table(kmk_trip$DEPARTURE_CITY, kmk_trip$SEA_DAYS)
table(kmk_trip$LANDING_CITY, kmk_trip$SEA_DAYS)

table(year(kmk_trip$DATE_LANDED), kmk_trip$SEA_DAYS)

kmk_catch <- catch[is.element(catch$TRIP_ID, kmk_id),]
kmk_catch_kmk <- subset(kmk_catch, SP_COMMON_NAME=='King Mackerel')

with(subset(kmk_catch, SP_COMMON_NAME=='King Mackerel'),
     aggregate(cbind(NUMBER_CAUGHT, TOTAL_NUM_KEPT, DISCARDED_LIVE, DISCARDED_DEAD, WEIGHT_LANDED),
               by = list(TRIP_ID), sum, na.rm = T))

aggregate(kmk_catch$NUMBER_CAUGHT, by = list(kmk_catch$TRIP_ID, kmk_catch$SP_COMMON_NAME), sum, na.rm = T)


# haul keep: vessel, port, date set, temp, depth, location, caught, kept, discard

kmk_haul_catch <- merge(kmk_haul, kmk_catch_kmk, by = 'TRIP_ID')

plot(-kmk_haul_catch$SET_BEGIN_LONG_CONV, kmk_haul_catch$SET_BEGIN_LAT_CONV, 
     asp = 1, cex = kmk_haul_catch$NUMBER_CAUGHT/1000)

plot(year(kmk_haul_catch$SET_BEGIN_DATE), kmk_haul_catch$NUMBER_CAUGHT)
plot(year(kmk_haul_catch$SET_BEGIN_DATE), kmk_haul_catch$DISCARDED_DEAD)
plot(kmk_haul_catch$SET_BEGIN_TEMP, (kmk_haul_catch$NUMBER_CAUGHT))
plot(kmk_haul_catch$SET_BEGIN_TEMP, (kmk_haul_catch$DISCARDED_DEAD))

### this is wrong because there is double counting/pseudo-reps of temp obs
# plot(density(kmk_haul_catch$SET_BEGIN_TEMP, 
#         weights = kmk_haul_catch$NUMBER_CAUGHT/sum(kmk_haul_catch$NUMBER_CAUGHT, na.rm = T),
#         na.rm = T))
# lines(density(kmk_haul_catch$SET_BEGIN_TEMP, na.rm = T), col = 2)


plot(-kmk_haul$SET_BEGIN_LONG_CONV, kmk_haul$SET_BEGIN_LAT_CONV,
     # xlim = range(kmk_haul$SET_BEGIN_LONG_CONV)
     asp = 1)
plot(fl_shp, add = T)
