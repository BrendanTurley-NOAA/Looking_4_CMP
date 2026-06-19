
library(dplyr)
library(geosphere)
library(lubridate)
library(mgcv)
library(NISTunits)
library(scales)
library(units)


### KMK sdm using seamap trawl data
### Scomberomorus cavalla


### load data
setwd("~/data/seamap/SEAMAPDATAV3CSV")

vessel <- read.csv('VESSELS.csv')
cruise <- read.csv('cruises.csv')
sta <- read.csv('STAREC.csv')
env <- read.csv('ENVREC.csv')
invrec <- read.csv('INVREC.csv')
catch <- read.csv('BGSREC.csv')
lthfreq <- read.csv('GLFREC.csv')
bcodes <- read.csv('BIOCODES.csv')
# merrec <- read.csv('MERREC.csv')

kmk <- bcodes[grep('Scomberomorus cavalla', bcodes$TAXON, ignore.case = T),]
catch_kmk <- subset(catch, BIO_BGS %in% kmk$BIOCODE)
# merrec_kmk <- subset(merrec, BGSID %in% catch_kmk$BGSID)
lthfreq_kmk <- subset(lthfreq, BGSID %in% catch_kmk$BGSID)

sta_kmk <- subset(sta, STATIONID %in% catch_kmk$STATIONID)

plot(sta_kmk$DECSLON,sta_kmk$DECSLAT, asp = 1)

sta_kmk0 <- subset(sta, CRUISEID %in% catch_kmk$CRUISEID)
sta_kmk0.1 <- subset(sta, CRUISE_NO %in% catch_kmk$CRUISE_NO)

plot(sta_kmk$DECSLON,sta_kmk$DECSLAT, asp = 1)
points(sta_kmk0$DECSLON,sta_kmk0$DECSLAT, col = 2)

### only shrimp trawl gear size 40
gear_sta_id <- subset(invrec, GEAR_SIZE==40 & GEAR_TYPE=='ST', select = 'STATIONID')
catch_kmk <- subset(catch_kmk, STATIONID %in% gear_sta_id$STATIONID)

### pull location (lon/lat), date-time, cruise, vessel, kmk # and weight, depth, sst, chl, sbt, wind sp and dir, wave ht and dir, air temp, air press
# join cruise, vessel, station, env, catch
# Join keys:
# CRUISEID    — Links all tables to their parent cruise
# STATIONID   — Links station-level tables to STAREC
# INVRECID    — Links BGSREC to its INVREC gear deployment
# BGSID       — Links GLFREC/MERREC to their BGSREC species record
# CTDID       — Links CTDCASTREC depth bins to their CTD cast header
# VESSEL + CRUISE_NO + P_STA_NO — Natural composite key (present in most tables)

# sta_cr <- left_join(sta, cruise, by = 'CRUISEID')
# sta_kmk <- subset(sta, STATIONID %in% catch_kmk$STATIONID)

all_merge <- merge(catch_kmk, sta, by = c('STATIONID', 'CRUISEID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO'), all.x = T) |>
  merge(env, by = c('STATIONID', 'CRUISEID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO'), all.x = T) |>
  merge(cruise, by = c('CRUISEID', 'VESSEL', 'CRUISE_NO'), all.x = T) |>
  merge(invrec, by = c('INVRECID','STATIONID', 'CRUISEID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO'), all.x = T) |>
  subset(OP=='')

### add stations with zero kmk but from the cruises that did catch kmk
all0_merge <- merge(sta_kmk0, env, by = c('STATIONID', 'CRUISEID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO'), all.x = T) |>
  merge(cruise, by = c('CRUISEID', 'VESSEL', 'CRUISE_NO'), all.x = T) |>
  merge(invrec, by = c('STATIONID', 'CRUISEID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO'), all.x = T) |>
  # merge(invrec, by = c('INVRECID','STATIONID', 'CRUISEID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO'), all.x = T) |>
  subset(OP=='')

# table(all_merge$OP,useNA = 'always')
# table(all_merge$HAULVALUE,useNA = 'always')

### effort: mins, distance, km^2 = vessel_spd(kt) * 1.85184(kph) * min_fish/60 * gear_size(ft) * .3048/1000(km)
# all_merge$ves_spd_kph <- all_merge$VESSEL_SPD * 1.85184
# all_merge$hrs_fish <- all_merge$MIN_FISH / 60
# all_merge$gear_km <- all_merge$GEAR_SIZE * .3048/1000
all_merge$VESSEL_SPD <- set_units(all_merge$VESSEL_SPD, knots)
all_merge$VESSEL_SPD <- set_units(all_merge$VESSEL_SPD, km/h)
all_merge$hrs_fish <- set_units(all_merge$MIN_FISH / 60, hours)
all_merge$GEAR_SIZE <- set_units(all_merge$GEAR_SIZE, feet)
all_merge$GEAR_SIZE <- set_units(all_merge$GEAR_SIZE, km)
all_merge$dist_fish <- all_merge$VESSEL_SPD * all_merge$hrs_fish
all_merge$effort_km2 <- all_merge$dist_fish * all_merge$GEAR_SIZE

all0_merge$VESSEL_SPD <- set_units(all0_merge$VESSEL_SPD, knots)
all0_merge$VESSEL_SPD <- set_units(all0_merge$VESSEL_SPD, km/h)
all0_merge$hrs_fish <- set_units(all0_merge$MIN_FISH / 60, hours)
all0_merge$GEAR_SIZE <- set_units(all0_merge$GEAR_SIZE, feet)
all0_merge$GEAR_SIZE <- set_units(all0_merge$GEAR_SIZE, km)
all0_merge$dist_fish <- all0_merge$VESSEL_SPD * all0_merge$hrs_fish
all0_merge$effort_km2 <- all0_merge$dist_fish * all0_merge$GEAR_SIZE

### take mean location and depth
all_merge <- all_merge |>
  rowwise() |>
  mutate(lon = mean(c(DECSLON, DECELON), na.rm = T),
         lat = mean(c(DECSLAT, DECELAT), na.rm = T),
         depth = mean(c(DEPTH_SSTA, DEPTH_ESTA), na.rm = T))
all0_merge <- all0_merge |>
  rowwise() |>
  mutate(lon = mean(c(DECSLON, DECELON), na.rm = T),
         lat = mean(c(DECSLAT, DECELAT), na.rm = T),
         depth = mean(c(DEPTH_SSTA, DEPTH_ESTA), na.rm = T))

### convert dates
all_merge$tz <- recode_values(all_merge$TIME_ZN,
                              3 ~ 'America/Chicago',
                              4 ~ 'America/Chicago',
                              5 ~ 'America/Halifax',
                              8 ~ 'UTC')
all0_merge$tz <- recode_values(all0_merge$TIME_ZN,
                               2 ~ 'America/New_York',
                               3 ~ 'America/Chicago',
                               4 ~ 'America/Chicago',
                               5 ~ 'America/Halifax',
                               8 ~ 'UTC')

start_dt_ls <- ymd_hms(all_merge$START_DATE) |> as.list()
all_merge$start_utc <- ymd_hms('1900-01-01 01:01:01')
### each one have different TZ; convert to UTC
# force_tz then with_tz
for(i in 1:length(start_dt_ls)){
  all_merge$start_utc[i] <- force_tz(start_dt_ls[[i]], tzone = all_merge$tz[i]) |>
    with_tz(tzone = 'UTC') #|> paste()
}

start0_dt_ls <- ymd_hms(all0_merge$START_DATE) |> as.list()
all0_merge$start_utc <- ymd_hms('1900-01-01 01:01:01')
### each one have different TZ; convert to UTC
# force_tz then with_tz
for(i in 1:length(start0_dt_ls)){
  all0_merge$start_utc[i] <- force_tz(start0_dt_ls[[i]], tzone = all0_merge$tz[i]) |>
    with_tz(tzone = 'UTC')
}

### fill in missing tow lengths; all_merge[,c('LONGITUDE','LATITUDE')] not reliable
tow_lth <- distm(all_merge[,c('DECSLON','DECSLAT')],
                 all_merge[,c('DECELON','DECELAT')]) |> set_units(m) |> 
  diag() |> set_units(km)
all_merge$dist_fish[which(is.na(all_merge$dist_fish))] <- tow_lth[which(is.na(all_merge$dist_fish))]

tow0_lth <- distm(all0_merge[,c('DECSLON','DECSLAT')],
                 all0_merge[,c('DECELON','DECELAT')]) |> set_units(m) |> 
  diag() |> set_units(km)
df <- all0_merge %>%
  rowwise() %>%
  mutate(
    distance_meters = distm(
      c(DECSLON,DECSLAT), 
      c(DECELON,DECELAT), 
      fun = distHaversine
    )[, 1]
  ) %>%
  ungroup() |>
  select(distance_meters)
all0_merge$dist_fish[which(is.na(all0_merge$dist_fish))] <- tow0_lth[which(is.na(all0_merge$dist_fish))]

# plot(all_merge$dist_fish, tow_lth, asp = 1,
# panel.first = abline(0,1, col = 2, lwd = 2, lty = 5))
# plot(all_merge[,c('DECSLON','DECSLAT')], asp = 1)
# points(all_merge[,c('DECELON','DECELAT')], col = 2)




### use the operation code in invrec and haulvalue in starec to filter for quality
### things to poder:
# 1 which location data to use (start, stop, ctd); take midpoint of start and stop of tow
# 2 which time of day to use (start, stop, ctd); take midpoint of start and stop of tow
# 3 which environmental data to use ; quick suggests that the env data is more complete; but many fields are NA

### simple model 1
### cpue ~ sst + sss + schl + depth + lon + lat + jday + hour + year

kmk_pos <- all_merge |> 
  select(CNTEXP, SELECT_BGS, effort_km2, dist_fish, lon, lat, TIME_MIL, TIME_EMIL, tz, START_DATE, start_utc, 
         depth, TEMP_SSURF, TEMP_BOT, WIND_SPD, TEMPSURF, TEMPMAX, SALSURF,
         SALMAX, CHLORSURF, CHLORMAX, OXYSURF, OXYMAX, TURBSURF, TURBMAX)
kmk_pos$cpue <- kmk_pos$SELECT_BGS / kmk_pos$effort_km2 |> drop_units()
kmk_pos$jday <- kmk_pos$start_utc |> yday()
kmk_pos$TIME_MIL <- sprintf('%04d', kmk_pos$TIME_MIL)
kmk_pos$hour <- paste0(substr(kmk_pos$TIME_MIL,1,2),':',substr(kmk_pos$TIME_MIL,3,4)) |>
  hm() |> hour()

plot(kmk_pos$lon, kmk_pos$lat, cex = (kmk_pos$cpue)/100, asp = 1)
plot(kmk_pos$lon, kmk_pos$lat, cex = log10(kmk_pos$cpue), asp = 1, pch = 21, bg = alpha('gray20',.2))

cpue_model <- gam(
  cpue ~ s(TEMP_SSURF) + 
    s(SALSURF) + 
    s(CHLORSURF) + 
    s(depth) + 
    s(lon, lat) +               # 2D spatial smooth
    s(jday, bs = "cc") +        # Cyclic smooth for Julian day (wraps around)
    s(hour, bs = "cc") +        # Cyclic smooth for hour of day (wraps around)
    as.factor(year(start_utc)),            # Year treated as a factor/fixed effect
  data = kmk_pos,            # Replace with your dataset name
  family = tw(),                     # Tweedie distribution (ideal for zero-inflated CPUE)
  method = "REML"                    # Restricted Maximum Likelihood (highly recommended)
)

