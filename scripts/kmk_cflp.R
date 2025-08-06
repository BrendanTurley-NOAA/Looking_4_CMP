
rm(list=ls())
gc()
library(cmocean)
library(dplyr)
library(lubridate)
library(sf)
library(terra)
library(viridisLite)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GSHHS_shp/i")
world <- vect('GSHHS_i_L1.shp')

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/king_mackerel")
kmk <- vect('king_mackerel_po.shp')
# setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
# gom <- vect('GOM_2500ft.shp')

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/cflp_statgrid")
sz_shp <- vect('CFLP_StatGrid_2013_v20140210.shp') |>
  st_as_sf()
sz_shp$AREA_FISHED <- sz_shp$SZ_ID

plot(kmk, border = 4, lwd = 2)
plot(world, add = T, col = 'gray50')
# plot(st_geometry(sz_shp), add = T)

natl_st <- c('ME', 'NH', 'MA', 'RI', 'CT', 'NY', 'NJ', 'DE', 'MD', 'VA')
satl_st <- c('FL', 'GA', 'SC', 'NC')
gom_st <- c('FL', 'AL', 'MS', 'LA', 'TX')

setwd("C:/Users/brendan.turley/Documents/CMP/data/cflp")
# cflp <- readRDS('CFLPkarnauskas.rds')
## cflp_ne <- subset(cflp, LAND_YEAR<2024 & LAND_YEAR>1999 & CATCH_TYPE == 'CATCH') |>
##   subset(REGION == 'NATL' & is.element(ST_ABRV, natl_st))
# cflp <- subset(cflp, LAND_YEAR<2024 & LAND_YEAR>1999 & CATCH_TYPE == 'CATCH') |>
#   subset(REGION == 'SATL' & is.element(ST_ABRV, satl_st) |
#            REGION == 'GOM' & is.element(ST_ABRV, gom_st))
# gc()



### pull out handlines only
## table(cflp$GEAR)
# gear_keep <- c('H', 'E', 'TR')
# cflp_hl <- subset(cflp , is.element(cflp$GEAR, gear_keep)) |>
#   subset(FLAG_MULTIGEAR==0 & FLAG_MULTIAREA==0)
# saveRDS(cflp_hl, 'cflp_gulfsa_temp.rds')
# gc()

cflp_hl <- readRDS('cflp_gulfsa_temp.rds')
gc()

### following methods by Walter & McCarthy 2014 (1993-2013SEDAR38-DW-10) 
# CPUE = total kilograms of king mackerel/(number of lines fished*number of hooks per line*total hours fished) 
# hist(log(cflp$TOTAL_WHOLE_POUNDS + .01))
# hist(log(cflp$GUTTED + .01))
# hist(log(cflp$WHOLE + .01))
### total whole pounds seems most appropriate; other 2 have lots of zeros
cflp_hl$tot_kg <- cflp_hl$TOTAL_WHOLE_POUNDS / 2.205
# cflp$tot_kg <- cflp$GUTTED / 2.205
# cflp$tot_kg <- cflp$WHOLE / 2.205
cflp_hl$pue <- (cflp_hl$NUMGEAR * cflp_hl$EFFORT * cflp_hl$FISHED)
cflp_hl$cpue <- cflp_hl$tot_kg / cflp_hl$pue # kg catch per number of hooks X hours
cflp_hl$cpue <- ifelse(is.infinite(cflp_hl$cpue), NA, cflp_hl$cpue)
gc()

# table(cflp_hl$LAND_YEAR, cflp_hl$LAND_MONTH, useNA = 'always')
# with(subset(cflp_hl, REGION=='GOM' & COMMON_NAME=='MACKERELS, KING AND CERO'),
#      addmargins(table(LAND_YEAR, LAND_MONTH, useNA = 'always')))

### correct days_away
diff_time <- (cflp_hl$LAND_DATE - cflp_hl$DEPART_DATE)
units(diff_time) <- 'days'
# plot(cflp_hl$DAYS_AWAY, diff_time + 1)
# abline(0, 1, lty = 5, col = 2)
cflp_hl$days_away_corrected <- diff_time + 1
rm(diff_time)
gc()

### what does this do?
# kmk_tot <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO'),
#      aggregate(tot_kg ~ SCHEDULE_NUMBER + VESSEL_ID, FUN = sum, na.rm = T))
# tot <- aggregate(tot_kg ~ SCHEDULE_NUMBER + VESSEL_ID, data = cflp_hl, sum, na.rm = T)
# tot_merge <- merge(kmk_tot, tot, by = c('SCHEDULE_NUMBER', 'VESSEL_ID'))
# tot_merge$kmk_pro <- tot_merge$tot_kg.x / tot_merge$tot_kg.y
# hist(tot_merge$kmk_pro) # proporition of KMK caught per trip
# 
# trip <- which(tot_merge$kmk_pro > .2)
# cflp_hl_0 <- cflp_hl[is.element(cflp_hl$SCHEDULE_NUMBER, tot_merge$SCHEDULE_NUMBER[trip]), ]
# rm(kmk_tot, tot, tot_merge, trip)
# gc()
### end


### new try at this; Walter sorts by year and then total landings to get at the 80%
vessel_yrs <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO',
       select = c(LAND_YEAR, VESSEL_ID, REGION)),
  aggregate(LAND_YEAR ~ VESSEL_ID + REGION, FUN = function(x) length(unique(x))))
vessel_tot <- with(subset(cflp_hl,
                          select = c(tot_kg, VESSEL_ID, REGION)),
                   aggregate(tot_kg ~ VESSEL_ID + REGION, FUN = sum, na.rm = T))
vessel_kmk_tot <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO',
                          select = c(tot_kg, VESSEL_ID, REGION)),
                   aggregate(tot_kg ~ VESSEL_ID + REGION, FUN = sum, na.rm = T)) |>
  setNames(c("VESSEL_ID","REGION","kmk_tot_kg"))
vessel_select <- merge(vessel_yrs, vessel_tot, by = c('VESSEL_ID', 'REGION')) |>
  merge(vessel_kmk_tot, by = c('VESSEL_ID', 'REGION'))
vessel_select$kmk_pro <- vessel_select$kmk_tot_kg / vessel_select$tot_kg
vessel_select <- vessel_select[order(vessel_select$LAND_YEAR, vessel_select$kmk_tot_kg, 
                                     vessel_select$kmk_pro,
                                     decreasing = T), ]
gom_ves <- subset(vessel_select, REGION=='GOM')
sa_ves <- subset(vessel_select, REGION=='SATL')
gom_ves$cummulative <- cumsum(gom_ves$tot_kg)/sum(gom_ves$tot_kg)
sa_ves$cummulative <- cumsum(sa_ves$tot_kg)/sum(sa_ves$tot_kg)
gom_ves_id <- gom_ves$VESSEL_ID[which(gom_ves$cummulative<=.8)]
sa_ves_id <- sa_ves$VESSEL_ID[which(sa_ves$cummulative<=.8)]
kmk_ves <- union(gom_ves_id,sa_ves_id)

cflp_hl_0 <- cflp_hl[is.element(cflp_hl$VESSEL_ID, kmk_ves), ] |>
  subset(
    NUMGEAR < quantile(cflp_hl$NUMGEAR, .995, na.rm = T) &
      EFFORT < quantile(cflp_hl$EFFORT, .995, na.rm = T) &
      FISHED < quantile(cflp_hl$FISHED, .995, na.rm = T)
  )
cflp_hl_1 <- cflp_hl_0

rm(cflp, cflp_hl, cflp_hl_0, vessel_select, vessel_kmk_tot, vessel_yrs, vessel_tot)
gc()
saveRDS(cflp_hl_1, 'cflp_hl_1.rds')



### alternative
### new try at this; Walter sorts by year and then total landings to get at the 80%
vessel_yrs <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO',
                          select = c(LAND_YEAR, VESSEL_ID, REGION, ST_ABRV)),
                   aggregate(LAND_YEAR ~ VESSEL_ID + REGION + ST_ABRV, FUN = function(x) length(unique(x))))
vessel_tot <- with(subset(cflp_hl,
                          select = c(tot_kg, VESSEL_ID, REGION, ST_ABRV)),
                   aggregate(tot_kg ~ VESSEL_ID + REGION + ST_ABRV, FUN = sum, na.rm = T))
vessel_kmk_tot <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO',
                              select = c(tot_kg, VESSEL_ID, REGION, ST_ABRV)),
                       aggregate(tot_kg ~ VESSEL_ID + REGION + ST_ABRV, FUN = sum, na.rm = T)) |>
  setNames(c("VESSEL_ID","REGION",'ST_ABRV',"kmk_tot_kg"))
vessel_select <- merge(vessel_yrs, vessel_tot, by = c('VESSEL_ID', 'REGION','ST_ABRV')) |>
  merge(vessel_kmk_tot, by = c('VESSEL_ID','REGION','ST_ABRV'))
vessel_select$kmk_pro <- vessel_select$kmk_tot_kg / vessel_select$tot_kg
vessel_select <- vessel_select[order(vessel_select$LAND_YEAR, vessel_select$kmk_tot_kg, 
                                     vessel_select$kmk_pro,
                                     decreasing = T), ]
n <- 1
ves_id <- list()
for(i in c('GOM','SATL')){
  ti <- subset(vessel_select, REGION==i)
  if(i=='GOM') st_sl <- gom_st
  if(i=='SATL') st_sl <- satl_st
  for(j in st_sl){
    tj <- subset(ti, ST_ABRV==j)
    tj$cummulative <- cumsum(tj$tot_kg)/sum(tj$tot_kg)
    ves_id[[n]] <- tj$VESSEL_ID[which(tj$cummulative<=.8)]
    n <- n + 1
  }
}
kmk_ves <- unique(unlist(ves_id))

cflp_hl_0 <- cflp_hl[is.element(cflp_hl$VESSEL_ID, kmk_ves), ] |>
  subset(
    NUMGEAR < quantile(cflp_hl$NUMGEAR, .995, na.rm = T) &
      EFFORT < quantile(cflp_hl$EFFORT, .995, na.rm = T) &
      FISHED < quantile(cflp_hl$FISHED, .995, na.rm = T)
  )
cflp_hl_1 <- cflp_hl_0

### pull vessels landing the top 80% of KMK; depreciated code wrongly interpreted
# vessel_landings <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO'),
#                         aggregate(tot_kg ~ VESSEL_ID + REGION + ST_ABRV, FUN = sum, na.rm = T))
# vessel_landings_else <- aggregate(tot_kg ~ VESSEL_ID + REGION + ST_ABRV, data = cflp_hl,
#                                   FUN = sum, na.rm = T)
# ves_land <- merge(vessel_landings, vessel_landings_else, by = c('VESSEL_ID', 'REGION', 'ST_ABRV')) |>
#   setNames(c('VESSEL_ID', 'REGION', 'ST_ABRV', 'kmk_tot_kg', 'all_tot_kg'))
# ves_land$kmk_prop <- ves_land$kmk_tot_kg / ves_land$all_tot_kg

# quantile(vessel_landings$tot_kg, seq(0,1,.1))
# hist(vessel_landings$tot_kg)
# quantile(ves_land$kmk_prop, seq(0,1,.1))
# hist(ves_land$kmk_prop)

# cutoff1 <- aggregate(kmk_tot_kg ~ REGION + ST_ABRV, data = ves_land, quantile, .2) |>
#   setNames(c("REGION", "ST_ABRV", "cutoff1"))
# cutoff2 <- aggregate(kmk_prop ~ REGION + ST_ABRV, data = ves_land, quantile, .2) |>
#   setNames(c("REGION", "ST_ABRV", "cutoff2"))

# ves_land <- merge(ves_land, cutoff1, by = c('REGION','ST_ABRV'), all.x = T) |>
#   merge(cutoff2, by = c('REGION','ST_ABRV'), all.x = T)
# kmk_ves <- ves_land$VESSEL_ID[which(ves_land$kmk_tot_kg > ves_land$cutoff1 |
#                                       ves_land$kmk_prop > ves_land$cutoff2)]

# cflp_hl_1 <- cflp_hl[is.element(cflp_hl$VESSEL_ID, kmk_ves), ] |>
#   subset(
#     NUMGEAR < quantile(cflp_hl$NUMGEAR, .995, na.rm = T) &
#       EFFORT < quantile(cflp_hl$EFFORT, .995, na.rm = T) &
#       FISHED < quantile(cflp_hl$FISHED, .995, na.rm = T)
#   )
# rm(ves_land, vessel_landings, vessel_landings_else, cutoff1, cutoff2)
# gc()
### end


### pull vessels landing the top 80% of KMK; alternative; depreciated code wrongly interpreted
# vessel_landings <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO'),
#                         aggregate(tot_kg ~ VESSEL_ID, FUN = sum, na.rm = T))
# vessel_landings_else <- aggregate(tot_kg ~ VESSEL_ID, data = cflp_hl,
#                                   FUN = sum, na.rm = T)
# ves_land <- merge(vessel_landings, vessel_landings_else, by = c('VESSEL_ID')) |>
#   setNames(c('VESSEL_ID', 'kmk_tot_kg', 'all_tot_kg'))
# ves_land$prop <- ves_land$kmk_tot_kg / ves_land$all_tot_kg
# 
# cutoff <- quantile(ves_land$kmk_tot_kg, .2)
# cutoff2 <- quantile(ves_land$prop, .2)
# 
# plot(ves_land$kmk_tot_kg, ves_land$prop, log='xy')
# abline(v = cutoff, h = cutoff2, col = 2, lty = 5)
# with(subset(ves_land, kmk_tot_kg > cutoff | prop > cutoff2),
#      points(kmk_tot_kg, prop, col = 4))
# with(subset(ves_land, kmk_tot_kg > cutoff & prop > cutoff2),
#      points(kmk_tot_kg, prop, col = 3))
# 
# kmk_ves2 <- ves_land$VESSEL_ID[which(ves_land$kmk_tot_kg > cutoff |
#                                              ves_land$prop > cutoff2)]
# 
# cflp_hl_2 <- cflp_hl[is.element(cflp_hl$VESSEL_ID, kmk_ves2), ]
### end


### cPUE per region overtime
cpue_yr_reg <- aggregate(cpue ~ LAND_YEAR + REGION + COMMON_NAME,
                         data = cflp_hl_1,
                         mean, na.rm = T)
gc()

with(subset(cpue_yr_reg, COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, cpue, typ = 'n', xlab = 'year', ylab = 'CPUE (kg / hook-hours)'))
with(subset(cpue_yr_reg, REGION=='GOM' & COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, cpue, typ = 'o', pch = 16))
with(subset(cpue_yr_reg, REGION=='SATL' & COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, cpue, typ = 'o', col = 2, pch = 16))
abline(h = with(subset(cpue_yr_reg, REGION=='GOM' & COMMON_NAME=='MACKERELS, KING AND CERO'),
                mean(cpue)),
       lty = 2)
abline(h = with(subset(cpue_yr_reg, REGION=='SATL' & COMMON_NAME=='MACKERELS, KING AND CERO'),
                mean(cpue)),
       col = 2, lty = 2)
# abline(v = 2013, col = 4, lty = 2)
grid()
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(cpue ~ LAND_YEAR, 
          data = subset(cpue_yr_reg, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1, lwd = 2)
abline(lm(cpue ~ LAND_YEAR, 
          data = subset(cpue_yr_reg, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2, lwd = 2)
### end


### CPUE per state over time
cpue_yr_st <- aggregate(cpue ~ LAND_YEAR + ST_ABRV + REGION + COMMON_NAME,
                        data = cflp_hl_1,
                        mean, na.rm = T)
gc()

#GOM
gulf <- c('AL', 'FL', 'LA', 'MS', 'TX')
with(subset(cpue_yr_st, is.element(ST_ABRV, gulf) & 
              REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, cpue, typ = 'n', xlab = 'year', ylab = 'CPUE (kg / hook-hours)'))
for(i in 1:length(gulf)){
  with(subset(cpue_yr_st, ST_ABRV==gulf[i] & 
                REGION=='GOM' &
                COMMON_NAME=='MACKERELS, KING AND CERO'),
       points(LAND_YEAR, cpue, typ = 'o', col = i, pch = 16, lwd = 2))
}
grid()
legend('topleft',gulf, lty=1, col=1:length(gulf),bty = 'n', pch = 16, lwd = 2)

#SATL
satl <- c('FL', 'GA', 'NC', 'SC') #'NJ', 'NY', 'VA')
with(subset(cpue_yr_st, is.element(ST_ABRV, satl) & 
              REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, cpue, typ = 'n', xlab = 'year', ylab = 'CPUE (kg / hook-hours)'))
for(i in 1:length(satl)){
  with(subset(cpue_yr_st, ST_ABRV==satl[i] & 
                REGION=='SATL' &
                COMMON_NAME=='MACKERELS, KING AND CERO'),
       points(LAND_YEAR, cpue, typ = 'o', col = i, pch = 16, lwd = 2))
}
grid()
legend('topleft',satl, lty=1, col=1:length(satl),bty = 'n', pch = 16, lwd = 2)
### end


### has the length of trips changed or have there been less days fished?
days_yr <- aggregate(days_away_corrected ~ LAND_YEAR + REGION + COMMON_NAME,
                     data = cflp_hl_1,
                     mean, na.rm = T) #median value not informative

ves_days_yr <- aggregate(days_away_corrected ~ LAND_YEAR + REGION + COMMON_NAME + VESSEL_ID,
                         data = cflp_hl_1,
                         mean, na.rm = T) #median value not informative

days_yr_region <- with(subset(ves_days_yr, COMMON_NAME=='MACKERELS, KING AND CERO'),
                       aggregate(days_away_corrected ~ LAND_YEAR + REGION,
                                 FUN = mean, na.rm = T))
days_yr_region_sd <- with(subset(ves_days_yr, COMMON_NAME=='MACKERELS, KING AND CERO'),
                          aggregate(days_away_corrected ~ LAND_YEAR + REGION,
                                    FUN = sd, na.rm = T))

kmk_ufdays <- with(subset(cflp_hl_1, COMMON_NAME=='MACKERELS, KING AND CERO'),
                   data.frame(d_day = yday(DEPART_DATE),
                              l_day = yday(LAND_DATE))) |>
  mutate(diff = l_day - d_day)



test <- subset(cflp_hl_1, COMMON_NAME=='MACKERELS, KING AND CERO', 
               select = c(SCHEDULE_NUMBER, LAND_YEAR, REGION, DEPART_DATE, LAND_DATE)) |>
  setNames(c('trip','year','region','t.1','t.2'))
test2 <- reshape(test, direction = "long", idvar = c('trip'), varying = list(names(test)[c(4,5)]))
test2$jday <- yday(test2$t.1)

fdays <- aggregate(jday ~ year + region, data = test2, function(x) length(unique(x)))

plot(fdays$year, fdays$jday, typ = 'n', xlab = 'year', ylab = 'Julian Days fished')
with(subset(fdays, region=='GOM'),
     points(year, jday, typ = 'o', pch = 16))
with(subset(fdays, region=='SATL'),
     points(year, jday, typ = 'o', pch = 16, col = 2))
grid()
legend('bottomleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')


test <- subset(cflp_hl_1, COMMON_NAME=='MACKERELS, KING AND CERO', 
               select = c(SCHEDULE_NUMBER, LAND_YEAR, REGION, ST_ABRV, DEPART_DATE, LAND_DATE)) |>
  setNames(c('trip','year','region','st','t.1','t.2'))
test2 <- reshape(test, direction = "long", idvar = c('trip'), varying = list(names(test)[c(5,6)]))
test2$jday <- yday(test2$t.1)

fdays <- aggregate(jday ~ year + region + st, data = test2, function(x) length(unique(x)))

gulf <- c('AL', 'FL', 'LA', 'MS', 'TX')
plot(fdays$year, fdays$jday, typ = 'n', xlab = 'year', ylab = 'Julian Days fished')
for(i in 1:length(gulf)){
  with(subset(fdays, st==gulf[i] & 
                region=='GOM'),
       points(year, jday, typ = 'o', col = i, pch = 16, lwd = 2))
}
grid()
legend('topleft',gulf, lty=1, col=1:length(gulf),bty = 'n', pch = 16, lwd = 2)

satl <- c('FL', 'GA', 'NC', 'SC')
plot(fdays$year, fdays$jday, typ = 'n', xlab = 'year', ylab = 'Julian Days fished')
for(i in 1:length(satl)){
  with(subset(fdays, st==satl[i] & 
                region=='SATL'),
       points(year, jday, typ = 'o', col = i, pch = 16, lwd = 2))
}
grid()
legend('bottomleft',satl, lty=1, col=1:length(satl),bty = 'n', pch = 16, lwd = 2)



with(subset(days_yr, COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, days_away_corrected, typ = 'n', xlab = 'year', ylab = 'Length of trip (days)'))
grid()
with(subset(days_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, days_away_corrected, typ = 'o', pch = 16))
with(subset(days_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, days_away_corrected, typ = 'o', pch = 16, col = 2))
# abline(v = 2013, lty = 5)
legend('topright', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(days_away_corrected ~ LAND_YEAR,
          data = subset(days_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(days_away_corrected ~ LAND_YEAR,
          data = subset(days_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)

with(days_yr_region,
     plot(LAND_YEAR, days_away_corrected, typ = 'n'))
grid()
with(subset(days_yr_region, REGION=='GOM'),
     points(LAND_YEAR, days_away_corrected, typ = 'o', pch = 16))
with(subset(days_yr_region, REGION=='SATL'),
     points(LAND_YEAR, days_away_corrected, typ = 'o', pch = 16, col = 2))
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(days_away_corrected ~ LAND_YEAR,
          data = subset(days_yr_region, REGION=='GOM')), col = 1)
abline(lm(days_away_corrected ~ LAND_YEAR,
          data = subset(days_yr_region, REGION=='SATL')), col = 2)


with(days_yr_region_sd,
     plot(LAND_YEAR, days_away_corrected, typ = 'n'))
grid()
with(subset(days_yr_region_sd, REGION=='GOM'),
     points(LAND_YEAR, days_away_corrected, typ = 'o', pch = 16))
with(subset(days_yr_region_sd, REGION=='SATL'),
     points(LAND_YEAR, days_away_corrected, typ = 'o', pch = 16, col = 2))
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(days_away_corrected ~ LAND_YEAR,
          data = subset(days_yr_region_sd, REGION=='GOM')), col = 1)
abline(lm(days_away_corrected ~ LAND_YEAR,
          data = subset(days_yr_region_sd, REGION=='SATL')), col = 2)
### end 




### none of these (hours, effort, numgear) are useful
hrs_yr <- aggregate(FISHED ~ LAND_YEAR + REGION + COMMON_NAME,
                     data = cflp_hl_1,
                    mean, na.rm = T) #median value not informative

num_yr <- aggregate(NUMGEAR ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_1,
                    mean, na.rm = T)

eff_yr <- aggregate(EFFORT ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_1,
                    mean, na.rm = T)

ves_yr <- aggregate(VESSEL_ID ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_1,
                    function(x) length(unique(x)))

trps_yr <- aggregate(SCHEDULE_NUMBER ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_1,
                    function(x) length(unique(x)))



with(subset(hrs_yr, COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, FISHED, typ = 'n', xlab = 'year', ylab = 'Hours fished'))
grid()
with(subset(hrs_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, FISHED, typ = 'o', pch = 16))
with(subset(hrs_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, FISHED, typ = 'o', pch = 16, col = 2))
# abline(v = 2013, lty = 5)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(FISHED ~ LAND_YEAR,
          data = subset(hrs_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(FISHED ~ LAND_YEAR,
          data = subset(hrs_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)


with(subset(eff_yr, COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, EFFORT, typ = 'n', xlab = 'year', ylab = 'Hooks per line'))
grid()
with(subset(eff_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, EFFORT, typ = 'o', pch = 16))
with(subset(eff_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, EFFORT, typ = 'o', pch = 16, col = 2))
# abline(v = 2013, lty = 5)
legend('topright', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(EFFORT ~ LAND_YEAR,
          data = subset(eff_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(EFFORT ~ LAND_YEAR,
          data = subset(eff_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)

with(subset(num_yr,COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, NUMGEAR, typ = 'n', xlab = 'year', ylab = 'Number of Gear'))
grid()
with(subset(num_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, NUMGEAR, typ = 'o', pch = 16))
with(subset(num_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, NUMGEAR, typ = 'o', pch = 16, col = 2))
# abline(v = 2013, lty = 5)
legend('topright', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(NUMGEAR ~ LAND_YEAR,
          data = subset(num_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(NUMGEAR ~ LAND_YEAR,
          data = subset(num_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)


with(subset(ves_yr, COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, VESSEL_ID, typ = 'n', xlab = 'year', ylab = 'Number of vessels fishing'))
grid()
with(subset(ves_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, VESSEL_ID, typ = 'o', pch = 16))
with(subset(ves_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, VESSEL_ID, typ = 'o', pch = 16, col = 2))
# abline(v = 2013, lty = 5)
legend('topright', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(VESSEL_ID ~ LAND_YEAR, 
          data = subset(ves_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(VESSEL_ID ~ LAND_YEAR, 
          data = subset(ves_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)


with(subset(trps_yr, COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, SCHEDULE_NUMBER, typ = 'n', xlab = 'year', ylab = 'Number of fishing trips'))
grid()
with(subset(trps_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, SCHEDULE_NUMBER, typ = 'o', pch = 16))
with(subset(trps_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, SCHEDULE_NUMBER, typ = 'o', pch = 16, col = 2))
# abline(v = 2013, lty = 5)
legend('topright', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(SCHEDULE_NUMBER ~ LAND_YEAR, 
          data = subset(trps_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(SCHEDULE_NUMBER ~ LAND_YEAR, 
          data = subset(trps_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)






### spatial footprint of trips over time

cpue_yr_area_region <- aggregate(cpue ~ LAND_YEAR + AREA_FISHED + COMMON_NAME + REGION,
                                 data = cflp_hl_1,
                                 median, na.rm = T)
with(subset(cpue_yr_area_region, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO' &
              LAND_YEAR>2012),
     which.max(cpue))

boxplot(cpue ~ LAND_YEAR, data = subset(cpue_yr_area_region, REGION=='GOM' &
                                          COMMON_NAME=='MACKERELS, KING AND CERO' &
                                          LAND_YEAR>2012),
        pch = 16, lty = 1, varwidth = T, staplewex = 0, lwd = 2)

boxplot(cpue ~ LAND_YEAR, data = subset(cpue_yr_area_region, REGION=='SATL' &
                                          COMMON_NAME=='MACKERELS, KING AND CERO'),
        pch = 16, lty = 1, varwidth = T, staplewex = 0, lwd = 2)


setwd("~/R_projects/misc-noaa-scripts/figs")
png('kmk_cpue_area2.png', width = 8, height = 5, units = 'in', res = 300)
par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(aggregate(AREA_FISHED ~ LAND_YEAR, 
               data = subset(cpue_yr_area_region,
                             REGION=='GOM' &
                               COMMON_NAME=='MACKERELS, KING AND CERO' &
                               LAND_YEAR>2012),
               length),
     typ = 'o',ylab = 'Number of areas fished', pch = 16)
grid()
dev.off()

par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(aggregate(AREA_FISHED ~ LAND_YEAR, 
               data = subset(cpue_yr_area_region,
                             REGION=='SATL' &
                               COMMON_NAME=='MACKERELS, KING AND CERO'),
               length),
     typ = 'o',ylab = 'Number of areas fished', pch = 16)
grid()

kmk_area <- aggregate(AREA_FISHED ~ LAND_YEAR, 
                      data = subset(cpue_yr_area_region,
                                    REGION=='GOM' &
                                      COMMON_NAME=='MACKERELS, KING AND CERO' &
                                      LAND_YEAR>2012), length)
1 - kmk_area$AREA_FISHED[11] / median(kmk_area$AREA_FISHED[1:10])


area_cnt <- aggregate(LAND_YEAR ~ AREA_FISHED, 
                      data = subset(cpue_yr_area_region,
                                    REGION=='GOM' &
                                      COMMON_NAME=='MACKERELS, KING AND CERO' &
                                      LAND_YEAR>2012),
                      length)
area_cnt2 <- aggregate(LAND_YEAR ~ AREA_FISHED, 
                       data = subset(cpue_yr_area_region,
                                     REGION=='GOM' &
                                       COMMON_NAME=='MACKERELS, KING AND CERO' &
                                       LAND_YEAR>2021),
                       length)


kmk_areas <-  merge(area_cnt,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf
plot(kmk_areas['LAND_YEAR'], pal = cmocean('haline')(10))

kmk_areas2 <-  merge(area_cnt2, 
                     sz_shp,
                     by = c('AREA_FISHED')) %>%
  st_as_sf

plot(kmk_areas2['LAND_YEAR'], pal = c('magenta2','purple4'))


setwd("~/R_projects/misc-noaa-scripts/figs")
png('kmk_areas_map.png', width = 7, height = 4, units = 'in', res = 300)
plot(world, xlim = c(-97.5,-80), ylim = c(24, 31))
plot(kmk_areas['LAND_YEAR'], pal = 'gray90', breaks = c(0,100), add=T)
plot(kmk_areas2['LAND_YEAR'], pal = c('deepskyblue2','gold2'),add=T)
legend(-97.5, 26.5, 
       c('0','1', '2'), fill = c('gray90','deepskyblue2','gold2'),
       title = '# of years fished 2022-23',
       xpd = F, horiz = T)
dev.off()


### aggregations
cpue_yr_area <- aggregate(cpue ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                          data = cflp_hl_1,
                          median, na.rm = T)

land_yr_area <- aggregate(tot_kg ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                          data = cflp_hl_1,
                          sum, na.rm = T)

days_yr_area <- aggregate(FISHED ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                          data = cflp_hl_1,
                          median, na.rm = T)

ves_yr_area <- aggregate(VESSEL_ID ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                         data = cflp_hl_1,
                         function(x) length(unique(x)))

trp_yr_area <- aggregate(SCHEDULE_NUMBER ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                         data = cflp_hl_1,
                         function(x) length(unique(x)))
gc()
### 1x1 grid in use exclusively starting 2013

# kmk_shp <- merge(subset(cpue_yr_area,
#                         COMMON_NAME=='MACKERELS, KING AND CERO' &
#                           LAND_YEAR>2012),
#                  sz_shp,
#                  by = c('AREA_FISHED'))

kmk_shp <- merge(subset(cpue_yr_area,
                        COMMON_NAME=='MACKERELS, KING AND CERO' &
                          LAND_YEAR>2012),
                 sz_shp,
                 by = c('AREA_FISHED')) |>
  merge(subset(land_yr_area,
               COMMON_NAME=='MACKERELS, KING AND CERO' &
                 LAND_YEAR>2012),
        by = c('AREA_FISHED', 'LAND_YEAR', 'COMMON_NAME')) |>
  merge(subset(days_yr_area,
               COMMON_NAME=='MACKERELS, KING AND CERO' &
                 LAND_YEAR>2012),
        by = c('AREA_FISHED', 'LAND_YEAR', 'COMMON_NAME')) |>
  merge(subset(ves_yr_area, 
               COMMON_NAME=='MACKERELS, KING AND CERO' &
                 LAND_YEAR>2012),
        by = c('AREA_FISHED', 'LAND_YEAR', 'COMMON_NAME')) |>
  merge(subset(trp_yr_area, 
               COMMON_NAME=='MACKERELS, KING AND CERO' &
                 LAND_YEAR>2012),
        by = c('AREA_FISHED', 'LAND_YEAR', 'COMMON_NAME'))


# kmk_shp2 <- merge(kmk_shp, subset(ves_yr_area, 
#                                   LAND_YEAR>2012),
#                   by = c('AREA_FISHED', 'LAND_YEAR'))
# kmk_shp3 <- merge(kmk_shp2, subset(land_yr_area,
#                                    COMMON_NAME=='MACKERELS, KING AND CERO' &
#                                      LAND_YEAR>2012),
#                   by = c('AREA_FISHED', 'LAND_YEAR'))
# kmk_shp4 <- merge(kmk_shp3, subset(days_yr_area,
#                                    COMMON_NAME=='MACKERELS, KING AND CERO' &
#                                      LAND_YEAR>2012),
#                   by = c('AREA_FISHED', 'LAND_YEAR'))


quantile(kmk_shp$cpue, seq(0,1,.05))
hist(kmk_shp$cpue)
# kmk_shp$cpue[which(kmk_shp$cpue>20)]
kmk_shp$cpue[which(kmk_shp$cpue>20)] <- 20

quantile(kmk_shp$tot_kg, seq(0,1,.05))
hist(kmk_shp$tot_kg)
# kmk_shp$tot_kg[which(kmk_shp$tot_kg>1e5)]
kmk_shp$tot_kg[which(kmk_shp$tot_kg>1e5)] <- 1e5

quantile(kmk_shp$FISHED, seq(0,1,.05))
hist(kmk_shp$FISHED)
# kmk_shp$FISHED[which(kmk_shp$FISHED>40)]
kmk_shp$FISHED[which(kmk_shp$FISHED>40)] <- 40

quantile(kmk_shp$VESSEL_ID, seq(0,1,.05))
hist(kmk_shp$VESSEL_ID)
# kmk_shp$VESSEL_ID[which(kmk_shp$VESSEL_ID>30)]
kmk_shp$VESSEL_ID[which(kmk_shp$VESSEL_ID>30)] <- 30

quantile(kmk_shp$SCHEDULE_NUMBER, seq(0,1,.05))
hist(kmk_shp$SCHEDULE_NUMBER)
# kmk_shp$SCHEDULE_NUMBER[which(kmk_shp$SCHEDULE_NUMBER>100)]
kmk_shp$SCHEDULE_NUMBER[which(kmk_shp$SCHEDULE_NUMBER>250)] <- 250


brks <- seq(0, 20, 2)
pal <- cmocean('amp')(length(brks)-1)
brks2 <- seq(0, 1e5, 1e4)
pal2 <- cmocean('dense')(length(brks2)-1)
brks4 <- seq(0, 40, 4)
pal4 <- cmocean('turbid')(length(brks4)-1)
brks3 <- seq(0, 30, 3)
pal3 <- cmocean('algae')(length(brks3)-1)
brks5 <- seq(0, 250, 25)
pal5 <- cmocean('rain')(length(brks5)-1)


{ ### CPUE by year
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_cpue_yr.png', width = 10, height = 7, units = 'in', res = 300)
  par(mfrow=c(4,3))
  for(i in 2013:2023){
    kmk_tmp <- subset(kmk_shp, LAND_YEAR==i) %>%
      st_as_sf()
    # plot(gom)
    plot(world, xlim = c(-98,-79), ylim = c(24, 31))
    plot(kmk_tmp['cpue'], breaks = brks, pal = pal,
         add = T)
    # plot(kmk, add = T, border = 1)
    mtext(i, line = 2)
  }
  dev.off()
  
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_cpue_yr_cb.png', width = 1.5, height = 7, units = 'in', res = 300)
  par(mar = c(1,1,1,4))
  image(1, seq(min(brks)+1,max(brks)-1,2), 
        t(as.matrix(seq(min(brks)+1,max(brks)-1,2))),
        col = pal, axes = F, xlab = '', ylab = '')
  axis(4,brks,las = 1)
  mtext('KMK handline CPUE',4,2.5)
  dev.off()
}


{ ### Landings by year
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_land_yr.png', width = 10, height = 7, units = 'in', res = 300)
  par(mfrow=c(4,3))
  for(i in 2013:2023){
    kmk_tmp <- subset(kmk_shp, LAND_YEAR==i) %>%
      st_as_sf()
    # plot(gom)
    plot(world, xlim = c(-98,-79), ylim = c(24, 31))
    plot(kmk_tmp['tot_kg'], breaks = brks2, pal = pal2,
         add = T)
    # plot(kmk, add = T, border = 1)
    mtext(i, line = 2)
  }
  dev.off()
  
  brks2.2 <- brks2/1e4
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_land_yr_cb.png', width = 1.5, height = 7, units = 'in', res = 300)
  par(mar = c(1,1,1,4))
  image(1, seq(min(brks2.2)+.5,max(brks2.2)-.5,1), 
        t(as.matrix(seq(min(brks2.2)+.5,max(brks2.2)-.5,1))),
        col = pal2, axes = F, xlab = '', ylab = '')
  axis(4,brks2.2,las = 1)
  mtext('KMK landings (x10,000 kg)',4,2.5)
  dev.off()
}


{ ### hours by year
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_hrs_yr.png', width = 10, height = 7, units = 'in', res = 300)
  par(mfrow=c(4,3))
  for(i in 2013:2023){
    kmk_tmp <- subset(kmk_shp, LAND_YEAR==i) %>%
      st_as_sf()
    # plot(gom)
    plot(world, xlim = c(-98,-79), ylim = c(24, 31))
    plot(kmk_tmp['FISHED'], breaks = brks4, pal = pal4,
         add = T)
    # plot(kmk, add = T, border = 1)
    mtext(i, line = 2)
  }
  dev.off()
  
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_hrs_yr_cb.png', width = 1.5, height = 7, units = 'in', res = 300)
  par(mar = c(1,1,1,4))
  image(1, seq(min(brks4)+1,max(brks4)-1,2), 
        t(as.matrix(seq(min(brks4)+1,max(brks4)-1,2))),
        col = pal4, axes = F, xlab = '', ylab = '')
  axis(4,brks4,las = 1)
  mtext('Hours fished',4,2.5)
  dev.off()
}


{ ### Vessels by year
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_ves_yr.png', width = 10, height = 7, units = 'in', res = 300)
  par(mfrow=c(4,3))
  for(i in 2013:2023){
    kmk_tmp <- subset(kmk_shp, LAND_YEAR==i) %>%
      st_as_sf()
    # plot(gom)
    plot(world, xlim = c(-98,-79), ylim = c(24, 31))
    plot(kmk_tmp['VESSEL_ID'], breaks = brks3, pal = pal3,
         add = T)
    # plot(kmk, add = T, border = 1)
    mtext(i, line = 2)
  }
  dev.off()
  
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_ves_yr_cb.png', width = 1.5, height = 7, units = 'in', res = 300)
  par(mar = c(1,1,1,4))
  image(1, seq(min(brks3)+1,max(brks3)-1,2), 
        t(as.matrix(seq(min(brks3)+1,max(brks3)-1,2))),
        col = pal3, axes = F, xlab = '', ylab = '')
  axis(4,brks3,las = 1)
  mtext('KMK vessels',4,2.5)
  dev.off()
}


{ ### Trips by year
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_trp_yr.png', width = 10, height = 7, units = 'in', res = 300)
  par(mfrow=c(4,3))
  for(i in 2013:2023){
    kmk_tmp <- subset(kmk_shp, LAND_YEAR==i) %>%
      st_as_sf()
    # plot(gom)
    plot(world, xlim = c(-98,-79), ylim = c(24, 31))
    plot(kmk_tmp['SCHEDULE_NUMBER'], breaks = brks5, pal = pal5,
         add = T)
    # plot(kmk, add = T, border = 1)
    mtext(i, line = 2)
  }
  dev.off()
  
  setwd("~/R_projects/misc-noaa-scripts/figs")
  png('kmk_hrs_yr_cb.png', width = 1.5, height = 7, units = 'in', res = 300)
  par(mar = c(1,1,1,4))
  image(1, seq(min(brks5)+1,max(brks5)-1,2), 
        t(as.matrix(seq(min(brks5)+1,max(brks5)-1,2))),
        col = pal5, axes = F, xlab = '', ylab = '')
  axis(4,brks5,las = 1)
  mtext('Trips',4,2.5)
  dev.off()
}



### linear regression
brk_pal <- function(dat , pal = 'balance', n = 40){
  bnd <- min(ceiling(abs(range(dat,na.rm=T))))
  brk <- seq(-bnd, bnd, by = bnd/(n/2))
  # brk = pretty(dat, n=30)
  pal <- cmocean('balance')(length(brk)-1)
  return(list(brk,pal))
}

afs <- unique(kmk_shp$AREA_FISHED)
out <- matrix(NA,length(afs),2) |> as.data.frame()
n <- 1
for(i in afs){
  tmp <- subset(kmk_shp, AREA_FISHED==i)
  res <- lm(cpue ~ LAND_YEAR, data = tmp) |>
    summary()
  out[n,] <- coef(res)[c(2,8)]
  n <- n + 1
}

out <- cbind(out,afs) |> 
  setNames(c('slope','p-val','AREA_FISHED')) |>
  merge(sz_shp,
        by = c('AREA_FISHED')) |> 
  st_as_sf()
length(which(out$`p-val`<.05))/nrow(out)
out$slope_adj <- out$slope
out$slope_adj <- ifelse(out$`p-val`>.05, NA, out$slope)

hist(out$slope)
hist(out$slope_adj)

brks_pal1 <- brk_pal(out$slope)
brks_pal2 <- brk_pal(out$slope_adj)

plot(out['slope'], breaks = brks_pal1[[1]], pal = brks_pal1[[2]])
plot(out['slope_adj'],  breaks = brks_pal2[[1]], pal = brks_pal2[[2]])

### monthly
# cpue_mth_area <- aggregate(cpue ~ LAND_YEAR + LAND_MONTH + AREA_FISHED + COMMON_NAME,
#                            data = subset(cflp_hl_1, LAND_YEAR>2012),
#                            median, na.rm = T)
cpue_mth_area <- aggregate(cpue ~ LAND_MONTH + AREA_FISHED,
                           data = subset(cflp_hl_1, 
                                         COMMON_NAME=='MACKERELS, KING AND CERO' &
                                           LAND_YEAR>2012),
                           median, na.rm = T)

land_mth_area <- aggregate(tot_kg ~ LAND_MONTH + AREA_FISHED,
                           data = subset(cflp_hl_1, 
                                         COMMON_NAME=='MACKERELS, KING AND CERO' &
                                           LAND_YEAR>2012),
                           median, na.rm = T)
gc()


kmk_shp <- merge(cpue_mth_area,
                 sz_shp,
                 by = c('AREA_FISHED')) |>
  merge(land_mth_area,
        by = c('AREA_FISHED', 'LAND_MONTH'))

quantile(kmk_shp$cpue, seq(0,1,.1))
summary(kmk_shp$cpue)
hist(kmk_shp$cpue)
boxplot(kmk_shp$cpue ~ kmk_shp$LAND_MONTH, outline = T)
kmk_shp$cpue[which(kmk_shp$cpue>20)]
kmk_shp$cpue[which(kmk_shp$cpue>20)] <- 20

quantile(kmk_shp$tot_kg, seq(0,1,.1))
summary(kmk_shp$tot_kg)
hist(kmk_shp$tot_kg)
boxplot(kmk_shp$tot_kg ~ kmk_shp$LAND_MONTH, outline = T)
kmk_shp$tot_kg[which(kmk_shp$tot_kg>1000)]
kmk_shp$tot_kg[which(kmk_shp$tot_kg>1000)] <- 1000

brks <- seq(0, 20, 2)
# pal <- cmocean('thermal')(length(brks)-1)
pal <- rev(rocket(length(brks)-1))

brks2 <- seq(0, 1000, 50)
# pal <- cmocean('thermal')(length(brks)-1)
pal2 <- rev(mako(length(brks2)-1))

par(mfrow=c(4,3))
for(i in 1:12){
  kmk_tmp <- subset(kmk_shp, LAND_MONTH==i) %>%
    st_as_sf()
  # plot(gom)
  plot(world, xlim = c(-98,-73), ylim = c(24, 40))
  plot(kmk_tmp['cpue'], breaks = brks, pal = pal,
       add = T)
  mtext(i)
}

par(mfrow=c(4,3))
for(i in 1:12){
  kmk_tmp <- subset(kmk_shp, LAND_MONTH==i) %>%
    st_as_sf()
  # plot(gom)
  # plot(world, xlim = c(-98,-79), ylim = c(24, 31))
  plot(world, xlim = c(-98,-73), ylim = c(24, 40))
  plot(kmk_tmp['tot_kg'], breaks = brks2, pal = pal2,
       add = T)
  mtext(i)
}





kmk <- which(cflp$COMMON_NAME=='MACKERELS, KING AND CERO')

cflp_kmk <- cflp[kmk, ]
kmk_land_agg <- aggregate(TOTAL_WHOLE_POUNDS ~ LAND_YEAR + REGION, 
                          data = cflp_kmk,
                          sum, na.rm = T)

plot(kmk_land_agg$LAND_YEAR, kmk_land_agg$TOTAL_WHOLE_POUNDS, typ = 'n')
with(subset(kmk_land_agg, REGION=='GOM'),
     points(LAND_YEAR, TOTAL_WHOLE_POUNDS, typ = 'o'))
with(subset(kmk_land_agg, REGION=='SATL'),
     points(LAND_YEAR, TOTAL_WHOLE_POUNDS, typ = 'o', col = 2))
abline(h = with(subset(kmk_land_agg, REGION=='GOM'), mean(TOTAL_WHOLE_POUNDS)),
       lty = 5)
abline(h = with(subset(kmk_land_agg, REGION=='SATL'), mean(TOTAL_WHOLE_POUNDS)),
       col = 2, lty = 5)

kmk_land_agg <- aggregate(TOTAL_WHOLE_POUNDS ~ LAND_YEAR + ST_ABRV, 
                          data = cflp_kmk,
                          sum, na.rm = T)
aggregate(TOTAL_WHOLE_POUNDS ~ ST_ABRV, 
          data = cflp_kmk,
          sum, na.rm = T)

states <- sort(unique(kmk_land_agg$ST_ABRV))
plot(kmk_land_agg$LAND_YEAR, kmk_land_agg$TOTAL_WHOLE_POUNDS, typ = 'n')
for(i in 1:length(states)){
  with(subset(kmk_land_agg, ST_ABRV==states[i]),
       points(LAND_YEAR, TOTAL_WHOLE_POUNDS, typ = 'o', col = i, pch = 16, lwd = 2))
}
legend('topleft',states, lty=1, col=1:length(states),bty = 'n', pch = 16, lwd = 2)


kmk_land_agg <- aggregate(TOTAL_WHOLE_POUNDS ~ AREA_FISHED + LAND_YEAR, 
                          data = cflp_kmk,
                          sum, na.rm = T)

### looking for unique trip identifiers; SCHEDULE_NUMBER seems to fit
cflp[cflp$SCHEDULE_NUMBER==1001335, ]
trips_keep <- is.element(cflp$SCHEDULE_NUMBER, cflp$SCHEDULE_NUMBER[kmk])
cflp_sub1 <- cflp[trips_keep, ]


### Walter & McCarthy 2014 (1993-2013SEDAR38-DW-10) used handlines only fished in one area and with one gear
### but each row in the data have gear/effort listed, so maybe I can get by with only fished in one area
### only one area
trip_area_agg <- aggregate(AREA_FISHED ~ SCHEDULE_NUMBER, data = cflp_sub1,
                           function(x) length(unique(x)))
length(which(trip_area_agg$AREA_FISHED==1))/nrow(trip_area_agg)
### only one gear
trip_gear_agg <- aggregate(GEAR ~ SCHEDULE_NUMBER, data = cflp_sub1,
                           function(x) length(unique(x)))
length(which(trip_gear_agg$GEAR==1))/nrow(trip_gear_agg)
### only one gear & one area
trip_agg <- aggregate(cbind(AREA_FISHED, GEAR) ~ SCHEDULE_NUMBER, data = cflp_sub1,
                      function(x) length(unique(x)))
length(which(trip_agg$GEAR==1 & trip_agg$AREA_FISHED==1))/nrow(trip_agg)
### only one gear & one area
trip_agg2 <- aggregate(cbind(FLAG_MULTIAREA, FLAG_MULTIGEAR) ~ SCHEDULE_NUMBER, data = cflp_sub1,
                       function(x) (unique(x)))
length(which(trip_agg2$FLAG_MULTIAREA==0 & trip_agg2$FLAG_MULTIGEAR==0))/nrow(trip_agg2)

length(which(cflp_sub1$FLAG_MULTIAREA==0 & cflp_sub1$FLAG_MULTIGEAR==0))
length(unique(cflp_sub1$SCHEDULE_NUMBER[which(cflp_sub1$FLAG_MULTIAREA==0 & cflp_sub1$FLAG_MULTIGEAR==0)]))

### kmk only
trip_spp_agg <- aggregate(COMMON_NAME ~ SCHEDULE_NUMBER, data = cflp_sub1,
                          function(x) length(unique(x)))
kmk_only <- trip_spp_agg$SCHEDULE_NUMBER[which(trip_spp_agg$COMMON_NAME==1)]

kmk_trips <- cflp[is.element(cflp$SCHEDULE_NUMBER, kmk_only), ]
kmk_trips <- cflp[kmk, ]
table(kmk_trips$REGION)
table(kmk_trips$AREA_FISHED, kmk_trips$REGION)
table(kmk_trips$LAND_YEAR, kmk_trips$REGION)
table(kmk_trips$LAND_YEAR, kmk_trips$ST_ABRV)
table(kmk_trips$LAND_MONTH, kmk_trips$ST_ABRV)
table(kmk_trips$GEAR)
table(kmk_trips$GEAR_FIN_NAME)

barplot(table(kmk_trips$LAND_YEAR))
barplot(table(kmk_trips$LAND_MONTH))


kmk_yr_region <- aggregate(TOTAL_WHOLE_POUNDS ~ LAND_YEAR + REGION, 
                           data = kmk_trips, sum, na.rm = T)

plot(kmk_yr_region$LAND_YEAR, kmk_yr_region$TOTAL_WHOLE_POUNDS, typ = 'n')
regions <- sort(unique(kmk_yr_region$REGION))
for(i in 1:5){
  with(subset(kmk_yr_region, REGION==regions[i]),
       lines(LAND_YEAR, TOTAL_WHOLE_POUNDS, col = i, typ = 'o', lwd = 2))
}
legend('topleft', regions, col = 1:5, lty = 1, lwd = 2)

