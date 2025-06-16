
library(cmocean)
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


setwd("C:/Users/brendan.turley/Documents/CMP/data/cflp")
cflp <- readRDS('CFLPkarnauskas.rds')
cflp <- cflp[which(cflp$LAND_YEAR<2024), ]

### following methods by Walter & McCarthy 2014 (1993-2013SEDAR38-DW-10) 
# CPUE = total kilograms of king mackerel/(number of lines fished*number of hooks per line*total hours fished) 
cflp$tot_kg <- cflp$TOTAL_WHOLE_POUNDS / 2.205
cflp$pue <- (cflp$NUMGEAR * cflp$EFFORT * cflp$FISHED)
cflp$cpue <- cflp$tot_kg / (cflp$NUMGEAR * cflp$EFFORT * cflp$FISHED)
cflp$cpue <- ifelse(is.infinite(cflp$cpue), NA, cflp$cpue)

### pull out handlines only
# table(cflp$GEAR)
gear_keep <- c('H', 'E', 'TR')
cflp_hl <- subset(cflp , is.element(cflp$GEAR, gear_keep)) |>
  subset(FLAG_MULTIGEAR==0 & FLAG_MULTIAREA==0)
gc()
# table(cflp_hl$EFFORT_DESCR)
# table(cflp_hl$EFFORT_DESCR, cflp_hl$EFFORT_UNIT, useNA = 'always')
# table(cflp_hl$LAND_YEAR, cflp_hl$LAND_MONTH, useNA = 'always')
# with(subset(cflp_hl, REGION=='GOM' & COMMON_NAME=='MACKERELS, KING AND CERO'),
#      addmargins(table(LAND_YEAR, LAND_MONTH, useNA = 'always')))

### pull vessels landing the top 80% of KMK
vessel_landings <- with(subset(cflp_hl, REGION=='GOM' & 
                                 COMMON_NAME=='MACKERELS, KING AND CERO'),
                        aggregate(tot_kg ~ VESSEL_ID, FUN = sum, na.rm = T))
vessel_landings_else <- with(subset(cflp_hl, REGION=='GOM'),
                             aggregate(tot_kg ~ VESSEL_ID, FUN = sum, na.rm = T))
ves_land <- merge(vessel_landings, vessel_landings_else, by = c('VESSEL_ID'))
ves_land$prop <- ves_land$tot_kg.x / ves_land$tot_kg.y
quantile(vessel_landings$tot_kg, seq(0,1,.1))
hist(vessel_landings$tot_kg)
quantile(ves_land$prop, seq(0,1,.1))
hist(ves_land$prop)
cutoff <- quantile(vessel_landings$tot_kg, .2)
cutoff2 <- quantile(ves_land$prop, .2)

plot(ves_land$tot_kg.x, ves_land$prop,log='xy')
abline(v = cutoff, h = cutoff2, col = 2, lty = 5)

# kmk_ves <- vessel_landings$VESSEL_ID[which(vessel_landings$tot_kg>cutoff)]
# kmk_ves2 <- ves_land$VESSEL_ID[which(ves_land$tot_kg.x>cutoff)]
# kmk_ves3 <- ves_land$VESSEL_ID[which(ves_land$prop>cutoff2)]
# identical(kmk_ves, kmk_ves2)
# identical(kmk_ves, kmk_ves3)
kmk_ves <- ves_land$VESSEL_ID[which(ves_land$tot_kg.x>cutoff |
                                             ves_land$prop>cutoff2)]
### check if hours fished longer than trip
diff_time <- (cflp_hl$LAND_DATE-cflp_hl$DEPART_DATE)
units(diff_time) <- 'hours'
### hours fished should not be greater than trip time
which(cflp_hl$FISHED>24 & diff_time==0)
cflp_hl[1029,]
### divide by number of gear
which((cflp_hl$FISHED/cflp_hl$NUMGEAR)>24 & diff_time==0)
cflp_hl[1008377,]

identical(which(cflp_hl$FISHED > diff_time),
          which(diff_time>24))
which(cflp_hl$FISHED>24 & cflp_hl$FISHED==0)
which(cflp_hl$FISHED>diff_time & cflp_hl$FISHED>24)


cflp_hl_0 <- cflp_hl[is.element(cflp_hl$VESSEL_ID, kmk_ves), ] |>
  subset(NUMGEAR<quantile(cflp_hl$NUMGEAR, .99, na.rm = T) &
           EFFORT<quantile(cflp_hl$EFFORT, .99, na.rm = T) &
           FISHED<quantile(cflp_hl$FISHED, .99, na.rm = T))

cpue_yr_reg <- aggregate(cpue ~ LAND_YEAR + REGION + COMMON_NAME,
                         data = cflp_hl,
                         median, na.rm = T)
gc()


with(subset(cpue_yr_reg, REGION=='GOM' & COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, cpue, typ = 'n'))
with(subset(cpue_yr_reg, REGION=='GOM' & COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, cpue, typ = 'o', pch = 16))
with(subset(cpue_yr_reg, REGION=='SATL' & COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, cpue, typ = 'o', col = 2, pch = 16))
abline(h = with(subset(cpue_yr_reg, REGION=='GOM' & COMMON_NAME=='MACKERELS, KING AND CERO'),
                mean(cpue)),
       lty = 5)
abline(h = with(subset(cpue_yr_reg, REGION=='SATL' & COMMON_NAME=='MACKERELS, KING AND CERO'),
                mean(cpue)),
       col = 2, lty = 5)
abline(v = 2013, col = 4, lty = 2)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')


cpue_yr_st <- aggregate(cpue ~ LAND_YEAR + ST_ABRV + REGION + COMMON_NAME,
                        data = cflp_hl_0,
                        median, na.rm = T)
gc()

#GOM
gulf <- c('AL', 'FL', 'LA', 'MS', 'TX')
with(subset(cpue_yr_st, is.element(ST_ABRV, gulf) & 
              REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, cpue, typ = 'n'))
for(i in 1:length(gulf)){
  with(subset(cpue_yr_st, ST_ABRV==gulf[i] & 
                REGION=='GOM' &
                COMMON_NAME=='MACKERELS, KING AND CERO'),
       points(LAND_YEAR, cpue, typ = 'o', col = i, pch = 16, lwd = 2))
}
legend('topleft',gulf, lty=1, col=1:length(gulf),bty = 'n', pch = 16, lwd = 2)

#SATL
satl <- c('GA', 'NC', 'NJ', 'NY', 'SC', 'VA')
with(subset(cpue_yr_st, is.element(ST_ABRV, satl) & 
              REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, cpue, typ = 'n'))
for(i in 1:length(satl)){
  with(subset(cpue_yr_st, ST_ABRV==satl[i] & 
                REGION=='SATL' &
                COMMON_NAME=='MACKERELS, KING AND CERO'),
       points(LAND_YEAR, cpue, typ = 'o', col = i, pch = 16, lwd = 2))
}
legend('topleft',satl, lty=1, col=1:length(satl),bty = 'n', pch = 16, lwd = 2)



cpue_yr <- aggregate(cpue ~ LAND_YEAR + REGION + COMMON_NAME,
                        data = cflp_hl_0,
                        median, na.rm = T)

with(subset(cpue_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, cpue, typ = 'o', pch = 16))
grid()
with(subset(cpue_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, cpue, typ = 'o', pch = 16, col = 2))
abline(v = 2013, lty = 5)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(cpue ~ LAND_YEAR, 
          data = subset(cpue_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(cpue ~ LAND_YEAR, 
          data = subset(cpue_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)


### none of these (hours, effort, numgear) are useful
hrs_yr <- aggregate(FISHED ~ LAND_YEAR + REGION + COMMON_NAME,
                     data = cflp_hl_0,
                     sum, na.rm = T) #median value not informative

num_yr <- aggregate(NUMGEAR ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_0,
                    sum, na.rm = T)

eff_yr <- aggregate(EFFORT ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_0,
                    sum, na.rm = T)

ves_yr <- aggregate(VESSEL_ID ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_0,
                    function(x) length(unique(x)))

trps_yr <- aggregate(SCHEDULE_NUMBER ~ LAND_YEAR + REGION + COMMON_NAME,
                    data = cflp_hl_0,
                    function(x) length(unique(x)))


with(subset(hrs_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, FISHED, typ = 'o', pch = 16))
grid()
with(subset(hrs_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, FISHED, typ = 'o', pch = 16, col = 2))
abline(v = 2013, lty = 5)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(FISHED ~ LAND_YEAR,
          data = subset(hrs_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(FISHED ~ LAND_YEAR,
          data = subset(hrs_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)

with(subset(eff_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, EFFORT, typ = 'o', pch = 16, col = 2))
grid()
with(subset(eff_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, EFFORT, typ = 'o', pch = 16))
abline(v = 2013, lty = 5)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(EFFORT ~ LAND_YEAR,
          data = subset(eff_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(EFFORT ~ LAND_YEAR,
          data = subset(eff_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)

with(subset(num_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, NUMGEAR, typ = 'o', pch = 16, col = 2))
grid()
with(subset(num_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, NUMGEAR, typ = 'o', pch = 16))
abline(v = 2013, lty = 5)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(NUMGEAR ~ LAND_YEAR,
          data = subset(num_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(NUMGEAR ~ LAND_YEAR,
          data = subset(num_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)

with(subset(ves_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, VESSEL_ID, typ = 'o', pch = 16))
grid()
with(subset(ves_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, VESSEL_ID, typ = 'o', pch = 16, col = 2))
abline(v = 2013, lty = 5)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(VESSEL_ID ~ LAND_YEAR, 
          data = subset(ves_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(VESSEL_ID ~ LAND_YEAR, 
          data = subset(ves_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)

with(subset(trps_yr, REGION=='SATL' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     plot(LAND_YEAR, SCHEDULE_NUMBER, typ = 'o', pch = 16, col = 2))
grid()
with(subset(trps_yr, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO'),
     points(LAND_YEAR, SCHEDULE_NUMBER, typ = 'o', pch = 16))
abline(v = 2013, lty = 5)
legend('topleft', c('GOM', 'SATL'), col = 1:2, pch = 16, bty = 'n')
abline(lm(SCHEDULE_NUMBER ~ LAND_YEAR, 
          data = subset(trps_yr, REGION=='GOM' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 1)
abline(lm(SCHEDULE_NUMBER ~ LAND_YEAR, 
          data = subset(trps_yr, REGION=='SATL' &
                          COMMON_NAME=='MACKERELS, KING AND CERO')), col = 2)




### spatial footprint of trips over time

cpue_yr_area_region <- aggregate(cpue ~ LAND_YEAR + AREA_FISHED + COMMON_NAME + REGION,
                                 data = cflp_hl_0,
                                 median, na.rm = T)
with(subset(cpue_yr_area_region, REGION=='GOM' &
              COMMON_NAME=='MACKERELS, KING AND CERO' &
              LAND_YEAR>2012),
     which.max(cpue))

boxplot(cpue ~ LAND_YEAR, data = subset(cpue_yr_area_region, REGION=='GOM' &
                                          COMMON_NAME=='MACKERELS, KING AND CERO' &
                                          LAND_YEAR>2012),
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
                          data = cflp_hl_0,
                          median, na.rm = T)

land_yr_area <- aggregate(tot_kg ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                          data = cflp_hl_0,
                          sum, na.rm = T)

days_yr_area <- aggregate(FISHED ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                          data = cflp_hl_0,
                          median, na.rm = T)

ves_yr_area <- aggregate(VESSEL_ID ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                         data = cflp_hl_0,
                         function(x) length(unique(x)))

trp_yr_area <- aggregate(SCHEDULE_NUMBER ~ LAND_YEAR + AREA_FISHED + COMMON_NAME,
                         data = cflp_hl_0,
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


### monthly
# cpue_mth_area <- aggregate(cpue ~ LAND_YEAR + LAND_MONTH + AREA_FISHED + COMMON_NAME,
#                            data = subset(cflp_hl_0, LAND_YEAR>2012),
#                            median, na.rm = T)
cpue_mth_area <- aggregate(cpue ~ LAND_MONTH + AREA_FISHED,
                           data = subset(cflp_hl_0, 
                                         COMMON_NAME=='MACKERELS, KING AND CERO' &
                                           LAND_YEAR>2012),
                           median, na.rm = T)

land_mth_area <- aggregate(tot_kg ~ LAND_MONTH + AREA_FISHED,
                           data = subset(cflp_hl_0, 
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
  plot(world, xlim = c(-98,-79), ylim = c(24, 31))
  plot(kmk_tmp['cpue'], breaks = brks, pal = pal,
       add = T)
  mtext(i)
}

par(mfrow=c(4,3))
for(i in 1:12){
  kmk_tmp <- subset(kmk_shp, LAND_MONTH==i) %>%
    st_as_sf()
  # plot(gom)
  plot(world, xlim = c(-98,-79), ylim = c(24, 31))
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

