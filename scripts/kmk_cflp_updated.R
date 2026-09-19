
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

#### read data and subset ####--------------------------------------------------
gom_st <- c('FL', 'AL', 'MS', 'LA', 'TX') |> sort()

setwd("C:/Users/brendan.turley/Documents/CMP/data/cflp")
cflp <- readRDS('CFLPblake.rds')
cflp <- subset(cflp, LAND_YEAR>1998 & CATCH_TYPE == 'CATCH') |>
  subset(REGION == 'GOM' & is.element(ST_ABRV, gom_st)) |>
  subset(AREA_FISHED!='1' & AREA_FISHED!='2' & !is.na(AREA_FISHED))
gc()

### pull out handlines only
# table(cflp$GEAR)
gear_keep <- c('H', 'E', 'TR')
# gear_keep <- c('TR')
cflp_hl <- subset(cflp , is.element(cflp$GEAR, gear_keep)) |>
  subset(FLAG_MULTIGEAR==0 & FLAG_MULTIAREA==0)

#### CPUE calculation and days away correction ####-----------------------------
## following methods by Walter & McCarthy 2014 (1993-2013SEDAR38-DW-10)
## CPUE = total kilograms of king mackerel/(number of lines fished*number of hooks per line*total hours fished)
## total whole pounds seems most appropriate; other 2 have lots of zeros
cflp_hl$tot_kg <- cflp_hl$TOTAL_WHOLE_POUNDS / 2.205
cflp_hl$pue <- (cflp_hl$NUMGEAR * cflp_hl$EFFORT * cflp_hl$FISHED)
cflp_hl$cpue <- cflp_hl$tot_kg / cflp_hl$pue # kg catch per number of hooks X hours
cflp_hl$cpue <- ifelse(is.infinite(cflp_hl$cpue), NA, cflp_hl$cpue)

### correct days_away
diff_time <- (cflp_hl$LAND_DATE - cflp_hl$DEPART_DATE)
units(diff_time) <- 'days'
cflp_hl$days_away_corrected <- round(diff_time + 1)
rm(diff_time)
gc()
### end


#### pull vessels landing the top 80% of KMK ####-------------------------------
### this sorts by region and state
### Walter sorts by year and then total landings to get at the 80%
vessel_yrs <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO',
                          select = c(LAND_YEAR, VESSEL_ID, REGION, ST_ABRV)),
                   aggregate(LAND_YEAR ~ VESSEL_ID + REGION + ST_ABRV,
                             FUN = function(x) length(unique(x))))
vessel_tot <- with(subset(cflp_hl,
                          select = c(tot_kg, VESSEL_ID, REGION, ST_ABRV)),
                   aggregate(tot_kg ~ VESSEL_ID + REGION + ST_ABRV, FUN = sum, na.rm = T))
vessel_kmk_tot <- with(subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO',
                              select = c(tot_kg, VESSEL_ID, REGION, ST_ABRV)),
                       aggregate(tot_kg ~ VESSEL_ID + REGION + ST_ABRV, FUN = sum, na.rm = T)) |>
  setNames(c("VESSEL_ID","REGION",'ST_ABRV',"kmk_tot_kg"))
vessel_select <- merge(vessel_yrs, vessel_tot,
                       by = c('VESSEL_ID', 'REGION','ST_ABRV')) |>
  merge(vessel_kmk_tot, by = c('VESSEL_ID','REGION','ST_ABRV'))
vessel_select$kmk_pro <- vessel_select$kmk_tot_kg / vessel_select$tot_kg
vessel_select <- vessel_select[order(vessel_select$LAND_YEAR,
                                     vessel_select$kmk_tot_kg,
                                     vessel_select$kmk_pro,
                                     decreasing = T), ]
n <- 1
ves_id <- list()
for(i in c('GOM','SATL')){
  ti <- subset(vessel_select, REGION==i)
  if(i=='GOM') st_sl <- gom_st
  # if(i=='SATL') st_sl <- satl_st
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
      FISHED < quantile(cflp_hl$FISHED, .995, na.rm = T) &
      tot_kg < quantile(cflp_hl$tot_kg, .995, na.rm = T) &
      days_away_corrected < quantile(cflp_hl$days_away_corrected, .995, na.rm = T)
  )
cflp_hl_1 <- cflp_hl_0



#### spatial footprint of trips over time ####----------------------------------

cpue_yr_area_region <- aggregate(cpue ~ LAND_YEAR + AREA_FISHED,
                                 data = subset(cflp_hl_1,
                                               COMMON_NAME=='MACKERELS, KING AND CERO'),
                                 median, na.rm = T)

b <- boxplot(cpue ~ LAND_YEAR, data = subset(cflp_hl_1,
                                                      COMMON_NAME=='MACKERELS, KING AND CERO'),
             pch = 16, lty = 1, varwidth = F, staplewex = 0, lwd = 2, outline = F)


setwd("~/R_projects/Looking_4_CMP/figs")
png('kmk_cpue_area3.png', width = 8, height = 5, units = 'in', res = 300)
par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(aggregate(AREA_FISHED ~ LAND_YEAR, 
               data = subset(cpue_yr_area_region,
                               LAND_YEAR>2012),
               length),
     typ = 'o',ylab = 'Number of areas fished', pch = 16)
grid()
dev.off()

kmk_area <- aggregate(AREA_FISHED ~ LAND_YEAR, 
                      data = subset(cpue_yr_area_region,
                                      LAND_YEAR>2012), length)
1 - median(kmk_area$AREA_FISHED[9:12]) / median(kmk_area$AREA_FISHED[1:4])

area_cnt <- aggregate(LAND_YEAR ~ AREA_FISHED, 
                      data = subset(cpue_yr_area_region,
                                      LAND_YEAR>2012),
                      length)
area_cnt2 <- aggregate(LAND_YEAR ~ AREA_FISHED, 
                       data = subset(cpue_yr_area_region,
                                       LAND_YEAR>2021),
                       length)
kmk_areas <-  merge(area_cnt,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf
plot(kmk_areas['LAND_YEAR'])
kmk_areas2 <-  merge(area_cnt2, 
                     sz_shp,
                     by = c('AREA_FISHED')) %>%
  st_as_sf

plot(kmk_areas2['LAND_YEAR'])


setwd("~/R_projects/Looking_4_CMP/figs")
png('kmk_areas_map2.png', width = 7, height = 4, units = 'in', res = 300)
plot(world, xlim = c(-97.5,-80), ylim = c(24, 31))
plot(kmk_areas['LAND_YEAR'], pal = 'gray90', breaks = c(0,100), add=T)
plot(kmk_areas2['LAND_YEAR'], pal = cmocean('algae')(4),add=T)
legend( -97.5, 26.5, 
       c('0','1', '2','3'), fill = c('gray90',cmocean('algae')(4)),
       title = '# of years fished 2022-24',
       xpd = F, horiz = T)
dev.off()
