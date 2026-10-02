
library(terra)
library(sf)
library(ggplot2)

setwd("~/data/shapefiles/cflp_statgrid")
sz_shp <- vect('CFLP_StatGrid_2013_v20140210.shp') |>
  st_as_sf()
sz_shp$AREA_FISHED <- sz_shp$SZ_ID

ggplot(sz_shp) +
  geom_sf(fill = "white", color = "grey") +
  geom_sf_text(aes(label = SZ_ID), size = 3, fun.aggregate = st_centroid) +
  theme_minimal()

setwd("~/CMP/data/cflp")
cflp <- readRDS('CFLPblake.rds')
cflp <- subset(cflp, LAND_YEAR>2013 & CATCH_TYPE == 'CATCH')
kmk_lb <- subset(cflp, COMMON_NAME=='MACKERELS, KING AND CERO')
gc()

mixing <- c('2482','2481','2480','2479')
kmk_lb$REGION[which(kmk_lb$AREA_FISHED %in% mixing)] <- 'GOM'

reg_tab <- table(kmk_lb$VESSEL_ID, kmk_lb$REGION)
mig_ves <- which(reg_tab[,1]>0 & reg_tab[,3]>0) |> names()
mig <- subset(kmk_lb, VESSEL_ID %in% mig_ves)
table(mig$VESSEL_ID, mig$GEAR)
table(mig$VESSEL_ID, mig$REGION) |> as.data.frame.matrix() |> View()
table(mig$LAND_MONTH, mig$REGION)

lb_mo_ar <- aggregate(TOTAL_WHOLE_POUNDS ~ LAND_MONTH + AREA_FISHED,
                                 data = mig,
                                 sum, na.rm = T)
kmk_areas <-  merge(lb_mo_ar,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf()

for(i in 1:12){
  plot(subset(kmk_areas, LAND_MONTH==i)['TOTAL_WHOLE_POUNDS'], main=month.abb[i])
}



mig_trps <- aggregate(SCHEDULE_NUMBER ~ LAND_MONTH + AREA_FISHED,
                      data = mig,
                      function(x) length(unique(x))/11)
kmk_areas <-  merge(mig_trps,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf()

### rewrite in ggplot facet
par(mfrow=c(3,4))
for(i in 1:12){
  plot(subset(kmk_areas, LAND_MONTH==i)['SCHEDULE_NUMBER'], main=month.abb[i])
}
ggplot(kmk_areas) +
  geom_sf(aes(fill = SCHEDULE_NUMBER)) + # Maps geometry automatically; colors by SCHEDULE_NUMBER
  facet_wrap(~ factor(LAND_MONTH, levels = 1:12, labels = month.abb), 
             nrow = 3, ncol = 4) +
  theme_minimal()


### which fish more in gulf
gulf_ves <- which(reg_tab[,1]>reg_tab[,3]) |> names()

gulf <- subset(kmk_lb, VESSEL_ID %in% gulf_ves)

aggregate(TOTAL_WHOLE_POUNDS ~ LAND_MONTH + REGION,
                      data = gulf,
                      sum, na.rm = T)

lb_mo_ar <- aggregate(TOTAL_WHOLE_POUNDS ~ LAND_MONTH + AREA_FISHED,
                      data = gulf,
                      sum, na.rm = T)
kmk_areas <-  merge(lb_mo_ar,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf()

for(i in 1:12){
  plot(subset(kmk_areas, LAND_MONTH==i)['TOTAL_WHOLE_POUNDS'], main=month.abb[i])
}

mig_trps <- aggregate(SCHEDULE_NUMBER ~ LAND_MONTH + AREA_FISHED,
                      data = gulf,
                      function(x) length(unique(x))/11)
kmk_areas <-  merge(mig_trps,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf()

for(i in 1:12){
  plot(subset(kmk_areas, LAND_MONTH==i)['SCHEDULE_NUMBER'], main=month.abb[i])
}


### which fish more in SA
sa_ves <- which(reg_tab[,1]<reg_tab[,3]) |> names()

sa <- subset(kmk_lb, VESSEL_ID %in% sa_ves)

aggregate(TOTAL_WHOLE_POUNDS ~ REGION,
          data = sa,
          sum, na.rm = T)
aggregate(TOTAL_WHOLE_POUNDS ~ LAND_MONTH + REGION,
          data = sa,
          sum, na.rm = T)

lb_mo_ar <- aggregate(TOTAL_WHOLE_POUNDS ~ LAND_MONTH + AREA_FISHED,
                      data = sa,
                      sum, na.rm = T)
kmk_areas <-  merge(lb_mo_ar,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf()

for(i in 1:12){
  plot(subset(kmk_areas, LAND_MONTH==i)['TOTAL_WHOLE_POUNDS'], main=month.abb[i])
}

mig_trps <- aggregate(SCHEDULE_NUMBER ~ LAND_MONTH + AREA_FISHED,
                      data = sa,
                      function(x) length(unique(x))/11)
kmk_areas <-  merge(mig_trps,
                    sz_shp,
                    by = c('AREA_FISHED')) %>%
  st_as_sf()

for(i in 1:12){
  plot(subset(kmk_areas, LAND_MONTH==i)['SCHEDULE_NUMBER'], main=month.abb[i])
}

table(gulf$LAND_MONTH, gulf$REGION) |> t() |> barplot(beside=T)
table(sa$LAND_MONTH, sa$REGION) |> t() |> barplot(beside=T)


sa_tot <- aggregate(TOTAL_WHOLE_POUNDS ~ REGION,
          data = sa,
          sum, na.rm = T)

gulf_tot <- aggregate(TOTAL_WHOLE_POUNDS ~ REGION,
          data = gulf,
          sum, na.rm = T)

all_tot <- aggregate(TOTAL_WHOLE_POUNDS ~ REGION,
                      data = kmk_lb,
                      sum, na.rm = T)

totals <- merge(all_tot, gulf_tot, by = 'REGION') |>
  merge(sa_tot, by = 'REGION') |> 
  setNames(c('region','tot_lb','tot_gulf','tot_sa'))

totals[1,3]/totals[1,2] |> setNames('Gulf proportion by Gulf > SA')
totals[2,4]/totals[2,2] |> setNames('SA proportion by SA > Gulf')

totals[1,4]/totals[2,2] |> setNames('SA proportion by Gulf > SA')
totals[2,3]/totals[1,2] |> setNames('Gulf proportion by SA > Gulf')


