### subset platform data for uniform usage across analyses
library(cmocean)
library(NISTunits)
library(lubridate)
library(sf)
library(terra)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
# utm <- "+proj=utm +zone=15 units=km"
# gom_utm <- project(gom, utm)
gom <- gom[which(gom$StatZone>12), ]
gom_sf <- st_as_sf(gom)

setwd("C:/Users/brendan.turley/Documents/data/boem")
### data downloaded from https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx
platforms <- read.csv('PlatStruc2.csv')

platforms$Install.Date <- mdy(platforms$Install.Date)
platforms$Removal.Date <- mdy(platforms$Removal.Date)
platforms$Site.Clear.Date <- mdy(platforms$Site.Clear.Date)

# platforms <- platforms[-which(is.na(platforms$Install.Date)),] |>
#   subset(Water.Depth..feet.<=2500)


### search for duplication
platforms <- platforms[order(platforms$Longitude,platforms$Latitude),]
dups1 <- which(duplicated(platforms$Longitude,platforms$Latitude))
dups2 <- which(duplicated(platforms$Longitude,platforms$Latitude,fromLast = T))
dups_sort <- rep(NA,length(dups1)*2)
dups_sort[seq(1, length(dups_sort), by = 2)] <- dups1
dups_sort[seq(2, length(dups_sort), by = 2)] <- dups2
plat_dups <- platforms[dups_sort,]
# plot(plat_dups$Longitude,plat_dups$Latitude,asp=1)
platforms <- platforms[-dups2,]

platforms$water_depth_m <- NISTftTOmeter(platforms$Water.Depth..feet.)

platforms <- subset(platforms, !is.na(Longitude) | !is.na(Latitude))

plat_sf <- st_as_sf(platforms,
                    coords = c('Longitude', 'Latitude'),
                    crs = 4326)
plat_sf <- st_transform(plat_sf, crs = st_crs(gom_sf)) %>%
  st_join(gom_sf,
          join = st_intersects,
          left = T)
gc()

plat_sf$rm_yr <- year(plat_sf$Removal.Date)

plot(plat_sf['rm_yr'])

plot(st_geometry(plat_sf), pch = 16)
plot(plat_sf['rm_yr'], pal = cmocean('matter')(10), pch = 16, add = T)

table(plat_sf$rm_yr)
barplot(table(plat_sf$rm_yr))

# setwd("~/R_projects/misc-noaa-scripts/figs")
# png('rig_removals.png', width = 7, height = 4, units = 'in', res = 300)
# with(subset(plat_sf, rm_yr>=2000),
#      barplot(table(rm_yr), las = 2, ylab = 'Offshore Rig Removals'))
# dev.off()


plat_2000 <- subset(plat_sf, rm_yr>=2013 | is.na(rm_yr))
table(plat_2000$rm_yr, useNA = 'always')
barplot(table(plat_2000$rm_yr))

plot(st_geometry(plat_2000), pch = 1)
plot(plat_2000['rm_yr'], pal = cmocean('amp')(10), pch = 16, add = T)

plot(plat_2000['rm_yr'], pal = cmocean('amp')(10), pch = 16)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/cflp_statgrid")
sz_shp <- vect('CFLP_StatGrid_2013_v20140210.shp') |>
  st_as_sf() %>%
  st_transform(st_crs(plat_2000))
sz_shp$AREA_FISHED <- sz_shp$SZ_ID
# sz_shp$STZ_ID<3100

# subset(sz_shp, X_coord<) |>
#   plot()

sf_use_s2(FALSE)
test <- st_join(plat_2000, sz_shp)

tot_rigs <- table(test$AREA_FISHED) |>
  data.frame() |>
  setNames(c('AREA_FISHED','tot_rigs'))
rm_rigs <- table(test$AREA_FISHED[which(!is.na(test$rm_yr))]) |>
  data.frame() |>
  setNames(c('AREA_FISHED','rm_rigs'))
ext_rigs <- table(test$AREA_FISHED[which(is.na(test$rm_yr))]) |>
  data.frame() |>
  setNames(c('AREA_FISHED','ext_rigs'))

rigs <- merge(tot_rigs, rm_rigs, by = c('AREA_FISHED'), all = T) |>
  merge(ext_rigs, by = c('AREA_FISHED'), all = T)
rigs[is.na(rigs)] <- 0
rigs$rm_per <- rigs$rm_rigs / rigs$tot_rigs

rig_shp <- merge(rigs, sz_shp, by = c('AREA_FISHED'), all = T) |>
  st_as_sf()
rig_shp <- rig_shp[which(!is.na(rig_shp$tot_rigs)),]

plot(rig_shp)


library(colors3d)
cols <- colors2d(expand.grid(1:4,5:8), 
                 colors = c("salmon", "gold", "gray", "dodgerblue3"),
                 xtrans = "rank", ytrans = "rank")

rig_shp$tot_c <- cut(rig_shp$tot_rigs, 
                     quantile(rig_shp$tot_rigs, seq(0,1,.25), na.rm = T),
                     # 4,
                     include.lowest = T)
rig_shp$rm_c <- cut(rig_shp$rm_per, 
                    quantile(rig_shp$rm_per, seq(0,1,.25), na.rm = T),
                    # 4,
                    include.lowest = T)
rig_shp$cc <- as.numeric(paste0(as.numeric(rig_shp$rm_c),as.numeric(rig_shp$tot_c)+4))

# rig_shp$cc <- as.numeric(paste0(as.numeric(rig_shp$tot_c)+4,as.numeric(rig_shp$rm_c)))


col_c <- expand.grid(1:4,5:8)
# col_c <- expand.grid(5:8, 1:4)
col_c <- col_c[order(col_c$Var1, col_c$Var2),]
col_cc <- sort(as.numeric(paste0(col_c$Var1, col_c$Var2)))
rig_shp$rig_multi <- match(rig_shp$cc, col_cc)
# matrix(col_cc,4,4)

plot(rig_shp['rig_multi'], pal = cols, breaks = seq(.5,16.5,1))

plot(rig_shp['tot_rigs'], pal = cmocean('dense')(10))
plot(rig_shp['rm_rigs'], pal = cmocean('algae')(11))
plot(rig_shp['rm_per'], pal = cmocean('matter')(10))
plot(rig_shp['tot_c'])
plot(rig_shp['rm_c'])
plot(rig_shp['cc'])
plot(rig_shp['rig_multi'])


# tot_c <- cut(rig_shp$tot_rigs, 4, include.lowest = T)
# rm_c <- cut(rig_shp$rm_per, 4, include.lowest = T)


cols <- colors2d(expand.grid(1:4,5:8), 
                 colors = c("salmon", "gold", "gray", "dodgerblue3"),
                 xtrans = "rank", ytrans = "rank")

image(matrix(1:16,4,4),
      col = cols,
      asp = 1)


# cols <- colors2d(col_c, 
#                  colors = c("salmon", "gold", "gray", "dodgerblue3"),
#                  xtrans = "rank", ytrans = "rank")
# 
# image((matrix(1:16,4,4)),col = cols,
#       asp = 1)

brk1 <- seq(0, 500, 50)
hist(rig_shp$tot_rigs)
# plot(rig_shp['tot_rigs'], breaks = brk1, pal = cmocean('dense')(10))

brk2 <- seq(0, 1, .1)
hist(rig_shp$rm_per)
# plot(rig_shp['rm_per'], breaks = brk2, pal = cmocean('matter')(10))

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GSHHS_shp/i")
world <- vect('GSHHS_i_L1.shp')

setwd("~/R_projects/misc-noaa-scripts/figs")
png('kmk_areas_rigs.png', width = 6, height = 6, units = 'in', res = 300)
par(mfrow=c(2,1), mar = c(1,1,1,1))
plot(world, xlim = c(-97.5,-86.5), ylim = c(25.5, 31), main = 'Total Rigs')
plot(rig_shp['tot_rigs'], breaks = brk1, pal = cmocean('dense')(10), add = T)

plot(world, xlim = c(-97.5,-86.5), ylim = c(25.5, 31), main = "Percentage of Rigs Removed (since 2000)")
plot(rig_shp['rm_per'], breaks = brk2, pal = cmocean('matter')(10), add = T)

# plot(world, xlim = c(-97.5,-86.5), ylim = c(25.5, 31))
# plot(rig_shp['rig_multi'], pal = cols, breaks = seq(.5,16.5,1), add = T)

dev.off()


brk1 <- seq(0, 500, 50)
hist(rig_shp$tot_rigs)

plot(rig_shp['tot_rigs'], breaks = brk1, pal = cmocean('dense')(10))

brk2 <- seq(0, 1, .1)
hist(rig_shp$rm_per)

plot(rig_shp['rm_per'], breaks = brk2, pal = cmocean('matter')(10))


setwd("~/R_projects/misc-noaa-scripts/figs")
png('kmk_areas_rigs_cb1.png', width = 1.5, height = 7, units = 'in', res = 300)
par(mar = c(1,1,1,4))
image(1, seq(min(brk1)+.5,max(brk1)-.5,1), 
      t(as.matrix(seq(min(brk1)+.5,max(brk1)-.5,1))),
      col = cmocean('dense')(10), axes = F, xlab = '', ylab = '')
axis(4,brk1,las = 1)
# mtext('Oil Rigs',4,2.5)
dev.off()

brk2.1 <- brk2 * 100
setwd("~/R_projects/misc-noaa-scripts/figs")
png('kmk_areas_rigs_cb2.png', width = 1.5, height = 7, units = 'in', res = 300)
par(mar = c(1,1,1,4))
image(1, seq(min(brk2.1)+.5,max(brk2.1)-.5,1), 
      t(as.matrix(seq(min(brk2.1)+.5,max(brk2.1)-.5,1))),
      col = cmocean('matter')(10), axes = F, xlab = '', ylab = '')
axis(4,brk2.1,paste0(brk2.1,'%'),las = 1)
# mtext('Percentage of Rigs Removed',4,2.5)
dev.off()


### number of rigs per year
platforms <- subset(platforms, water_depth_m<=100)

rigs <- data.frame(year = 1947:2023)

# rigs_in <- cumsum(table(year(platforms$Install.Date)))|>
rigs_in <- table(year(platforms$Install.Date)) |>
  as.data.frame() |>
  setNames(c('year','install'))
# rigs_in$year <- as.numeric(rownames((rigs_in)))

# rigs_rm <- cumsum(table(year(platforms$Removal.Date)))|>
rigs_rm <- table(year(platforms$Removal.Date))|>
  as.data.frame() |>
  setNames(c('year','removal'))
# rigs_rm$year <- as.numeric(rownames((rigs_rm)))

rigs_all <- merge(rigs, rigs_in, by = c('year'), all = T) |>
  merge(rigs_rm, by = c('year'), all = T)

rigs_all$sum <- ifelse(is.na(rigs_all$removal),
                       rigs_all$install,
                       rigs_all$install - rigs_all$removal)
rigs_all$ext <- cumsum(rigs_all$sum)


rigs_all2 <- matrix(NA,length(rigs$year),3)
for(i in rigs$year){
  # i=1990
  cur1 <- which(year(platforms$Install.Date)<=i & 
          year(platforms$Removal.Date)>i)
  cur2 <- which(year(platforms$Install.Date)<=i & 
          is.na(year(platforms$Removal.Date)))
  cur <- union(cur1, cur2)
  
  rm <- which(year(platforms$Install.Date)<=i & 
                year(platforms$Removal.Date)==i)
  
  rigs_all2[i-1946, ] <- cbind(i, length(cur), length(rm))
  
}
rigs_all2 <- as.data.frame(rigs_all2) |>
  setNames(c('year','current','removed'))

subset(platforms, is.na(Removal.Date)) |>
  nrow()

subset(platforms, is.na(Install.Date)) |>
  nrow()

rigs_all[77,]
rigs_all2[77,]

plot(rigs_all2$year, rigs_all2$current, typ = 'h')
barplot(rigs_all2$current)
with(subset(rigs_all2, year>=2000),
     barplot(current, names.arg = 2000:2023,las = 2, ylab = 'Total Offshore Rigs'))

setwd("~/R_projects/misc-noaa-scripts/figs")
png('rig_removals2.png', width = 7, height = 4, units = 'in', res = 300)
with(subset(rigs_all2, year>=2000),
     barplot(current, names.arg = 2000:2023,las = 2, ylab = 'Total Offshore Rigs',
             ylim=c(0,4000)))
# axis(2, seq(0,3500,500),las=2)
dev.off()

1 - (rigs_all2$current[which(rigs_all2$year==2023)] / rigs_all2$current[which(rigs_all2$year==2000)])

