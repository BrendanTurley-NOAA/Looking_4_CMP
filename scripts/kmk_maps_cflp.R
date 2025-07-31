
rm(list=ls())
gc()
library(cmocean)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(scales)
library(sf)
library(terra)
library(viridisLite)

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GSHHS_shp/i")
world <- vect('GSHHS_i_L1.shp')

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/ne_10m_admin_1_states_provinces")
states <- vect('ne_10m_admin_1_states_provinces.shp')

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/king_mackerel")
kmk <- vect('king_mackerel_po.shp')
# setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
# gom <- vect('GOM_2500ft.shp')

z_cutoff <- (-6000)
setwd("C:/Users/brendan.turley/Documents/data/bathy")
bathy <- rast('etopo1.nc')

values(bathy)[which(values(bathy>0))] <- NA
values(bathy)[which(values(bathy)<z_cutoff)] <- z_cutoff
brks <- seq(z_cutoff,0,100)
cols <- mako(length(brks)-1)
# cols <- (cmocean('ice')(length(brks)-1))

# bathy_l <- bathy
# values(bathy_l)[which(values(bathy_l>0))] <- NA
# values(bathy_l) <- abs(values(bathy_l))
# # values(bathy_l)[which(values(bathy_l)>abs(z_cutoff))] <- abs(z_cutoff)
# hist(values(bathy_l))
# values(bathy_l) <- log10(values(bathy_l))
# hist(values(bathy_l))
# brks <- seq(0,5,.1)
# cols <- rev(mako(length(brks)-1))


slope <- terrain(bathy, "slope", unit="radians")
aspect <- terrain(bathy, "aspect", unit="radians")
hill <- shade(slope, aspect, 45, 270)

setwd("~/R_projects/Looking_4_CMP/figs")
png('kmk_map.png', width = 10, height = 9, units = 'in', res = 300)
par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(hill, col=grey(0:100/100), legend=FALSE, xlim = c(-98, -67), ylim = c(23, 42))
# plot(bathy_l, col = alpha(cols, .5), breaks = brks,add = T, legend = F)
plot(bathy, col = alpha(cols, .5), breaks = brks,add = T, legend = F)
contour(bathy, levels = c(-100), add = T, drawlabels = F)
# plot(world, add = T, col = 'gray60')
plot(states, add = T, col = 'gray60')
plot(kmk, border = 'white', lwd = 2, add = T)
dev.off()



setwd("C:/Users/brendan.turley/Downloads")
kmk_am <- rast('Scomberomorus_cavalla.nc')
kmk_nc <- nc_open('Scomberomorus_cavalla.nc')

setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GSHHS_shp/i")
world <- vect('GSHHS_i_L1.shp')

cols2 <- cmocean('rain')(60)
# cols2 <- rocket(60, direction = -1)

setwd("~/R_projects/Looking_4_CMP/figs")
png('kmk_aquamap.png', width = 10, height = 9, units = 'in', res = 300)
# plot(kmk_am, col = cols2)
plot(kmk_am, col = cols2, 
     main = 'KMK probability of occurrence',
     xlim = c(-100, -74.5), ylim = c(17.5, 36))
plot(world, add = T, col = 'gray80')
dev.off()
