rm(list=ls())
gc()
library(cmocean)
library(data.table)
library(fields)
library(lubridate)
library(ncdf4)
library(sf)
library(terra)


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
gom <- vect('GOM_2500ft.shp')
utm <- "+proj=utm +zone=15 units=km"
# gom_utm <- project(gom, utm)
# gom_utm <- gom_utm[which(gom_utm$StatZone>12), ]
# gom_sf <- st_as_sf(gom_utm)

### gom shapfile for buffer subsetting
# gom_sf <- gom[which(gom$DepZone!='Inshore'),] |>
#   project(utm) |>
#   st_as_sf()

gom_sf <- project(gom, utm) |>
  st_as_sf()


setwd("C:/Users/brendan.turley/Documents/data/shapefiles/king_mackerel")
kmk_zones <- vect('king_mackerel_po.shp')


setwd("C:/Users/brendan.turley/Documents/data/bathy")
bathy <- rast('etopo1.nc')

### isolate shelf
bathy[values(bathy$Band1) > (-10)] <- NA
bathy[values(bathy$Band1) < (-120)] <- NA

plot(bathy,
     main = 'Continential Shelf cutout')

### set all values to 1; then make into shapefile
bathy[!is.na(values(bathy$Band1))] <- 1
bathy2 <- as.polygons(bathy)

bathy_c <- crop(bathy2,
                ext(gom)) |>
  st_as_sf() |>
  st_transform(st_crs(gom_sf)) |>
  st_simplify(dTolerance = 0)

plot(bathy_c)

kmk_shp <- st_intersection(bathy_c, gom_sf)
plot(st_geometry(kmk_shp))
