
library(readxl)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)

### spatial domain
min_lon <- -100
max_lon <- -80
min_lat <- 16
max_lat <- 31

### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))

### load data
setwd("~/CMP/data/tagging")
dats <- read_xlsx('King_Mackerel_extraction.xlsx', sheet = 2) |>
  type.convert()
dats$LONGITUDE_2 <- as.numeric(dats$LONGITUDE_2)
dats$LATITUDE_2 <- as.numeric(dats$LATITUDE_2)


table(dats$RELEASE_YEAR,
      dats$RECAPTURE_year)

lon_max <- -86
lat_max <- 25.97

mex_rel <- subset(dats, LONGITUDE_1 < lon_max & LATITUDE_1 < lat_max)

table(mex_rel$RELEASE_YEAR,
      mex_rel$RECAPTURE_year)

plot(world, col = 'lightgrey', border = 'darkgrey')
points(mex_rel$LONGITUDE_1, mex_rel$LATITUDE_1, pch = 20, col = 'blue')
arrows(mex_rel$LONGITUDE_1, mex_rel$LATITUDE_1,
       mex_rel$LONGITUDE_2, mex_rel$LATITUDE_2,
       length = 0.1, col = 'red', lwd = 0.5)


setwd("C:/Users/brendan.turley/Downloads")
returns <- read.csv('kgm_mex_returns.csv')
table(returns$Species)

`%notin%` <- Negate(`%in%`)

which(dats$TAG_ID_1 %in% returns$Tag_Number)
returns$Species[which(returns$Tag_Number %in% dats$TAG_ID_1)] |> table()
returns$Species[which(returns$Tag_Number %notin% dats$TAG_ID_1)] |> table()
which(returns$Tag_Number %in% dats$TAG_ID_1)

