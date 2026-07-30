### to do
# 1 create larger groups for an adjacency matrix for use in migration code
# 1a groups are Mexico, Texas, eastern Gulf, South Atlantic
# 1b How to classify FL Keys and LA?
# 2 run code and create sensitivity analyses to test assumptions
# 3 plot density maps of release and recapture locations
# 4 plot out lengths for release and recapture across years and locations


# https://kateto.net/netscix2016.html

library(data.table)
library(lubridate)
library(dplyr)
library(igraph)
library(sf)
library(terra)
library(readxl)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(geosphere)
library(sf)
library(mregions2)

sf_use_s2(FALSE)


### load shapefile
kgm_shp <- st_read("~/data/shapefiles/king_mackerel/king_mackerel_po.shp") |>
  st_transform(crs = 4326)

### Mexico shapefile
gaz_search('Mexico') |> View()
mex_gulf <- gaz_geometry(25280, format = "sfc") |> st_as_sf()
mex_gulf <- st_cast(mex_gulf, "POLYGON")
st_geometry(mex_gulf) <- 'geometry'
mex_gulf$Group <- 'Gulf'
mex_gulf$Zone <- 'Mexico'
mex_gulf <- mex_gulf[,c('Group','Zone','geometry')]

# Combine while ignoring attribute mismatches
combined_sf = rbindlist(list(kgm_shp, mex_gulf),
                        fill = TRUE, use.names = TRUE, ignore.attr = TRUE) |> 
  st_as_sf()


### load data
setwd("~/CMP/data/tagging")
dats <- read_xlsx('King_Mackerel_extraction.xlsx', sheet = 2) |>
  type.convert()
names(dats) <- tolower(names(dats))

dats$tag_date_2 <- as.Date(as.numeric(dats$tag_date_2), origin = '1899-12-31')
dats$recapture_month <- as.numeric(dats$recapture_month)
dats$recapture_year <- as.numeric(dats$recapture_year)
dats$longitude_2 <- as.numeric(dats$longitude_2)
dats$latitude_2 <- as.numeric(dats$latitude_2)
dats$days_at_large <- as.numeric(dats$days_at_large)
dat <- subset(dats, longitude_1<(-70)) |>
  subset(longitude_2 <(-70))


### subset for recaptures
recaptures <- subset(dat, !is.na(recapture_year)) |>
  subset(!is.na(longitude_2)) |> 
  type.convert()

### define nodes in grid
release <- data.frame(
  id = recaptures$id,
  lon = recaptures$longitude_1 |> as.numeric(),
  lat = recaptures$latitude_1 |> as.numeric()
)
capture <- data.frame(
  id = recaptures$id,
  lon = recaptures$longitude_2 |> as.numeric(),
  lat = recaptures$latitude_2 |> as.numeric()
)

### convert to sf
# release_sf <- st_as_sf(release, coords = c("lon", "lat"), crs = 4326) |>
#   st_join(kgm_shp, join = st_within)  |>
#   mutate(group_zone = paste(Group, Zone, sep = "_"))
# capture_sf <- st_as_sf(capture, coords = c("lon", "lat"), crs = 4326) |>
#   st_join(kgm_shp, join = st_within) |>
#   mutate(group_zone = paste(Group, Zone, sep = "_"))
# 
# table(release_sf$group_zone)
# table(capture_sf$group_zone)


release_sf <- st_as_sf(release, coords = c("lon", "lat"), crs = 4326) |>
  st_join(combined_sf, join = st_within)  |>
  mutate(group_zone = paste(Group, Zone, sep = "_"))
capture_sf <- st_as_sf(capture, coords = c("lon", "lat"), crs = 4326) |>
  st_join(combined_sf, join = st_within) |>
  mutate(group_zone = paste(Group, Zone, sep = "_"))

### find ssignments for points that are not within any polygon
ind1 <- which(release_sf$group_zone=='NA_NA')
# Find the index of the nearest polygon for each point
nearest_idx1 <- st_nearest_feature(release_sf[ind1,], combined_sf)
# Assign polygon attributes to the points
points_assigned1 <- combined_sf[nearest_idx1, ]
release_sf[ind1, 2:7] <- points_assigned1
release_sf <- release_sf |> mutate(group_zone = paste(Group, Zone, sep = "_"))

ind2 <- which(capture_sf$group_zone=='NA_NA')
# Find the index of the nearest polygon for each point
nearest_idx2 <- st_nearest_feature(capture_sf[ind2,], combined_sf)
# Assign polygon attributes to the points
points_assigned2 <- combined_sf[nearest_idx2, ]
capture_sf[ind2, 2:7] <- points_assigned2
capture_sf <- capture_sf |> mutate(group_zone = paste(Group, Zone, sep = "_"))


table(release_sf$group_zone)
table(capture_sf$group_zone)


from_df <- release_sf |> dplyr::select(id, group_zone) |>
  st_drop_geometry() |> setNames(c('id','from'))
to_df <- capture_sf |> dplyr::select(id, group_zone) |>
  st_drop_geometry() |> setNames(c('id','to'))
from_to <- merge(from_df, to_df, by = 'id')

from_to |> dplyr::select(from, to) |> table() |> addmargins() |> as.data.frame.matrix()
