
# https://kateto.net/netscix2016.html

library(dplyr)
library(igraph)
library(sf)
library(terra)
library(readxl)

sf_use_s2(FALSE)
# 
# setwd("~/CMP/data/tagging")
# dat <- read_xlsx('King_Mackerel_extraction.xlsx', sheet = 2) |>
#   type.convert()
# 
# dat$RECAPTURE_MONTH <- as.numeric(dat$RECAPTURE_MONTH)
# dat$RECAPTURE_year <- as.numeric(dat$RECAPTURE_year)
# dat$LONGITUDE_2 <- as.numeric(dat$LONGITUDE_2)
# dat$LATITUDE_2 <- as.numeric(dat$LATITUDE_2)
# # dat$RECAPTURE_year <- gsub('\\.', '', dat$RECAPTURE_year) |> as.numeric()
# # dat$LONGITUDE_2 <- gsub('\\.', '', dat$LONGITUDE_2) |> as.numeric()
# dat <- subset(dat, LONGITUDE_1<(-70)) |>
#   subset(LONGITUDE_2 <(-70))
# 
# recaptures <- subset(dat, !is.na(RECAPTURE_year)) |>
#   subset(!is.na(LONGITUDE_2)) |> 
#   type.convert()
# 
# lons <- seq(min(dat$LONGITUDE_1) |> floor(),
#             max(dat$LONGITUDE_1) |> ceiling(),
#             .1)
# lats <- seq(min(dat$LATITUDE_1) |> floor(),
#             max(dat$LATITUDE_1) |> ceiling(),
#             .1)
# 
# release <- data.frame(
#   lon = recaptures$LONGITUDE_1 |> as.numeric(),
#   lat = recaptures$LATITUDE_1 |> as.numeric()
# )
# 
# capture <- data.frame(
#   lon = recaptures$LONGITUDE_2 |> as.numeric(),
#   lat = recaptures$LATITUDE_2 |> as.numeric()
# )
# 
# lon_lat <- expand.grid(lon = lons, lat = lats)
# 
# # Convert to an sf spatial object
# release_sf <- st_as_sf(release, coords = c("lon", "lat"), crs = 4326) |> st_make_valid()
# capture_sf <- st_as_sf(capture, coords = c("lon", "lat"), crs = 4326) |> st_make_valid()
# lon_lat_sf <- st_as_sf(lon_lat, coords = c("lon", "lat"), crs = 4326) |> st_make_valid()
# 
# # # # Project the points to a local UTM zone (e.g., EPSG 32617 for Florida/East Coast US)
# # # points_projected <- st_transform(points_sf, crs = 32617)
# # grid_template <- rast(release_sf, res = 100) 
# # 
# # # Populate the grid template using values from the spatial points
# # final_grid <- rasterize(release_sf, grid_template, field = "value", fun = mean)
# # 
# # # Review your grid parameters
# # print(final_grid)
# # 
# # # Plot the finished spatial grid
# # plot(final_grid)
# 
# 
# # Create a 500-meter resolution vector fishnet grid across the bounding box
# poly_grid <- st_make_grid(lon_lat_sf, cellsize = 1) |> st_as_sf() |> st_make_valid()
# 
# # Assign an unique ID to every single polygon grid square
# poly_grid$grid_id <- 1:nrow(poly_grid)
# 
# # Spatial join to discover which grid cell each coordinate drops into
# all_sf <- rbind(release_sf, capture_sf)
# 
# points_with_grid_id <- st_join(all_sf, poly_grid, left = T)
# points_with_grid_id <- st_join(release_sf, poly_grid, left = F)
# points_with_grid_id <- st_join(poly_grid, release_sf, left = F)
# 
# plot(points_with_grid_id)



#### restart
setwd("~/CMP/data/tagging")
dats <- read_xlsx('King_Mackerel_extraction.xlsx', sheet = 2) |>
  type.convert()

dats$RECAPTURE_MONTH <- as.numeric(dats$RECAPTURE_MONTH)
dats$RECAPTURE_year <- as.numeric(dats$RECAPTURE_year)
dats$LONGITUDE_2 <- as.numeric(dats$LONGITUDE_2)
dats$LATITUDE_2 <- as.numeric(dats$LATITUDE_2)
dats$DAYS_AT_LARGE <- as.numeric(dats$DAYS_AT_LARGE)
# dat$RECAPTURE_year <- gsub('\\.', '', dat$RECAPTURE_year) |> as.numeric()
# dat$LONGITUDE_2 <- gsub('\\.', '', dat$LONGITUDE_2) |> as.numeric()
dats <- subset(dats, LONGITUDE_1<(-70)) |>
  subset(LONGITUDE_2 <(-70))
# dat <- dats
dat <- subset(dats, DAYS_AT_LARGE<90)

recaptures <- subset(dat, !is.na(RECAPTURE_year)) |>
  subset(!is.na(LONGITUDE_2)) |> 
  type.convert()

plot(dats$LONGITUDE_1, dats$LATITUDE_1, asp = 1)
arrows(dats$LONGITUDE_1, dats$LATITUDE_1,
       dats$LONGITUDE_2, dats$LATITUDE_2,
       length = .01)
plot(recaptures$LONGITUDE_1, recaptures$LATITUDE_1, asp = 1)
arrows(recaptures$LONGITUDE_1, recaptures$LATITUDE_1,
       recaptures$LONGITUDE_2, recaptures$LATITUDE_2,
       length = .01)

lons <- seq(min(c(dat$LONGITUDE_1, dat$LONGITUDE_2)) |> floor(),
            max(c(dat$LONGITUDE_1, dat$LONGITUDE_2)) |> ceiling(),
            1)
lats <- seq(min(c(dat$LATITUDE_1, dat$LATITUDE_2)) |> floor(),
            max(c(dat$LATITUDE_1, dat$LATITUDE_2)) |> ceiling(),
            1)
lon_lat <- expand.grid(lon = lons, lat = lats)
lon_lat$x.grid <- cut(lon_lat$lon, lons)
lon_lat$y.grid <- cut(lon_lat$lat, lats)
lon_lat$xy.grid <- paste(lon_lat$x.grid, lon_lat$y.grid)


release <- data.frame(
  id =recaptures$id,
  lon = recaptures$LONGITUDE_1 |> as.numeric(),
  lat = recaptures$LATITUDE_1 |> as.numeric()
)
capture <- data.frame(
  id =recaptures$id,
  lon = recaptures$LONGITUDE_2 |> as.numeric(),
  lat = recaptures$LATITUDE_2 |> as.numeric()
)
release$x.grid <- cut(release$lon, lons)
release$y.grid <- cut(release$lat, lats)
release$xy.grid <- paste(release$x.grid, release$y.grid)
rel_wt <- table(release$xy.grid) |> 
  as.data.frame(stringsAsFactors = FALSE) |>
  setNames(c('xy.grid','n'))
capture$x.grid <- cut(capture$lon, lons)
capture$y.grid <- cut(capture$lat, lats)
capture$xy.grid <- paste(capture$x.grid, capture$y.grid)
cap_wt <- table(capture$xy.grid) |> 
  as.data.frame(stringsAsFactors = FALSE) |>
  setNames(c('xy.grid','n'))

all_grids <- rbind(release, capture)
nodes_id <- all_grids$xy.grid |> unique() |> sort()

nodes <- data.frame(grid = 1:length(nodes_id),xy.grid = nodes_id) |> 
  merge(lon_lat, by = 'xy.grid', all.x = T)
nodes$lon <- nodes$lon - .5
nodes$lat <- nodes$lat - .5

plot(recaptures$LONGITUDE_1, recaptures$LATITUDE_1, asp = 1)
arrows(recaptures$LONGITUDE_1, recaptures$LATITUDE_1,
       recaptures$LONGITUDE_2, recaptures$LATITUDE_2,
       length = .01)
points(nodes$lon, nodes$lat, pch = 21)

vertices <- nodes[,2:4]

### define edges
# rel_cap <- data.frame(
#   lon1 = recaptures$LONGITUDE_1 |> as.numeric(),
#   lat1 = recaptures$LATITUDE_1 |> as.numeric(),
#   lon2 = recaptures$LONGITUDE_2 |> as.numeric(),
#   lat2 = recaptures$LATITUDE_2 |> as.numeric()
# )
# 
# rel_cap$x.grid1 <- cut(rel_cap$lon1, lons)
# rel_cap$y.grid1 <- cut(rel_cap$lat1, lats)
# rel_cap$xy.grid <- paste(rel_cap$x.grid1, rel_cap$y.grid1)
# 
# rel_cap$x.grid2 <- cut(rel_cap$lon2, lons)
# rel_cap$y.grid2 <- cut(rel_cap$lat2, lats)
# rel_cap$xy.grid2 <- paste(rel_cap$x.grid2, rel_cap$y.grid2)
# 
# merge(rel_cap, nodes, by = 'xy.grid') |> select(grid)


# edges <- cbind(merge(release, nodes, by = 'xy.grid', all.x = T) |> select(grid),
#                merge(capture, nodes, by = 'xy.grid', all.x = T) |> select(grid)) |>
#   setNames(c('from','to'))

from_tmp <- merge(release, nodes, by = 'xy.grid', all.x = T) |> select(id,grid)
to_tmp <- merge(capture, nodes, by = 'xy.grid', all.x = T) |> select(id,grid)
edges <- merge(from_tmp, to_tmp, by = 'id') |> select('grid.x','grid.y') |>
  setNames(c('from','to'))
edges_c <- distinct(edges)
edges_wt <- table(edges) |> as.data.frame() #|> 
  # setNames(c(c('from','to', 'weight')))
edges_wt <- subset(edges_wt, Freq>0) |> 
  setNames(c(c('from','to', 'weight')))

# 3. Build the igraph object
g <- graph_from_data_frame(d = edges_wt, vertices = vertices, directed = TRUE)

# 4. Extract coordinates into a 2-column matrix [Lon, Lat]
geo_layout <- as.matrix(nodes[, c("lon", "lat")])

# 5. Plot the graph with disabled rescaling
plot(g, 
     layout = geo_layout, 
     rescale = FALSE, 
     edge.arrow.size=.2,
     # edge.arrow.mode=2,
     vertex.size = 50, 
     vertex.label = NA,
     edge.width = E(g)$weight/10,
     edge.curved=0.2,
     main = "Geographic igraph Network")

plot(dats$LONGITUDE_1, dats$LATITUDE_1, asp = 1)
points(dats$LONGITUDE_2, dats$LATITUDE_2)
arrows(dats$LONGITUDE_1, dats$LATITUDE_1,
       dats$LONGITUDE_2, dats$LATITUDE_2,
       length = .01)
plot(g, 
     layout = geo_layout, 
     rescale = FALSE, 
     edge.arrow.size=.2,
     edge.color='red',
     # edge.arrow.mode=2,
     vertex.size = 50, 
     vertex.label = NA,
     edge.width = E(g)$weight/10,
     edge.curved=0.5,
     add=T)

# https://igraph.discourse.group/t/optimal-community-detection/1438
### descriptives
edge_density(g, loops=F)
reciprocity(g)
dyad_census(g) # Mutual, asymmetric, and nyll node pairs
transitivity(g, type="global")  # net is treated as an undirected network
transitivity(g, type="local")
triad_census(g) # for directed networks 
diameter(g, directed=T)
get_diameter(g, directed=T)
deg <- degree(g, mode="all")
plot(g, vertex.size=deg*10, 
     layout = geo_layout, 
     edge.arrow.size=.2, 
     rescale = FALSE)
deg.dist <- degree_distribution(g, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

hs <- hits_scores(g, weights = NA)

par(mfrow=c(1,2))
plot(g, vertex.size=hs$hub*100, main="Hubs", 
     edge.arrow.size=.2, 
     layout = geo_layout, 
     rescale = FALSE)
plot(g, vertex.size=hs$authority*100, main="Authorities", 
     edge.arrow.size=.2, 
     layout = geo_layout, 
     rescale = FALSE)


mean_distance(g, directed=T)
cliques(g)
sapply(cliques(g), length) # clique sizes
largest_cliques(g) # cliques with max number of nodes

ceb <- cluster_edge_betweenness(g) 
dendPlot(ceb, mode="hclust")
plot(ceb, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE) 

clp <- cluster_label_prop(g)
plot(clp, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)

cfg <- cluster_fast_greedy(as_undirected(g))
plot(cfg, as_undirected(g), 
     edge.arrow.size=.2, 
     layout = geo_layout, 
     rescale = FALSE)

wc <- cluster_walktrap(g)
plot(wc, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)
modularity(g,wc$membership)
wc$modularity


compare(cfg, wc)

g_simp <- simplify(g)
comms <- cluster_fluid_communities(g_simp, 3)


lec <- cluster_leading_eigen(g)
lec$membership
plot(lec, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)


cen <- alpha_centrality(g,weights=NA)
plot(g, 
     vertex.size=cen*5,
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)
