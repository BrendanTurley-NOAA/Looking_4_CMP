
# https://kateto.net/netscix2016.html

library(dplyr)
library(igraph)
library(sf)
library(terra)
library(readxl)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(geosphere)


sf_use_s2(FALSE)

### load data
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
dat <- subset(dats, LONGITUDE_1<(-70)) |>
  subset(LONGITUDE_2 <(-70))

# distance_km <- distm(dat[,c('LONGITUDE_1','LATITUDE_1')], 
#                          dat[,c('LONGITUDE_2','LATITUDE_2')],
#                          fun = distHaversine) |> diag()/1000


recaptures <- subset(dat, !is.na(RECAPTURE_year)) |>
  subset(!is.na(LONGITUDE_2)) |> 
  type.convert()

### subset data for TAL
recap_sub <- subset(recaptures, DAYS_AT_LARGE<180)


win <- subset(recap_sub, month(TAG_DATE_1)==12 | month(TAG_DATE_1)<3)
spr <- subset(recap_sub, month(TAG_DATE_1)>2 & month(TAG_DATE_1)<6)
sum <- subset(recap_sub, month(TAG_DATE_1)>5 & month(TAG_DATE_1)<9)
aut <- subset(recap_sub, month(TAG_DATE_1)>8 | month(TAG_DATE_1)<12)

par(mfrow=c(2,2))
plot(recap_sub$LONGITUDE_1, recap_sub$LATITUDE_1, typ = 'n')
arrows(win$LONGITUDE_1, win$LATITUDE_1,
       win$LONGITUDE_2, win$LATITUDE_2,
       length = .05)

plot(recap_sub$LONGITUDE_1, recap_sub$LATITUDE_1, typ = 'n')
arrows(spr$LONGITUDE_1, spr$LATITUDE_1,
       spr$LONGITUDE_2, spr$LATITUDE_2,
       length = .05)

plot(recap_sub$LONGITUDE_1, recap_sub$LATITUDE_1, typ = 'n')
arrows(sum$LONGITUDE_1, sum$LATITUDE_1,
       sum$LONGITUDE_2, sum$LATITUDE_2,
       length = .05)

plot(recap_sub$LONGITUDE_1, recap_sub$LATITUDE_1, typ = 'n')
arrows(aut$LONGITUDE_1, aut$LATITUDE_1,
       aut$LONGITUDE_2, aut$LATITUDE_2,
       length = .05)


### find all nodes

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
lon_lat$lon <- lon_lat$lon - .5
lon_lat$lat <- lon_lat$lat - .5

### spatial domain
min_lon <- min(lons)
max_lon <- max(lons)
min_lat <- min(lats)
max_lat <- max(lats)

### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))


release <- data.frame(
  id = recaptures$id,
  lon = recaptures$LONGITUDE_1 |> as.numeric(),
  lat = recaptures$LATITUDE_1 |> as.numeric()
)
capture <- data.frame(
  id = recaptures$id,
  lon = recaptures$LONGITUDE_2 |> as.numeric(),
  lat = recaptures$LATITUDE_2 |> as.numeric()
)
release$x.grid <- cut(release$lon, lons)
release$y.grid <- cut(release$lat, lats)
release$xy.grid <- paste(release$x.grid, release$y.grid)

capture$x.grid <- cut(capture$lon, lons)
capture$y.grid <- cut(capture$lat, lats)
capture$xy.grid <- paste(capture$x.grid, capture$y.grid)

all_grids <- rbind(release, capture)
nodes_id <- all_grids$xy.grid |> unique() |> sort()
nodes <- data.frame(grid = 1:length(nodes_id), xy.grid = nodes_id) |> 
  merge(lon_lat, by = 'xy.grid', all.x = T)


### find land and remove nodes
# plot(world)
# points(nodes$lon, nodes$lat, pch = 21, col = 2, cex = 2.5)
# text(nodes$lon, nodes$lat, nodes$grid)
land <- c(32,57,78,85,92)
land_nodes <- nodes[is.element(nodes$grid, land),]
not_land_grids <- nodes$grid[!is.element(nodes$grid, land)]
nodes <- nodes[!is.element(nodes$grid, land), ]

### define vertices
vertices <- nodes[,2:4]


### subset data for TAL
recap_sub <- subset(recaptures, DAYS_AT_LARGE<90)
# recap_sub <- subset(recaptures, DAYS_AT_LARGE>365)

rel_sub <- subset(release, id %in% recap_sub$id)
cap_sub <- subset(capture, id %in% recap_sub$id)

rel_wt <- table(rel_sub$xy.grid) |> 
  as.data.frame(stringsAsFactors = FALSE) |>
  setNames(c('xy.grid','n'))
cap_wt <- table(cap_sub$xy.grid) |> 
  as.data.frame(stringsAsFactors = FALSE) |>
  setNames(c('xy.grid','n'))


### subset nodes
all_grid_sub <- rbind(rel_sub, cap_sub)
nodes_sub_id <- all_grid_sub$xy.grid |> unique() |> sort()

nodes_sub <- nodes[which(nodes$xy.grid %in% nodes_sub_id), ]
vert_sub <- nodes_sub[,2:4]


### define edges
from_tmp <- merge(rel_sub, nodes_sub, by = 'xy.grid', all.x = T) |> dplyr::select(id, grid)
to_tmp <- merge(cap_sub, nodes_sub, by = 'xy.grid', all.x = T) |> dplyr::select(id, grid)
edges <- merge(from_tmp, to_tmp, by = 'id') |> dplyr::select('grid.x','grid.y') |>
  setNames(c('from','to'))
# edges_c <- distinct(edges)
edges_wt <- table(edges) |> as.data.frame()
edges_wt <- subset(edges_wt, Freq>0) |> 
  setNames(c(c('from','to', 'weight')))
# edges_wt <- subset(edges_wt, weight>1)
edges_wt <- edges_wt[which(edges_wt$to %in% not_land_grids), ]
edges_wt <- edges_wt[which(edges_wt$from %in% not_land), ]


### define weights as inverse distance
from_tmp <- merge(rel_sub, nodes_sub, by = 'xy.grid', all.x = T) |> dplyr::select(id, grid, lon.y, lat.y)
to_tmp <- merge(cap_sub, nodes_sub, by = 'xy.grid', all.x = T) |> dplyr::select(id, grid, lon.y, lat.y)
edges <- merge(from_tmp, to_tmp, by = 'id') |> 
  dplyr::select('grid.x', 'lon.y.x', 'lat.y.x', 'grid.y', 'lon.y.y', 'lat.y.y') |>
  setNames(c('from','from.x','from.y','to','to.x','to.y'))
distance_km <- distm(edges[,c('from.x','from.y')],
                     edges[,c('to.x','to.y')],
                         fun = distHaversine) |> diag()/1000
distance_km[which(distance_km==0)] <- 1
inv_wt <- data.frame(inv_dis = 1/distance_km,
                     ft_i = paste0(edges$from,edges$to))
inv_distance <- data

edges_wt <- edges |> dplyr::select(from, to) |> table() |> as.data.frame() |>
  subset(Freq>0)
edges_wt$ft_i <- paste0(edges_wt$from,edges_wt$to)
edges_wt2 <- merge(edges_wt, inv_wt, by = 'ft_i') |> 
  distinct() |> dplyr::select(from, to, inv_dis) |> 
  setNames(c(c('from','to', 'weight')))

### make graph and plot
# 3. Build the igraph object
g <- graph_from_data_frame(d = edges_wt2, vertices = vert_sub, directed = TRUE) |>
  simplify()

# 4. Extract coordinates into a 2-column matrix [Lon, Lat]
geo_layout <- as.matrix(nodes_sub[, c("lon", "lat")])

# 5. Plot the graph with disabled rescaling
### shapefile for plotting

plot(world, col = 'gray')
plot(g, 
     layout = geo_layout, 
     rescale = FALSE, 
     edge.arrow.size = .2,
     edge.arrow.width = 2,
     edge.color = 'dodgerblue4',
     vertex.size = 50, 
     # vertex.label = NA,
     edge.width = E(g)$weight*10,
     edge.curved = 0.2,
     main = "Geographic igraph Network", 
     add = T)

plot(g,
     edge.arrow.size=.3,
     # edge.arrow.mode=2,
     edge.color = 'dodgerblue3',
     edge.width = E(g)$weight |> sqrt(),
     loop.size = 2)


### assign membership
ceb <- cluster_edge_betweenness(g, weights = E(g)$weight) 
modularity(g, ceb$membership)
plot_dendrogram(ceb, mode="hclust")
 
clp <- cluster_label_prop(g, weights = E(g)$weight)
modularity(g, ceb$membership)

cfg <- cluster_fast_greedy(as_undirected(g))
modularity(g, cfg$membership)

cwt <- cluster_walktrap(g, weights = E(g)$weight, steps = 2)
modularity(g, cwt$membership)

cle <- cluster_leading_eigen(as_undirected(g))
modularity(g, cle$membership)

imc <- cluster_infomap(g)
modularity(g, imc$membership)

ldc <- cluster_leiden(as_undirected(g), objective_function = 'modularity', resolution = .1, n_iterations = 5)
modularity(g, ldc$membership)

clu <- cluster_louvain(as_undirected(g), resolution = 1)
modularity(g, clu$membership)

cop <- cluster_optimal(g)
modularity(g, cop$membership)

igraph::compare(ceb, clp, method = 'nmi')
igraph::compare(cfg, clp, method = 'nmi')
igraph::compare(cwt, clp, method = 'nmi')
igraph::compare(cwt, cle, method = 'nmi')



par(mfrow=c(3,3))
plot(world)
plot(ceb, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, ceb$membership) |> round(digits=2))

plot(world)
plot(clp, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, clp$membership) |> round(digits=2))

plot(world)
plot(cfg, as_undirected(g), 
     edge.arrow.size=.2, 
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, cfg$membership) |> round(digits=2))

plot(world)
plot(cwt, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, cwt$membership) |> round(digits=2))

plot(world)
plot(cle, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, cle$membership) |> round(digits=2))

plot(world)
plot(imc, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, imc$membership) |> round(digits=2))

plot(world)
plot(ldc, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, ldc$membership) |> round(digits=2))

plot(world)
plot(clu, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, clu$membership) |> round(digits=2))

plot(world)
plot(cop, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE,
     add = T)
mtext(modularity(g, cop$membership) |> round(digits=2))



#### testing

### length conversions
dats$LENGTH_DETERMINATION_1 |> table()
dats$LENGTH_DETERMINATION_2 |> table()

dats$LENGTH_UNIT_1 |> table()
dats$LENGTH_UNIT_2 |> table()

dats$LENGTH_TYPE_1 |> table()
dats$LENGTH_TYPE_2 |> table()

### convert to mm
dats$LENGTH_1 <- dats$LENGTH_1 |> as.numeric()
dats$lth_mm_1 <- ifelse(dats$LENGTH_UNIT_1 == 'CM', dats$LENGTH_1 * 10,
                        ifelse(dats$LENGTH_UNIT_1 == 'IN', dats$LENGTH_1 * 25.4,
                               dats$LENGTH_1))
dats$LENGTH_2 <- dats$LENGTH_2 |> as.numeric()
dats$lth_mm_2 <- ifelse(dats$LENGTH_UNIT_2 == 'CM', dats$LENGTH_2 * 10,
                        ifelse(dats$LENGTH_UNIT_2 == 'IN', dats$LENGTH_2 * 25.4,
                               ifelse(dats$LENGTH_UNIT_2 == 'FT', dats$LENGTH_2 * 304.8,
                                      dats$LENGTH_2)))

### convert to FL
dats$lth_mm_1 <- ifelse(dats$LENGTH_TYPE_1 == 'TLE',
                        -4.28+0.963*dats$lth_mm_1, dats$lth_mm_1)
dats$lth_mm_2 <- ifelse(dats$LENGTH_TYPE_2 == 'TLE',
                        -4.28+0.963*dats$lth_mm_2, dats$lth_mm_2)
hist(dats$lth_mm_1)
hist(dats$lth_mm_2)
hist(dats$LENGTH_2)

which(dats$lth_mm_2>2000)
dats$LENGTH_2[412]
dats$LENGTH_UNIT_2[412] ### looks like it should be mm not cm
dats$lth_mm_2[412] <- 805

dats$LENGTH_2[24189] ### looks like it was entered incorrectly, should read 25
dats$LENGTH_UNIT_2[24189]
dats$LENGTH_2[24189] <- 25
dats$lth_mm_2[24189] <- dats$LENGTH_2[24189] * 25.4 # convert from 

boxplot(dats$lth_mm_1 ~ dats$RELEASE_MONTH)
boxplot(dats$lth_mm_1 ~ dats$RELEASE_YEAR)

boxplot(dats$lth_mm_2 ~ dats$RECAPTURE_MONTH)
boxplot(dats$lth_mm_2 ~ dats$RECAPTURE_year)

boxplot(dats$lth_mm_1 ~ dats$COUNTRY_ID_2)
boxplot(dats$lth_mm_1 ~ dats$STATE_ID_2)

contract(g,imc$membership) |> plot()

# Decompose graph into connected components
comps <- decompose(g)
# Find the largest component
giant_comp <- comps[[which.max(sapply(comps, vcount))]]
# Run Spinglass
spinglass <- cluster_spinglass(giant_comp)
cluster_spinglass(g)
plot(spinglass, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)
plot(giant_comp, 
     edge.arrow.size=.2,
     layout = giant_comp, 
     rescale = FALSE)


library(MASS)
library(cmocean)


?kde2d

allrel_kde <- kde2d(dat$LONGITUDE_1, dat$LATITUDE_1,
                    n = c(length(lons)*10,length(lats)*10),
                    h = c(.5,.5))
allrel_kde$z[which(allrel_kde$z<quantile(allrel_kde$z,.95))] <- NA
brks <- pretty(allrel_kde$z,n=50)
col <- cmocean('thermal')(length(brks)-1)

image(allrel_kde, asp = 1, col = col, breaks = brks)
plot(world, add = T, col = 'gray')
points(dat$LONGITUDE_1, dat$LATITUDE_1)



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
lon_lat$lon <- lon_lat$lon - .5
lon_lat$lat <- lon_lat$lat - .5

### spatial domain
min_lon <- min(lons)
max_lon <- max(lons)
min_lat <- min(lats)
max_lat <- max(lats)

### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))
plot(lon_lat$lon, lon_lat$lat)


release <- data.frame(
  id =recaptures$id,
  lon = recaptures$LONGITUDE_1 |> as.numeric(),
  lat = recaptures$LATITUDE_1 |> as.numeric()
)
capture <- data.frame(
  id = recaptures$id,
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

nodes <- data.frame(grid = 1:length(nodes_id), xy.grid = nodes_id) |> 
  merge(lon_lat, by = 'xy.grid', all.x = T)
# nodes$lon <- nodes$lon - .5
# nodes$lat <- nodes$lat - .5

plot(recaptures$LONGITUDE_1, recaptures$LATITUDE_1, asp = 1,
     xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat))
arrows(recaptures$LONGITUDE_1, recaptures$LATITUDE_1,
       recaptures$LONGITUDE_2, recaptures$LATITUDE_2,
       length = .01)
points(nodes$lon, nodes$lat, pch = 21)

plot(world)
points(nodes$lon, nodes$lat, pch = 21, col = 2, cex = 2.5)
text(nodes$lon, nodes$lat, nodes$grid)

vertices <- nodes[,2:4]
### remove land nodes; 16, 46, 53
land <- c(16, 46, 53)
vertices <- vertices[-which(vertices$grid %in% land), ]

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
# edges_wt <- subset(edges_wt, weight>1)
edges_wt <- edges_wt[-which(edges_wt$to %in% land), ]
edges_wt <- edges_wt[-which(edges_wt$from %in% land), ]


# 3. Build the igraph object
g <- graph_from_data_frame(d = edges_wt, vertices = vertices, directed = TRUE)

# 4. Extract coordinates into a 2-column matrix [Lon, Lat]
geo_layout <- as.matrix(nodes[-which(nodes$grid %in% land), c("lon", "lat")])

# 5. Plot the graph with disabled rescaling
### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))

plot(world, col = 'gray')
plot(g, 
     layout = geo_layout, 
     rescale = FALSE, 
     edge.arrow.size=.3,
     # edge.arrow.mode=2,
     edge.color = 'dodgerblue3',
     vertex.size = 50, 
     # vertex.label = NA,
     edge.width = E(g)$weight/10,
     edge.curved=0.2,
     main = "Geographic igraph Network", add= T)

plot(g,
     edge.arrow.size=.3,
     # edge.arrow.mode=2,
     edge.color = 'dodgerblue3',
     edge.width = E(g)$weight/10)

plot(dats$LONGITUDE_1, dats$LATITUDE_1, asp = 1,
     xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat))
points(dats$LONGITUDE_2, dats$LATITUDE_2)
arrows(dats$LONGITUDE_1, dats$LATITUDE_1,
       dats$LONGITUDE_2, dats$LATITUDE_2,
       length = .01)
plot(world, add= T, col = 'gray')

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
motifs(g)
neighborhood_size(g)
n_g <- make_neighborhood_graph(g)
plot(g, vertex.size=neighborhood_size(g)*10, 
     layout = geo_layout, 
     edge.arrow.size=.2, 
     rescale = FALSE)

edge_density(g,loops=T)
edge_n_gedge_density(g, loops=F)
reciprocity(g)
dyad_census(g) # Mutual, asymmetric, and nyll node pairs
transitivity(g, type="global")  # net is treated as an undirected network
transitivity(g, type="local")
triad_census(g) # for directed networks 
diameter(g, directed=T)
get_diameter(g, directed=T)
deg <- degree(g, mode="out")
plot(g, vertex.size=deg*20, 
     layout = geo_layout, 
     edge.arrow.size=.2,
     edge.width = E(g)$weight/10,
     rescale = FALSE)
deg.dist <- degree_distribution(g, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

kcore <- coreness(g)
plot(g, vertex.size=kcore*10, 
     layout = geo_layout, 
     edge.arrow.size=.2,
     edge.width = E(g)$weight/10,
     rescale = FALSE)

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
cliques(g) |> plot()
sapply(cliques(g), length) # clique sizes
largest_cliques(g) # cliques with max number of nodes

ceb <- cluster_edge_betweenness(g, weights = E(g)$weight) 
plot_dendrogram(ceb, mode="hclust")
plot(ceb, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE) 

clp <- cluster_label_prop(g, weights = E(g)$weight)
plot(clp, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)

cfg <- cluster_fast_greedy(as_undirected(g))
plot(cfg, as_undirected(g), 
     edge.arrow.size=.2, 
     layout = geo_layout, 
     rescale = FALSE)

wc <- cluster_walktrap(g, weights = E(g)$weight, steps = 4)
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
lec2 <- cluster_leading_eigen(g, start = wc$membership)
plot(lec, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)
plot(world, add=T)
plot(lec2, g, 
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)

cen <- alpha_centrality(g)
plot(g, 
     vertex.size=cen*5,
     edge.arrow.size=.2,
     layout = geo_layout, 
     rescale = FALSE)




