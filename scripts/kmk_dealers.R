

library(dplyr)
library(here)
library(stringr)
library(viridis)
library(dplyr)
library(here)
library(lubridate)
library(stringr)
library(sf)
library(terra)
library(tigris)
library(ggplot2)
library(rnaturalearth) # Provides map data
library(rnaturalearthdata)

# 1. Fetch map data as an sf object
# world <- ne_countries(scale = "large", returnclass = "sf")
states <- ne_states(country = 'United States of America', returnclass = "sf")

### county shapfiles from Census TIGRIS
cnty <- counties()
# states <- states()
### which state FIPS: FL, AL, MS, LA, TX
stfps <- paste(sprintf('%02d',c(12, 01, 28, 22, 48)))
gulf <- subset(cnty, STATEFP %in% stfps)

setwd("~/data/shapefiles/GSHHS_shp/i")
world <- vect('GSHHS_i_L1.shp') |> st_as_sf()


### identify coastal and coastal adjacent counties using NCCOS's ENOW dataset via Seann
setwd("~/R_projects/Fishing-Community-Resilience/data/ENOW_Sectors")
coast_ref <- read.csv('ENOW_Geography_Reference.csv') 
coast_ref$County_Name[299] <- 'Multnomah'
coast_ref$County_Name <- toupper(coast_ref$County_Name)
coast_ref$County_Name <- gsub('\\.','',coast_ref$County_Name)
gulf_cnty <- subset(coast_ref, Region =='Gulf of Mexico')
gulf_cnty$GEOID <- str_pad(gulf_cnty$GEOID, 5, side = 'left', pad = '0')

gulf <- merge(gulf, gulf_cnty) |>
  filter(shore.Adjacent == 1)



### load data ------------------------
setwd("C:/Users/brendan.turley/Documents/R_projects/Fishing-Community-Resilience/data")
# dat <- read.csv('lbcw_gom_combined_cleaned.csv')
dat <- read.csv('lbcw_gom_combined_cleaned_v03.csv')
dat <- dat[which(dat$shore.Adjacent == 1), ]

names(dat)

### dealers overtime
kmk_dat <- subset(dat, Species.ITIS=='172435')

# kmk_dlr <- unique(kmk_dat$License)
kmk_dlr <- unique(kmk_dat$SupplierDealer.ID)

cnty_n <- aggregate(SupplierDealer.ID ~ dealer_county + dealer_geoid,
                    data = kmk_dat, function(x) length(unique(x))) |>
  # arrange(desc(License)) |>
  setNames(c('county','GEOID','License'))

aggregate(SupplierDealer.ID ~ Year, data = kmk_dat, function(x) length(unique(x))) |>
  setNames(c('Year','License')) |>
  plot(typ = 'o')

subset(dat, SupplierDealer.ID %in% kmk_dlr) |>
  group_by(Year) |>
  summarize(
    License = n_distinct(SupplierDealer.ID)
    ) |>
  plot(typ = 'o')

dlr_st <- subset(dat, SupplierDealer.ID %in% kmk_dlr) |>
  group_by(Year, LandingState) |>
  summarize(
    License = n_distinct(SupplierDealer.ID)
  )
plot(dlr_st$Year, dlr_st$License, typ = 'n', xlab = '', ylab = 'Number of dealers')
for(i in unique(dlr_st$LandingState)){
  with(subset(dlr_st, LandingState==i),
       points(Year, License, col = which(i==unique(dlr_st$LandingState)), typ = 'o', pch = 16, lwd = 2))
}


dlr_st <- kmk_dat |>
  group_by(Year, LandingState) |>
  summarize(
    License = n_distinct(SupplierDealer.ID)
  )
plot(dlr_st$Year, dlr_st$License, typ = 'n', xlab = '', ylab = 'Number of dealers')
for(i in unique(dlr_st$LandingState)){
  with(subset(dlr_st, LandingState==i),
       points(Year, License, col = which(i==unique(dlr_st$LandingState)), typ = 'o', pch = 16, lwd = 2))
}


### what has most landings?
aggregate(Landed.Lbs ~ Common.Name, data = dat, sum, na.rm = T) |> View()
aggregate(value_2024 ~ Common.Name, data = dat, sum, na.rm = T) |> View()

kmk_dat <- subset(dat, Species.ITIS=='172435')
kmk_dat$dol_lbs <- kmk_dat$value_2024 / kmk_dat$Landed.Lbs

val_st_yr <- aggregate(value_2024 ~ LandingState + Year, data = kmk_dat, sum, na.rm = T)
lbs_st_yr <- aggregate(Landed.Lbs ~ LandingState + Year, data = kmk_dat, sum, na.rm = T)
ppp_st_yr <- aggregate(dol_lbs ~ LandingState + Year, data = kmk_dat, mean, na.rm = T) # mean price / pound
dlr_st_yr <- aggregate(License ~ LandingState + Year, data = kmk_dat, function(x) length(unique(x)))

st_yr_m <- merge(val_st_yr, lbs_st_yr, by = c('LandingState','Year'))
st_yr_m$dol_lbs <- st_yr_m$value_2024 / st_yr_m$Landed.Lbs # aggregated price / pound

ld_yr_m <- merge(lbs_st_yr, dlr_st_yr, by = c('LandingState','Year')) |>
  merge(val_st_yr,by = c('LandingState','Year'))
ld_yr_m$lbs_dlr <- ld_yr_m$Landed.Lbs / ld_yr_m$License 
ld_yr_m$dol_dlr <- ld_yr_m$value_2024 / ld_yr_m$License 

states <- sort(unique(dlr_st_yr$LandingState))


par(mfrow = c(2,2))

plot(val_st_yr$Year, val_st_yr$value_2024, typ = 'n', 
     xlab = '',ylab = 'Total value (2023 USD)')
for(i in states){
  with(subset(val_st_yr, LandingState==i),
       points(Year, value_2024, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
}
# legend('topleft',states, lty = 1, col = 1:5, lwd = 2)

plot(lbs_st_yr$Year, lbs_st_yr$Landed.Lbs, typ = 'n', 
     xlab = '',ylab = 'Total landings (lbs)')
for(i in states){
  with(subset(lbs_st_yr, LandingState==i),
       points(Year, Landed.Lbs, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
}
# legend('topleft',states, lty = 1, col = 1:5, lwd = 2)

plot(dlr_st_yr$Year, dlr_st_yr$License, typ = 'n', 
     xlab = '',ylab = 'Number of dealers')
for(i in states){
  with(subset(dlr_st_yr, LandingState==i),
       points(Year, License, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
}
legend('topright',states, lty = 1, col = 1:5, lwd = 2,cex=.7)

# plot(st_yr_m$Year, st_yr_m$dol_lbs, typ = 'n',
#      xlab = '',ylab = 'USD / lbs (2023 USD)')
# for(i in states){
#   with(subset(st_yr_m, LandingState==i),
#        points(Year, dol_lbs, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
# }
# legend('topleft',states, lty = 1, col = 1:5, lwd = 2)

plot(ppp_st_yr$Year, ppp_st_yr$dol_lbs, typ = 'n', 
     xlab = '',ylab = 'USD / lbs (2023 USD)')
for(i in states){
  with(subset(ppp_st_yr, LandingState==i),
       points(Year, dol_lbs, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
}
# legend('topleft',states, lty = 1, col = 1:5, lwd = 2)

plot(ld_yr_m$Year, ld_yr_m$lbs_dlr, typ = 'n', 
     xlab = '',ylab = 'lbs / dealer')
for(i in states){
  with(subset(ld_yr_m, LandingState==i),
       points(Year, lbs_dlr, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
}

plot(ld_yr_m$Year, ld_yr_m$dol_dlr, typ = 'n', 
     xlab = '',ylab = 'USD / dealer')
for(i in states){
  with(subset(ld_yr_m, LandingState==i),
       points(Year, dol_dlr, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
}




plot(dlr_st_yr$Year, dlr_st_yr$License/mean(dlr_st_yr$License), typ = 'n', 
     xlab = '',ylab = 'Number of dealers', ylim = c(0,2.5))
for(i in states){
  with(subset(dlr_st_yr, LandingState==i),
       points(Year, License/mean(License), col = which(i==states), typ = 'o', pch = 16, lwd = 2))
  print(i)
  with(subset(dlr_st_yr, LandingState==i),
       lm(License ~ Year) |> summary() |> print())
}
legend('topright',states, lty = 1, col = 1:5, lwd = 2,cex=.7)




### map of value and landings per county overall
kmk_dat |>
  group_by(dealer_county, dealer_geoid) |>
  summarize(
    total_value = sum(value_2024, na.rm = TRUE),
    total_lbs = sum(Landed.Lbs, na.rm = TRUE),
    unique_dealers = n_distinct(License)
  ) |>
  arrange(desc(total_value))

cnty_sum <- kmk_dat |> 
  group_by(Year, dealer_county, dealer_geoid) |>
  summarize(
    total_value = sum(value_2024, na.rm = TRUE),
    total_lbs = sum(Landed.Lbs, na.rm = TRUE)
  ) |> 
  group_by(dealer_county, dealer_geoid) |>
  summarize(
    total_value = mean(total_value, na.rm = TRUE),
    total_lbs = mean(total_lbs, na.rm = TRUE)
  ) |>
  rename(GEOID = dealer_geoid)
  
gulf_val <- merge(gulf,cnty_sum, by = 'GEOID')

# cnty_n <- aggregate(License ~ dealer_county + dealer_geoid, data = kmk_dat, function(x) length(unique(x))) |>
#   # arrange(desc(License)) |>
#   setNames(c('county','GEOID','License'))
# cnty_n <- merge(gulf, cnty_n, by = 'GEOID')

cnty_n <- subset(dat, SupplierDealer.ID %in% kmk_dlr) |>
  group_by(dealer_county, dealer_geoid) |>
  summarize(
    License = n_distinct(SupplierDealer.ID)
  ) |>
  setNames(c('county','GEOID','License')) |>
  filter(License>=3)
cnty_n <- merge(gulf, cnty_n, by = 'GEOID')

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = gulf_val, 
          aes(fill = total_value)) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "2024 USD",
    labels = scales::label_comma(),
    na.value = "grey50",
    option = 'G',
    direction = -1
  ) +
  geom_sf(data = states, fill = NA) +
  coord_sf(
    xlim = c(-98, -80), 
    ylim = c(24, 31)
  ) + 
  labs(
    title = "King Mackerel Value by County",
    subtitle = "Mean Annual value 2000-2024",
    caption = "SEFSC-SSRG dealer dataset",
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal()
ggsave('kgm_usd_county.png', width = 7, height = 5, units = 'in',
       path = "~/R_projects/Looking_4_CMP/figs")

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = gulf_val,
          aes(fill = total_lbs)) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "Pounds",
    labels = scales::label_comma(),
    na.value = "grey50",
    option = 'F',
    direction = -1
  ) +
  geom_sf(data = states, fill = NA) +
  coord_sf(
    xlim = c(-98, -80), 
    ylim = c(24, 31)
  ) + 
  labs(
    title = "King Mackerel Landings by County",
    subtitle = "Mean Annual landings 2000-2024",
    caption = "SEFSC-SSRG dealer dataset",
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal()
ggsave('kgm_lbs_county.png', width = 7, height = 5, units = 'in',
       path = "~/R_projects/Looking_4_CMP/figs")


ggplot() +
  geom_sf(data = world) +
  geom_sf(data = cnty_n,
          aes(fill = License)) +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Dealers",
    labels = scales::label_comma(),
    na.value = "grey50",
    option = 'F',
    direction = -1
  ) +
  geom_sf(data = states, fill = NA) +
  coord_sf(
    xlim = c(-98, -80), 
    ylim = c(24, 31)
  ) +
  labs(
    title = "King Mackerel Dealers per County",
    subtitle = "Total Dealers 2000-2024",
    caption = "SEFSC-SSRG dealer dataset",
    x = 'Longitude', y = 'Latitude'
  ) +
  theme_minimal()
ggsave('kgm_dlr_county.png', width = 7, height = 5, units = 'in',
       path = "~/R_projects/Looking_4_CMP/figs")


### trend in landings per county over time

library(Kendall)

kmk_ann <- kmk_dat |> 
  group_by(Year, dealer_county, dealer_geoid) |>
  summarize(
    total_value = sum(value_2024, na.rm = TRUE),
    total_lbs = sum(Landed.Lbs, na.rm = TRUE)#,
    # unique_dealers = n_distinct(License)
  )

kmk_yr_dlr <- subset(dat, SupplierDealer.ID %in% kmk_dlr) |>
  group_by(Year, dealer_county, dealer_geoid) |>
  summarize(
    unique_dealers = n_distinct(SupplierDealer.ID)
  )
kmk_ann <- merge(kmk_ann, kmk_yr_dlr, by = c('Year', 'dealer_county', 'dealer_geoid')) 

dlr_rm <- subset(dat, SupplierDealer.ID %in% kmk_dlr) |> 
  group_by(dealer_county, dealer_geoid) |>
  summarize(
    unique_dealers = n_distinct(License)
  ) |>
  filter(unique_dealers < 3)

dlr_rm <- kmk_dat |>
  group_by(dealer_county, dealer_geoid) |>
  summarize(
    unique_dealers = n_distinct(License)
  ) |>
  filter(unique_dealers < 3)

filtered_data <- kmk_ann |>
  # group_by(dealer_geoid) |>
  # filter(max(unique_dealers) > 2) |>
  # group_by(unique_dealers) |>
  # filter(unique_dealers >= 3) |>
  ungroup() |>
  group_by(dealer_geoid) |>
  filter(n() >= 4)

county_mk_trends <- filtered_data %>%
  # Ensure data is sorted chronologically within each county
  arrange(dealer_geoid, Year) %>% 
  group_by(dealer_geoid) %>%
  summarize(
    # Tau ranges from -1 (perfect decrease) to 1 (perfect increase)
    mk_tau = as.numeric(MannKendall(total_lbs)$tau),
    p_value = as.numeric(MannKendall(total_lbs)$sl),
    mk_tau2 = as.numeric(MannKendall(unique_dealers)$tau),
    p_value2 = as.numeric(MannKendall(unique_dealers)$sl)
  ) |>
  rename(GEOID = dealer_geoid)
county_mk_trends <- subset(county_mk_trends, !is.element(GEOID, dlr_rm$dealer_geoid))

gulf_trend <- merge(gulf,county_mk_trends, by = 'GEOID') |>
  filter(p_value2 < 1)
# gulf_trend$mk_tau[gulf_trend$p_value>.1] <- 0



ggplot() +
  geom_sf(data = world) +
  geom_sf(data = gulf_trend,
          aes(fill = mk_tau)) +
  scale_fill_gradient2(
    low = "blue",        # Color for negative values (decreasing trend)
    mid = "white",       # Color exactly at 0 (no trend)
    high = "red",        # Color for positive values (increasing trend)
    midpoint = 0,        # Explicitly centers the scale at 0
    name = "Trend Slope",
    labels = scales::label_comma()
  ) +
  geom_sf(data = states, fill = NA) +
  stat_sf_coordinates(
    data = filter(gulf_trend, p_value < 0.05), # Filters data on the fly
    shape = 21,          # Equivalent to pch = 21 (allows both fill and border color)
    fill = "black",      # Interior background color (bg = 1)
    color = "white",     # Outer border line color (col = 'white')
    size = 2,            # Bumped size slightly so the white border is crisp
    stroke = 1 
  )  +
  coord_sf(
    xlim = c(-98, -80), 
    ylim = c(24, 31)
  ) +
  labs(title = "Landings Trend Over Time by County",
       x = 'Longitude', y = 'Latitude') +
  theme_minimal()
ggsave('kgm_lbs_county_t.png', width = 7, height = 5, units = 'in',
       path = "~/R_projects/Looking_4_CMP/figs")


ggplot() +
  geom_sf(data = world) +
  geom_sf(data = gulf_trend, 
          aes(fill = mk_tau2)) +
  scale_fill_gradient2(
    low = "blue",        # Color for negative values (decreasing trend)
    mid = "white",       # Color exactly at 0 (no trend)
    high = "red",        # Color for positive values (increasing trend)
    midpoint = 0,        # Explicitly centers the scale at 0
    name = "Trend Slope",
    labels = scales::label_comma()
  ) +
  geom_sf(data = states, fill = NA) +
  stat_sf_coordinates(
    data = filter(gulf_trend, p_value2 < 0.05), # Filters data on the fly
    shape = 21,          # Equivalent to pch = 21 (allows both fill and border color)
    fill = "black",      # Interior background color (bg = 1)
    color = "white",     # Outer border line color (col = 'white')
    size = 2,            # Bumped size slightly so the white border is crisp
    stroke = 1 
  ) +
  coord_sf(
    xlim = c(-98, -80), 
    ylim = c(24, 31)
  ) +
  labs(title = "Dealers Trend Over Time by County",
       x = 'Longitude', y = 'Latitude') +
  theme_minimal()
ggsave('kgm_dlr_county_t.png', width = 7, height = 5, units = 'in',
       path = "~/R_projects/Looking_4_CMP/figs")


### scratch ###
lm_ts <- function(x){
  mod <- lm(x ~ c(1:length(x)))
  return(list(b = coef(mod)[2],
              p_val = summary(mod)$coefficients[2,4]))
}

lm_ts(rnorm(100))

county_mk_trends <- filtered_data %>%
  # Ensure data is sorted chronologically within each county
  arrange(dealer_geoid, Year) %>% 
  group_by(dealer_geoid) %>%
  summarize(
    # Tau ranges from -1 (perfect decrease) to 1 (perfect increase)
    lm_b = as.numeric(lm_ts(total_lbs)$b),
    p_value = as.numeric(lm_ts(total_lbs)$p_val),
    lm_b2 = as.numeric(lm_ts(unique_dealers)$b),
    p_value2 = as.numeric(lm_ts(unique_dealers)$p_val)
  ) |>
  rename(GEOID = dealer_geoid)

gulf_trend <- merge(gulf,county_mk_trends, by = 'GEOID')
# gulf_trend$mk_tau[gulf_trend$p_value>.1] <- 0

ggplot(data = gulf_trend) +
  geom_sf(aes(fill = lm_b)) +
  scale_fill_gradient2(
    low = "blue",        # Color for negative values (decreasing trend)
    mid = "white",       # Color exactly at 0 (no trend)
    high = "red",        # Color for positive values (increasing trend)
    midpoint = 0,        # Explicitly centers the scale at 0
    name = "Trend Slope",
    labels = scales::label_comma()
  ) +
  stat_sf_coordinates(
    data = ~ filter(.x, p_value < 0.05), # Filters data on the fly
    shape = 21,          # Equivalent to pch = 21 (allows both fill and border color)
    fill = "black",      # Interior background color (bg = 1)
    color = "white",     # Outer border line color (col = 'white')
    size = 3,            # Bumped size slightly so the white border is crisp
    stroke = 1 
  )  +
  labs(title = "Landings Trend Over Time by County") +
  theme_minimal()


ggplot(data = gulf_trend) +
  geom_sf(aes(fill = lm_b2)) +
  scale_fill_gradient2(
    low = "blue",        # Color for negative values (decreasing trend)
    mid = "white",       # Color exactly at 0 (no trend)
    high = "red",        # Color for positive values (increasing trend)
    midpoint = 0,        # Explicitly centers the scale at 0
    name = "Trend Slope",
    labels = scales::label_comma()
  ) +
  stat_sf_coordinates(
    data = ~ filter(.x, p_value2 < 0.05), # Filters data on the fly
    shape = 21,          # Equivalent to pch = 21 (allows both fill and border color)
    fill = "black",      # Interior background color (bg = 1)
    color = "white",     # Outer border line color (col = 'white')
    size = 3,            # Bumped size slightly so the white border is crisp
    stroke = 1 
  )  +
  labs(title = "Dealers Trend Over Time by County") +
  theme_minimal()


kmk_dat

cnty_val <- aggregate(value_2024 ~ dealer_county + dealer_geoid, data = kmk_dat, sum, na.rm = T) |>
  arrange(desc(value_2024)) |>
  setNames(c('county','GEOID','value_2024'))
cnty_lbs <- aggregate(Landed.Lbs ~ dealer_county + dealer_geoid, data = kmk_dat, sum, na.rm = T) |>
  arrange(desc(Landed.Lbs)) |>
  setNames(c('county','GEOID','Landed.Lbs'))
cnty_n <- aggregate(License ~ dealer_county + dealer_geoid, data = kmk_dat, function(x) length(unique(x))) |>
  arrange(desc(License)) |>
  setNames(c('county','GEOID','License'))
# cnty_val <- cnty_val[-which(cnty_n$License<3),]
# cnty_lbs <- cnty_lbs[-which(cnty_n$License<3),]
gulf_val <- merge(gulf, cnty_val, by = 'GEOID') |>
  merge(cnty_lbs, by = 'GEOID') |>
  merge(cnty_n, by = 'GEOID')
# gulf_val$value_2024 <- log10(gulf_val$value_2024)
# gulf_val$value_2024 <- 10^(gulf_val$value_2024)



ggplot(data = gulf_val) +
  geom_sf(aes(fill = value_2024)) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "2024 USD",
    labels = scales::label_comma(),
    na.value = "grey50",
    option = 'G',
    direction = -1
  ) +
  labs(
    title = "King Mackerel Value by County",
    subtitle = "Total landed value 2001-2024",
    caption = "SEFSC-SSRG dealer dataset"
  ) +
  theme_minimal()

ggplot(data = gulf_val) +
  geom_sf(aes(fill = Landed.Lbs)) +
  scale_fill_viridis_c(
    trans = "log10",
    name = "Pounds",
    labels = scales::label_comma(),
    na.value = "grey50",
    option = 'F',
    direction = -1
  ) +
  labs(
    title = "King Mackerel Landings by County",
    subtitle = "Total landings 2001-2024",
    caption = "SEFSC-SSRG dealer dataset"
  ) +
  theme_minimal()

ggplot(data = gulf_val) +
  geom_sf(aes(fill = License)) +
  scale_fill_viridis_c(
    # trans = "log10",
    name = "Pounds",
    labels = scales::label_comma(),
    na.value = "grey50",
    option = 'F',
    direction = -1
  ) +
  labs(
    title = "King Mackerel Dealers per County",
    subtitle = "Total Dealers 2001-2024",
    caption = "SEFSC-SSRG dealer dataset"
  ) +
  theme_minimal()
