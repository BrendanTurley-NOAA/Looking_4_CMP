

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

### county shapfiles from Census TIGRIS
cnty <- counties()
### which state FIPS: FL, AL, MS, LA, TX
stfps <- paste(sprintf('%02d',c(12, 01, 28, 22, 48)))
gulf <- subset(cnty, STATEFP %in% stfps)


### identify coastal and coastal adjacent counties using NCCOS's ENOW dataset via Seann
setwd(paste0(here(),'/',"data",'/',"ENOW_Sectors"))
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

ggplot(data = gulf_val) +
  geom_sf(aes(fill = total_value)) +
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
    subtitle = "Mean Annual value 2001-2024",
    caption = "SEFSC-SSRG dealer dataset"
  ) +
  theme_minimal()

ggplot(data = gulf_val) +
  geom_sf(aes(fill = total_lbs)) +
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
    subtitle = "Mean Annual landings 2001-2024",
    caption = "SEFSC-SSRG dealer dataset"
  ) +
  theme_minimal()

### trend in landings per county over time

library(Kendall)

kmk_ann <- kmk_dat |> 
  group_by(Year, dealer_county, dealer_geoid) |>
  summarize(
    total_value = sum(value_2024, na.rm = TRUE),
    total_lbs = sum(Landed.Lbs, na.rm = TRUE),
    unique_dealers = n_distinct(License)
  )

filtered_data <- kmk_ann |>
  group_by(dealer_geoid) |>
  filter(n() >= 4) |>
  ungroup() # Always good practice to ungroup after group-specific operations

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

gulf_trend <- merge(gulf,county_mk_trends, by = 'GEOID')
# gulf_trend$mk_tau[gulf_trend$p_value>.1] <- 0

ggplot(data = gulf_trend) +
  geom_sf(aes(fill = mk_tau)) +
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
  geom_sf(aes(fill = mk_tau2)) +
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
