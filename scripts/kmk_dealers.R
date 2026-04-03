

library(dplyr)
library(here)
library(stringr)
library(viridis)

### load data ------------------------
setwd("C:/Users/brendan.turley/Documents/R_projects/Fishing-Community-Resilience/data")
dat <- read.csv('lbcw_gom_combined_cleaned.csv')
dat <- dat[which(dat$shore.Adjacent == 1), ]

names(dat)

kmk_dat <- subset(dat, Species.ITIS=='172435')
kmk_dat$dol_lbs <- kmk_dat$value_2023 / kmk_dat$Landed.Lbs

val_st_yr <- aggregate(value_2023 ~ LandingState + Year, data = kmk_dat, sum, na.rm = T)
lbs_st_yr <- aggregate(Landed.Lbs ~ LandingState + Year, data = kmk_dat, sum, na.rm = T)
ppp_st_yr <- aggregate(dol_lbs ~ LandingState + Year, data = kmk_dat, mean, na.rm = T) # mean price / pound
dlr_st_yr <- aggregate(License ~ LandingState + Year, data = kmk_dat, function(x) length(unique(x)))

st_yr_m <- merge(val_st_yr, lbs_st_yr, by = c('LandingState','Year'))
st_yr_m$dol_lbs <- st_yr_m$value_2023 / st_yr_m$Landed.Lbs # aggregated price / pound

ld_yr_m <- merge(lbs_st_yr, dlr_st_yr, by = c('LandingState','Year')) |>
  merge(val_st_yr,by = c('LandingState','Year'))
ld_yr_m$lbs_dlr <- ld_yr_m$Landed.Lbs / ld_yr_m$License 
ld_yr_m$dol_dlr <- ld_yr_m$value_2023 / ld_yr_m$License 

states <- sort(unique(dlr_st_yr$LandingState))


par(mfrow = c(2,2))

plot(val_st_yr$Year, val_st_yr$value_2023, typ = 'n', 
     xlab = '',ylab = 'Total value (2023 USD)')
for(i in states){
  with(subset(val_st_yr, LandingState==i),
       points(Year, value_2023, col = which(i==states), typ = 'o', pch = 16, lwd = 2))
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
