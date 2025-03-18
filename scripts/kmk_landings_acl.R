
setwd("C:/Users/brendan.turley/Documents/CMP/data")
land <- read.csv('kmk_comm_land.csv')
land$year1 <- 2016:2022

gn_land <- read.csv('kmk_comm_land_gn.csv')
gn_land$year1 <- 2016:2022

wcol <- c(2,3,5,6,8,9)
plot(land$year1, land$WZ_Quota, typ = 'n', ylim = c(0,1.5e6))
for(i in 2:3){
  points(land$year1, land[,i], col = 1)
}

plot(land$year1, land$WZ_Quota, typ = 'n', ylim = c(0,.75e6))
for(i in 5:6){
  points(land$year1, land[,i], col = 1)
}

plot(land$year1, land$WZ_Quota, typ = 'n', ylim = c(0,1e6))
for(i in 8:9){
  points(land$year1, land[,i], col = 1)
}

setwd("~/R_projects/misc-noaa-scripts/figs")
png('zone_land_quota.png', width = 10, height = 7, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(land$year1, land[,2]/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = 'Landings (x1,000,000 lbs.)')
grid()
points(land$year1, land[,3]/1e6,
     typ = 'l', lty = 5, lwd = 2)
with(subset(land, WZ > WZ_Quota),
     points(year1, WZ/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Western HL')

legend('bottomleft', c('Sector ACL', "Commercial Landings", 'Overages'),
       col = c(1,1,'red'), lty = c(5,1,NA), pch = c(NA, NA, 17), bty = 'n')

plot(land$year1, land[,5]/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = '')
grid()
points(land$year1, land[,6]/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, NZ > NZ_Quota),
     points(year1, NZ/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Northern HL')

plot(land$year1, land[,8]/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = 'Landings (x1,000,000 lbs.)')
grid()
points(land$year1, land[,9]/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, SZ > SZ_Quota),
     points(year1, SZ/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Southern HL')

plot(gn_land$year1, gn_land[,2]/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = '')
grid()
points(gn_land$year1, gn_land[,3]/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(gn_land, South_Zone_GN > South_Zone_GN_Quota),
     points(year1, South_Zone_GN/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Southern GN')

dev.off()

subset(gn_land, South_Zone_GN > South_Zone_GN_Quota)
