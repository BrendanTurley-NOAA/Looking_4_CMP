
library(cmocean)
library(readxl)

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



#################
#### updated ####
#################

library(readxl)

setwd("C:/Users/brendan.turley/Documents/CMP/data/landings")
land <- read_xlsx('kmk_landings_sero_acl.xlsx', sheet = 2) |> as.data.frame()
land$year1 <- 2024:2016

(land$NZ[1]-land$NZ[9])/land$NZ[9]
(land$SZ[1]-land$SZ[9])/land$SZ[9]
(land$WZ[1]-land$WZ[9])/land$WZ[9]

gn_land <- read_xlsx('kmk_landings_sero_acl.xlsx', sheet = 3) |> as.data.frame()
gn_land$year1 <- 2024:2012
gn_land <- subset(gn_land, year1>=2016)

tot_land <- cbind(land[,2:4] |> rowSums(),
      gn_land$`Total Reported`) |> rowSums()
tot_quota <- cbind(land[,5:7] |> rowSums(),
      gn_land$`Quota`) |> rowSums()
(tot_land[1]-tot_land[9])/tot_land[9]

plot(land$year1, tot_land/1e6, typ = 'l', lwd = 2, pch = 16,
     panel.first = abline(h = 0, lty = 5), las = 1,
     xlab = '', ylab = 'Landings (x1,000,000 lbs.)',
     ylim = c(0, 4))
points(land$year1, tot_quota/1e6, typ = 'l', lwd = 2, lty = 5)


setwd("~/R_projects/misc-noaa-scripts/figs")
png('zone_land_quota_update.png', width = 10, height = 7, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(land$year1, land$WZ/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = 'Landings (x1,000,000 lbs.)')
grid()
points(land$year1, land$`WZ Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, WZ > `WZ Quota`),
     points(year1, WZ/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Western HL')
abline(h = 0, lwd = 1.5)

plot(land$year1, land$NZ/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = '')
grid()
points(land$year1, land$`NZ Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, NZ > `NZ Quota`),
     points(year1, NZ/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Northern HL')
abline(h = 0, lwd = 1.5)

legend('topright', c('Sector ACL', "Commercial Landings", 'Overages'),
       col = c(1,1,'red'), lty = c(5,1,NA), pch = c(NA, NA, 17), bty = 'n')

plot(land$year1, land$SZ/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = 'Landings (x1,000,000 lbs.)')
grid()
points(land$year1, land$`SZ Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, SZ > `SZ Quota`),
     points(year1, SZ/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Southern HL')
abline(h = 0, lwd = 1.5)

plot(gn_land$year1, gn_land$`Total Reported`/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), xaxt = 'n', las = 1,
     xlab = '', ylab = '')
grid()
points(gn_land$year1, gn_land$`Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(gn_land, `Total Reported` > `Quota`),
     points(year1, `Total Reported`/1e6, col = 'red', pch = 17, cex = 2))
axis(1, land$year1, land$Year, cex.axis = .7)
mtext('Southern GN')
abline(h = 0, lwd = 1.5)
dev.off()


setwd("~/R_projects/misc-noaa-scripts/figs")
png('zone_land_quota_update2.png', width = 7, height = 5, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,4,1,1))
plot(land$year1, land$WZ/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), las = 1,
     xlab = '', ylab = 'Landings (million lbs.)')
grid()
points(land$year1, land$`WZ Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, WZ > `WZ Quota`),
     points(year1, WZ/1e6, col = 'red', pch = 17, cex = 1.5))
axis(1, land$year1)
mtext('Western HL')
abline(h = 0, lwd = 1.5, lty = 2)

plot(land$year1, land$NZ/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), las = 1,
     xlab = '', ylab = '')
grid()
points(land$year1, land$`NZ Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, NZ > `NZ Quota`),
     points(year1, NZ/1e6, col = 'red', pch = 17, cex = 1.5))
axis(1, land$year1)
mtext('Northern HL')
abline(h = 0, lwd = 1.5, lty = 2)

legend('topleft', c('Sector ACL', "Commercial Landings", 'Overages'),
       col = c(1,1,'red'), lty = c(5,1,NA), pch = c(NA, NA, 17), bty = 'n',
       cex = .9)

plot(land$year1, land$SZ/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), las = 1,
     xlab = '', ylab = 'Landings (million lbs.)')
grid()
points(land$year1, land$`SZ Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(land, SZ > `SZ Quota`),
     points(year1, SZ/1e6, col = 'red', pch = 17, cex = 1.5))
axis(1, land$year1)
mtext('Southern HL')
abline(h = 0, lwd = 1.5, lty = 2)

plot(gn_land$year1, gn_land$`Total Reported`/1e6,
     typ = 'l', lty = 1, lwd = 2,
     ylim = c(0,1.4), las = 1,
     xlab = '', ylab = '')
grid()
points(gn_land$year1, gn_land$`Quota`/1e6,
       typ = 'l', lty = 5, lwd = 2)
with(subset(gn_land, `Total Reported` > `Quota`),
     points(year1, `Total Reported`/1e6, col = 'red', pch = 17, cex = 1.5))
axis(1, land$year1)
mtext('Southern GN')
abline(h = 0, lwd = 1.5, lty = 2)
dev.off()


### from SERO - Micheal Larkin
setwd("C:/Users/brendan.turley/Documents/CMP/data/landings")
acls <- read.csv('kgm_acls.csv')
rec_state <- read_xlsx('Gulf_king_mackerel_rec_landings_Oct2026.xlsx', sheet = 3) |> as.data.frame()
rec_mode <- read_xlsx('Gulf_king_mackerel_rec_landings_Oct2026.xlsx', sheet = 2) |> as.data.frame()

rec_state_pro <- sweep(rec_state[,3:7], 1, rec_state$total_ww_lbs, FUN = '/') |> t()
rec_mode_pro <- sweep(rec_mode[,3:6], 1, rec_mode$total_ww_lbs, FUN = '/') |> t()

cols <- c('orangered1','gold','gray','cornflowerblue','purple4')
cols2 <- cmocean('deep', direction = 1)(4)


setwd("~/R_projects/King-Mackerel-ESP/figures/plots")
png('kgm_landings_rec.png',
    width = 7, height = 9, units = 'in', res = 300, pointsize = 12)
par(mar = c(5,5,1,5),mfrow=c(3,1))

b <- barplot(rec_state$total_ww_lbs/1e6, names.arg = 2000:2024, las = 2,
             ylab = 'Total Recreational Landings \n(million lbs)')
lines(c(b-.6,b[length(b)]), 
      c(acls$recreational[which(acls$fishing_year %in% 2000:2024)], 
        acls$recreational[which(acls$fishing_year %in% 2000:2024)][length(b)]),
      lwd = 2,
      typ = 's')
mtext('a)', font = 2, side = 3, adj = 0.01)

b2 <- barplot(rec_state_pro, names.arg = 2000:2024, las = 2,
              ylab = 'Landings by State', col = cols, yaxt = 'n')
axis(2,seq(0,1,0.2),labels = paste0(seq(0,100,20),'%'), las = 2)
legend(b2[25]+.5,1, c('FL','AL','MS','LA','TX'), 
       fill = rev(cols), xpd = T, bty = 'n')
mtext('b)', font = 2, side = 3, adj = 0.01)

b3 <- barplot(rec_mode_pro, names.arg = 2000:2024, las = 2,
              ylab = 'Landings by Mode', xlab = 'Fishing Year', col = cols2, yaxt = 'n')
axis(2,seq(0,1,0.2),labels = paste0(seq(0,100,20),'%'), las = 2)
legend(b3[25]+.5,1, rev(rownames(rec_mode_pro)), 
       fill = rev(cols2), xpd = T, bty = 'n',xjust = 0)
mtext('c)', font = 2, side = 3, adj = 0.01)

dev.off()

setwd("C:/Users/brendan.turley/Documents/CMP/data/landings")
com <- read_xlsx('KM_com_land_2625_20260428C.xlsx', sheet = 2) |> as.data.frame()
com <- subset(com, FISHING_YEAR>1999)
tot_lbs <- aggregate(tot_lbs ~ FISHING_YEAR, data = com, sum, na.rm = T)
yr_st <- aggregate(tot_lbs ~ FISHING_YEAR + ST_ABRV, data = com, sum, na.rm = T)
st_abrv <- c('TX','LA','MS','AL','FL')

plot(yr_st$FISHING_YEAR, yr_st$tot_lbs/1e6, typ = 'n')
for(i in st_abrv){
  points(yr_st$FISHING_YEAR[yr_st$ST_ABRV==i],
         yr_st$tot_lbs[yr_st$ST_ABRV==i]/1e6,
         typ = 'o', pch = 16, col = which(st_abrv==i))
}

reshaped_lbs_st <- reshape(yr_st, idvar = 'FISHING_YEAR', timevar = 'ST_ABRV', direction = 'wide')
reshaped_lbs_st[is.na(reshaped_lbs_st)] <- 0
reshaped_lbs_st <- reshaped_lbs_st[,c(1,6,4,5,2,3)]
yr_pro <- t(as.matrix(reshaped_lbs_st[,2:6]/tot_lbs$tot_lbs))

cols <- c('orangered1','gold','gray','cornflowerblue','purple4')

setwd("~/R_projects/King-Mackerel-ESP/figures/plots")
png('kgm_landings_com.png',
    width = 7, height = 6, units = 'in', res = 300, pointsize = 12)
par(mar = c(5,5,1,5),mfrow=c(2,1))

b <- barplot(tot_lbs$tot_lbs/1e6, names.arg = tot_lbs$FISHING_YEAR, las = 2,
        ylab = 'Total Landings \n(million lbs)',
        ylim = c(0, max(acls$commercial[which(acls$fishing_year %in% 2000:2024)])))
lines(c(b-.6,b[length(b)]), 
      c(acls$commercial[which(acls$fishing_year %in% 2000:2024)], 
        acls$commercial[which(acls$fishing_year %in% 2000:2024)][length(b)]),
      lwd = 2,
      typ = 's')

b2 <- barplot(yr_pro, beside = F, names.arg = tot_lbs$FISHING_YEAR, 
        las = 2, col = cols,
        ylab = 'Landings by State', xlab = 'Fishing Year', yaxt = 'n')
axis(2,seq(0,1,0.2),labels = paste0(seq(0,100,20),'%'), las = 2)
legend(b2[25]+.5,1, rev(st_abrv), 
       fill = rev(cols), xpd = T, bty = 'n')

dev.off()
