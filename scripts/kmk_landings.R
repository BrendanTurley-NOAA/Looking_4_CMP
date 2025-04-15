

setwd("C:/Users/brendan.turley/Documents/CMP/data")

kmk_ad15 <- read.table('kmk_ad15_landings.txt',header = T)
kmk_ad15$year <- 2002:2023
kmk_ad15[is.na(kmk_ad15)] <- 0

matplot(kmk_ad15[,-1], typ = 'l')

plot(2001:2022, kmk_ad15$Northern.HL+kmk_ad15$East.F.HL, typ = 'l')
for(i in 4:6){
  points(2001:2022, kmk_ad15[,i], col = i, typ = 'l')
}

apply(kmk_ad15[,c(2:6)],1,sum)

kmk_land <- read.csv('Comm_Land_YrStateSpecies.csv')
kmk_land$pr_lb <- kmk_land$Dollars / kmk_land$Live.Lbs

gom_kmk <- subset(kmk_land, Region=='GULF OF MEXICO')
plot(gom_kmk$Year, gom_kmk$pr_lb, 
     col = as.factor(gom_kmk$State), pch = as.numeric(as.factor(gom_kmk$State)))
legend('topleft',levels(as.factor(gom_kmk$State)), col = 1:5, pch = 1:5)

yr_st <- aggregate(Live.Lbs ~ Year + State, data = gom_kmk, sum, na.rm = T)
yr_all <- aggregate(Live.Lbs ~ Year, data = gom_kmk, sum, na.rm = T)


png('kmk_landings_yr.png', width = 8, height = 5, res = 300, units = 'in')
par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(yr_st$Year, yr_st$Live.Lbs/1e6, typ = 'n',las=1,
     xlab = 'Year', ylab = 'Landings (x1,000,000 lbs.)')
state <- sort(unique(yr_st$State))
n = 1
for(i in state){
  with(subset(yr_st, State==i), {
    lines(Year, Live.Lbs/1e6, col = n, lwd = 2, pch = n + 15, typ = 'o')
    # abline(lm(Live.Lbs/10000 ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
dev.off()

for(i in state){
  print(i)
  with(subset(yr_st, State==i),
       print(summary(lm(Live.Lbs ~ Year))))
}

plot(yr_all$Year, yr_all$Live.Lbs/10000, typ = 'o', pch =16, lwd = 2, las = 1)

# plot(kmk_ad15$year, kmk_ad15$Com.Landings, typ = 'o')
b <- barplot(kmk_ad15$Com.Landings, names = kmk_ad15$year, las = 2)
lines(b,kmk_ad15$Com.ACL, lwd = 3, lty = 5)

barplot(t(kmk_ad15[,2:6]), beside=F, col = 2:6)


### FOSS
### Atlantic

setwd("C:/Users/brendan.turley/Documents/CMP/data/landings")
atl_land <- read.csv('KMK_USATL_FOSS_landings_2.csv', skip = 1)

atl_land$Pounds <- as.numeric(gsub(',', '', atl_land$Pounds))
atl_land$Metric.Tons <- as.numeric(gsub(',', '', atl_land$Metric.Tons))
atl_land$Dollars <- as.numeric(gsub(',', '', atl_land$Dollars))

aggregate(Pounds ~ NMFS.Name + State, data = atl_land, sum)

atl_agg_all <- aggregate(Pounds ~ Year + Collection, data = atl_land, sum, na.rm = T)

plot(atl_agg_all$Year, atl_agg_all$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
axis(1, seq(1950,2020,20))
grid()
with(subset(atl_agg_all, Collection=='Commercial'), 
     points(Year, Pounds, col = 2, pch = 16, typ = 'o'))
with(subset(atl_agg_all, Collection=='Recreational'), 
     points(Year, Pounds, col = 3, pch = 16, typ = 'o'))
legend('topleft', c('Commercial', 'Recreational'), col = 2:3, pch = 16)
abline(lm(Pounds ~ Year, data = subset(atl_agg_all, Collection=='Commercial')), col = 2)
abline(lm(Pounds ~ Year, data = subset(atl_agg_all, Collection=='Recreational')), col = 3)
mtext('Atlantic Com + Rec', side = 3, adj = 0.5)

state_agg <- aggregate(Pounds ~ State, data = atl_land, sum, na.rm = T)
# barplot(state_agg$Pounds, names.arg = state_agg$State, las = 2)

atl_agg <- aggregate(Pounds ~ Year + State, data = atl_land, sum, na.rm = T)

plot(atl_agg$Year, atl_agg$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
axis(1, seq(1950,2020,20))
grid()
state <- sort(unique(atl_agg$State))[c(3:4,9,11)]
pts <- rep(c(1,2,3,4,5)+15,3)
n = 1
for(i in state){
  with(subset(atl_agg, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
mtext('Atlantic Com + Rec', side = 3, adj = 0.5)


plot(atl_agg$Year, atl_agg$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)', ylim = c(0, 4e5))
axis(1, seq(1950,2020,20))
grid()
state <- sort(unique(atl_agg$State))[-c(3:4,9,11)]
pts <- rep(1:8+15,3)
n = 1
for(i in state){
  with(subset(atl_agg, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:8+15, col = 1:8)
mtext('Atlantic Com + Rec', side = 3, adj = 0.5)

atl_agg <- aggregate(Pounds ~ Year + State + Collection, data = atl_land, sum, na.rm = T)
atl_agg_com <- subset(atl_agg, Collection=='Commercial')
atl_agg_rec <- subset(atl_agg, Collection=='Recreational')


plot(atl_agg_com$Year, atl_agg_com$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
axis(1, seq(1950,2020,20))
grid()
state <- sort(unique(atl_agg_com$State))
state <- sort(unique(atl_agg_com$State))[c(3:4,9,11)]
pts <- rep(c(1,2,3,4,5)+15,3)
n = 1
for(i in state){
  with(subset(atl_agg_com, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
mtext('Atlantic Com', side = 3, adj = 0.5)


plot(atl_agg_com$Year, atl_agg_com$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)', ylim = c(0,5e4))
axis(1, seq(1950,2020,20))
grid()
state <- sort(unique(atl_agg_com$State))[-c(3:4,9,11)]
pts <- rep(1:8+15,3)
n = 1
for(i in state){
  with(subset(atl_agg_com, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topright',state, pch = 1:8+15, col = 1:8)
mtext('Atlantic Com', side = 3, adj = 0.5)


plot(atl_agg_rec$Year, atl_agg_rec$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
axis(1, seq(1950,2020,20))
grid()
state <- sort(unique(atl_agg_rec$State))[c(2:3,8,10)]
pts <- rep(c(1,2,3,4,5)+15,3)
n = 1
for(i in state){
  with(subset(atl_agg_rec, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
mtext('Atlantic Rec', side = 3, adj = 0.5)


plot(atl_agg_rec$Year, atl_agg_rec$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)', ylim = c(0,4e5))
axis(1, seq(1950,2020,20))
grid()
state <- sort(unique(atl_agg_rec$State))[-c(2:3,8,10)]
pts <- rep(1:7+15,3)
n = 1
for(i in state){
  with(subset(atl_agg_rec, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:7+15, col = 1:7)
mtext('Atlantic Rec', side = 3, adj = 0.5)


### GULF

setwd("C:/Users/brendan.turley/Documents/CMP/data/landings")
gulf_land <- read.csv('KMK_USGulf_FOSS_landings_2.csv', skip = 1)

gulf_land$Pounds <- as.numeric(gsub(',', '', gulf_land$Pounds))
gulf_land$Metric.Tons <- as.numeric(gsub(',', '', gulf_land$Metric.Tons))
gulf_land$Dollars <- as.numeric(gsub(',', '', gulf_land$Dollars))

aggregate(Pounds ~ NMFS.Name + State, data = gulf_land, sum)

state_agg <- aggregate(Pounds ~ State, data = gulf_land, sum, na.rm = T)
barplot(state_agg$Pounds, names.arg = state_agg$State, las = 2)

gulf_agg_all <- aggregate(Pounds ~ Year + Collection, data = gulf_land, sum, na.rm = T)

plot(gulf_agg_all$Year, gulf_agg_all$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
with(subset(gulf_agg_all, Collection=='Commercial'), 
     points(Year, Pounds, col = 2, pch = 16, typ = 'o'))
with(subset(gulf_agg_all, Collection=='Recreational'), 
     points(Year, Pounds, col = 3, pch = 16, typ = 'o'))
legend('topleft', c('Commercial', 'Recreational'), col = 2:3, pch = 16)
abline(lm(Pounds ~ Year, data = subset(gulf_agg_all, Collection=='Commercial')), col = 2)
abline(lm(Pounds ~ Year, data = subset(gulf_agg_all, Collection=='Recreational')), col = 3)


gulf_agg <- aggregate(Pounds ~ Year + State, data = gulf_land, sum, na.rm = T)

plot(gulf_agg$Year, gulf_agg$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
state <- sort(unique(gulf_agg$State))
pts <- rep(c(1,2,3,4,5)+15,3)
n = 1
for(i in state){
  with(subset(gulf_agg, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
mtext('Gulf Com + Rec', side = 3, adj = 0.5)


gulf_agg <- aggregate(Pounds ~ Year + State + Collection, data = gulf_land, sum, na.rm = T)
gulf_agg_com <- subset(gulf_agg, Collection=='Commercial')
gulf_agg_rec <- subset(gulf_agg, Collection=='Recreational')


plot(gulf_agg_com$Year, gulf_agg_com$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
state <- sort(unique(gulf_agg_com$State))
pts <- rep(c(1,2,3,4,5)+15,3)
n = 1
for(i in state){
  with(subset(gulf_agg_com, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
mtext('Gulf Com', side = 3, adj = 0.5)

plot(gulf_agg_rec$Year, gulf_agg_rec$Pounds, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Landings (lbs.)')
state <- sort(unique(gulf_agg_rec$State))
pts <- rep(c(1,2,3,4,5)+15,3)
n = 1
for(i in state){
  with(subset(gulf_agg_rec, State==i), {
    lines(Year, Pounds, col = n, lwd = 2, pch = pts[n], typ = 'o')
    # abline(lm(Pounds ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
mtext('Gulf Rec', side = 3, adj = 0.5)


gulf_agg_com2 <- subset(gulf_agg_com, Year>1999)
# png('kmk_landings_yr.png', width = 8, height = 5, res = 300, units = 'in')
par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(gulf_agg_com2$Year, gulf_agg_com2$Pounds/1e6, typ = 'n',las=1,
     xlab = 'Year', ylab = 'Landings (x1,000,000 lbs.)')
state <- sort(unique(gulf_agg_com2$State))
n = 1
for(i in state){
  with(subset(gulf_agg_com2, State==i), {
    lines(Year, Pounds/1e6, col = n, lwd = 2, pch = n + 15, typ = 'o')
    # abline(lm(Live.Lbs/10000 ~ Year), lty=1, col = n)
  })
  n <- n + 1
}
legend('topleft',state, pch = 1:5+15, col = 1:5)
# dev.off()

