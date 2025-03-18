

library(readxl)

setwd("C:/Users/brendan.turley/Documents/CMP/data")
coast_cnty <- read_xlsx('coastline-counties-list.xlsx')
gulf_coast <- subset(coast_cnty, `COASTLINE REGION`=='Gulf of Mexico')
gulf_coast <- toupper(gulf_coast$NAME)


setwd("C:/Users/brendan.turley/Documents/CMP/data/landings")
fl_land <-read.csv('KMK_FL_landings_county.csv', skip = 9)
fl_land$effort <- fl_land$Pounds / fl_land$Trips

cnty_keep <- which(is.element(fl_land$County.Landed, gulf_coast))
flgulf_land <- fl_land[cnty_keep, ]
fl_cnty <- sort(unique(flgulf_land$County.Landed))

cnty_lb_agg <- aggregate(Pounds ~ County.Landed, data = flgulf_land, sum, na.rm = T)
cnty_lb_agg <- cnty_lb_agg[order(cnty_lb_agg$Pounds, decreasing = T), ]

cnty_tr_agg <- aggregate(Trips ~ County.Landed, data = flgulf_land, sum, na.rm = T)

cnty_agg <- merge(cnty_lb_agg, cnty_tr_agg, by = 'County.Landed')
cnty_agg$effort <- cnty_agg$Pounds/cnty_agg$Trips

plot(fl_land$Year, fl_land$Pounds, typ = 'n')
n <- 1
for(i in 1:length(fl_cnty)){
  tmp <- subset(fl_land, County.Landed==fl_cnty[i])
  
  if(is.element(unique(tmp$County.Landed), cnty_lb_agg$County.Landed[1:5])){
    cols <- n
    n <- n + 1
  } else {
    cols <- 'gray60'
  }
       points(tmp$Year, tmp$Pounds,
              typ = 'l', col = cols, lwd = 1)
}
legend('topright', sort(cnty_lb_agg$County.Landed[1:5]), lty = 1, col = 1:5)


plot(fl_land$Year, fl_land$Pounds/fl_land$Trips, typ = 'n', ylim = c(0, 5000))
for(i in 1:length(fl_cnty)){
  tmp <- subset(fl_land, County.Landed==fl_cnty[i])
  points(tmp$Year, tmp$Pounds/tmp$Trips,
         typ = 'l', col = i, lwd = 1)
}
legend('topright', sort(cnty_lb_agg$County.Landed[1:5]), lty = 1, col = 1:5)



setwd("C:/Users/brendan.turley/Documents/CMP/data/landings")
gulf_land <-read.csv('KMK_USGulf_FOSS_landings.csv', skip = 1)
gulf_land$Pounds <- as.numeric(gsub(',', '', gulf_land$Pounds))

gulf_com <- subset(gulf_land, Collection=='Commercial')
states <- unique(gulf_com$State)
# states[2] <- 'FLORIDA'

plot(gulf_com$Year, gulf_com$Pounds, typ = 'n')
for(i in 1:length(states)){
  with(subset(gulf_com, State==states[i]),
              points(Year, Pounds, typ = 'l', col = i))
}
legend('topleft', states, lty = 1, col = 1:5, bty = 'n')
