
library(cmocean)
library(dplyr)
library(lubridate)
library(readxl)
library(tibble)
library(timeDate)
library(lattice)
library(viridisLite)


# seasonviridisLite# season open
open <- as_tibble(data.frame(YEAR = 2012:2024, open = (ymd(holiday(year=2012:2024,  USMLKingsBirthday)) + 1)))

setwd("C:/Users/brendan.turley/Documents/CMP/data")
# season info
open_close <- read_excel('kmk_gilnet-open-close.xlsx')
open$close <- as.Date(subset(open_close, year>2011)$close)
# gillnet landings
kmk_gn_d <- read_excel('GOM_KM_GN.xlsx')
kmk_gn_d <- type.convert(kmk_gn_d)
kmk_gn_d <- merge(kmk_gn_d, open, by = c('YEAR'), all.x = T)
# remove days after closure
out_season <- ifelse(kmk_gn_d$LANDED_DATE>kmk_gn_d$close, 1, 0)
kmk_gn_d <- kmk_gn_d[-which(out_season==1),]
# The conversion factor we use for King Mackerel gutted to whole weight is 1.04
gut2whole <- 1.04
table(kmk_gn_d$WGT_TYPE)
table(kmk_gn_d$YEAR, kmk_gn_d$AREA)
### convert to gutted; Kyle said U is treated as gutted
kmk_gn_d$wt_convert <- ifelse(kmk_gn_d$WGT_TYPE=='W', kmk_gn_d$LANDED_LBS*(1/gut2whole), kmk_gn_d$LANDED_LBS)
kmk_gn_d$wt_convert2 <- ifelse(kmk_gn_d$WGT_TYPE=='G' | kmk_gn_d$WGT_TYPE=='U', kmk_gn_d$LANDED_LBS*(gut2whole), kmk_gn_d$LANDED_LBS)
kmk_gn_d$yday <- yday(kmk_gn_d$LANDED_DATE)

kmk_yearly <- aggregate(cbind(wt_convert, wt_convert2) ~ YEAR, data = kmk_gn_d, sum, na.rm = T)
barplot(kmk_yearly$wt_convert, names.arg = kmk_yearly$YEAR, las = 2)
barplot(kmk_yearly$wt_convert2, names.arg = kmk_yearly$YEAR, las = 2)

kmk_daily <- aggregate(wt_convert ~ YEAR + yday, data = kmk_gn_d, sum, na.rm = T)
kmk_daily_ves <- aggregate(VESSEL_ID ~ YEAR + yday, data = kmk_gn_d, function(x) length(unique(x)))
kmk_daily <- merge(kmk_daily, kmk_daily_ves, by = c('YEAR', 'yday'), all=T)
kmk_daily <- kmk_daily[order(kmk_daily$YEAR, kmk_daily$yday),]


missing <- data.frame(yday = 1:90)
all_d <- matrix(NA, 90, 13)
all_d_c <- matrix(NA, 90, 13)
out <- matrix(NA, 90*13, 5)

yrs <- sort(unique(kmk_daily$YEAR))
n <- 1
a <- 1
b <- 0
for(i in yrs){
  tmp <- subset(kmk_daily, YEAR==i) |>
    merge(missing, by = 'yday', all = T)
  tmp$wt_convert[is.na(tmp$wt_convert)] <- 0
  
  days_open <- tmp$yday - with(subset(open, YEAR==i), yday(open))
  b <- b + length(days_open)
  out[a:b, ] <- cbind(i, 
                      days_open, 
                      tmp$wt_convert, 
                      cumsum(tmp[,3]), 
                      cumsum(tmp[,3])/max(cumsum(tmp[,3])))
  a <- b + 1
  
  all_d[ , n] <- tmp[,3]
  all_d_c[ , n] <- cumsum(tmp[,3])
  n <- n + 1
}
out <- out[which(out[,2]>0),] |>
  as.data.frame()
names(out) <- c('year','days_open','wt_convert','cumsum', 'pro')
out <- out[which(out$days_open<60),]

# matplot(all_d, typ ='s', lty = 1)
# matplot(all_d_c, typ ='l', lty = 1)


cols <- cmocean('phase')(length(yrs))
cols <- turbo(length((yrs)))

# plot(out[,2:3], typ = 'n')
plot(0:60, seq(0,1,length.out=61),typ='n')
n <- 1
for(i in yrs){
  tmp <- subset(out, year==i)
  # lines(tmp$days_open, tmp$cumsum, col = n, typ = 'l', lwd = 2)
  lines(tmp$days_open, tmp$pro, col = cols[n], lty = 1, typ = 'l', lwd = 2)
  n <- n + 1
}

setwd("C:/Users/brendan.turley/Documents/R_projects/misc-noaa-scripts/figs")
png('kmk_gn_daily_combined.png', width = 8, height = 6, res = 300, units = 'in')
plot(0:60, seq(0,.61,length.out=61), typ='n', las = 1, 
     xlab = 'Days open', ylab = "Landings proportion")
grid()
n <- 1
for(i in yrs){
  tmp <- subset(out, year==i)
  # lines(tmp$days_open, tmp$cumsum, col = n, typ = 'l', lwd = 2)
  lines(tmp$days_open[-1], diff(tmp$pro), col = cols[n], lty = 1, typ = 'l', lwd = 2)
  n <- n + 1
}
legend('topright',legend=seq(2012,2024),lwd=2,col=cols[1:13],bty='n',cex=.7)
dev.off()

setwd("C:/Users/brendan.turley/Documents/R_projects/misc-noaa-scripts/figs")
png('kmk_gn_daily_combined2.png', width = 8, height = 6, res = 300, units = 'in')
plot(0:60, seq(0,1,length.out=61), typ='n', las = 1, 
     xlab = 'Days open', ylab = "Landings proportion")
grid()
n <- 1
for(i in yrs){
  tmp <- subset(out, year==i)
  # lines(tmp$days_open, tmp$cumsum, col = n, typ = 'l', lwd = 2)
  lines(tmp$days_open, (tmp$pro), col = cols[n], lty = 1, typ = 'l', lwd = 2)
  n <- n + 1
}
legend('bottomright',legend=seq(2012,2024),lwd=2,col=cols[1:13],bty='n',cex=.7)
dev.off()

out$year_f <- factor(out$year)
lineData <- data.frame(
  year_f = levels(out$year_f),
  height = yday(open$close) - yday(open$open)
  )

png('kmk_gn_daily.png', width = 8, height = 6, res = 300, units = 'in')
xyplot(pro ~ days_open | year_f, data = out,
       type = c('l','g'), as.table = T, lwd = 2,
       xlab = 'Days open', ylab = "Landings proportion", layout = c(4,4),
       panel = function(x, ...) {
         level <- dimnames(trellis.last.object())[["year_f"]][packet.number()]
         panel.xyplot(x, ...);
         panel.abline(v = lineData$height[lineData$year_f==level],
                      lty = 3, col = 'darkorange', lwd = 2)
         })
dev.off()

kmk_yr <- aggregate(yday ~ YEAR , data = kmk_daily, max) |>
  merge(open, by = 'YEAR')
days_open <- kmk_yr$yday - yday(kmk_yr$open)
num_vess <- aggregate(VESSEL_ID ~ YEAR , data = kmk_gn_d, function(x) length(unique(x)))
fish_days <- aggregate(wt_convert ~ year, data = out, function(x) length(which(x>0)))
daily_land <- aggregate(wt_convert ~ year, data = out, function(x) mean(x[which(x>0)]))

# kmk_ves_d <- aggregate(wt_convert ~ YEAR + yday + VESSEL_ID, data = kmk_gn_d, sum, na.em = T)
# boxplot(kmk_ves_d$wt_convert~kmk_ves_d$YEAR)


png('kmk_gn_yearly.png', width = 8, height = 5, res = 300, units = 'in')
par(mfrow = c(2,2), mar = c(4,4,1,1))

# plot(kmk_yr$YEAR, days_open, typ = 'o',
plot(kmk_yr$YEAR, lineData$height, typ = 'o',
     xlab = '', ylab = '', pch = 16, lwd = 2, las = 1)
abline(v = c(2013, 2018, 2023:2024), col = 2, lwd = 10)
title(xlab = '', ylab = 'Days open',line = 2.5)
grid()

# plot(num_vess$YEAR, num_vess$VESSEL_ID, typ = 'o',
#      xlab = '', ylab = '', pch = 16, lwd = 2, las = 1,
#      ylim = c(0,max(num_vess$VESSEL_ID)))
# grid()
barplot(num_vess$VESSEL_ID, names.arg = num_vess$YEAR, las = 2)
title(xlab = '', ylab = 'Number of vessels',line = 2.5)


plot(fish_days$year, fish_days$wt_convert, typ = 'o',
     xlab = '', ylab = '', pch = 16, lwd = 2, las = 1,
     ylim = c(0,max(fish_days$wt_convert)))
title(xlab = 'Year', ylab = 'Fished days',line = 2.5)
abline(v = 2016, col = 2, lty = 5, lwd = 2)
grid()

# boxplot(kmk_ves_d$wt_convert/1000 ~ kmk_ves_d$YEAR,
b <- boxplot(kmk_gn_d$wt_convert/1000 ~ kmk_gn_d$YEAR,
        xlab = '', ylab = '', pch = 16, lwd = 2, las = 1, lty = 1, 
        staplewex  = 0, outline = F)
title(xlab = 'Year', ylab = 'Landings per trip (x1,000 lbs)',line = 2.5)
abline(h = axTicks(2), col = "lightgray", lty = "dotted")
points(c(.5,4.5),rep(25,2), typ = 'l', lwd = 2, col = 2, lty = 2, lend = 2)
points(c(4.5,13.5),rep(45,2), typ = 'l', lwd = 2, col = 2, lty = 2, lend = 2)

# plot(daily_land$year, daily_land$wt_convert/1000, typ = 'o',
#      xlab = '', ylab = '', pch = 16, lwd = 2, las = 1)
# title(xlab = 'Year', ylab = 'Mean daily landings (x1,000 lbs)',line = 2.5)
# grid()

dev.off()



plot(kmk_yr$YEAR, fish_days$wt_convert/days_open)

plot(kmk_yr$YEAR, daily_land$wt_convert/num_vess$VESSEL_ID)

### boxplot daily landings per year

boxplot(kmk_gn_d$wt_convert~kmk_gn_d$YEAR)
boxplot(kmk_gn_d$wt_convert2~kmk_gn_d$YEAR)
quantile(kmk_gn_d$wt_convert)
