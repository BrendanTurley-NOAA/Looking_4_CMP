source('C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/scripts/kmk_convert_2fl.R')

library(dplyr)
library(lubridate)
library(readxl)
library(stringr)


setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")
dat <- read_xlsx('KMK_LAdata.xlsx')

dat$year <- year(dat$DATE)
dat$month <- month(dat$DATE)
dat$STATE <- toupper(dat$STATE)
dat$GEAR <- toupper(dat$GEAR)

table(dat$year, dat$month)
table(dat$year, dat$STATE)
table(dat$month, dat$STATE)
table(dat$MODE)
table(dat$SEX)
table(dat$GEAR)

dat2 <- subset(dat, AGE < 9 & AGE >1) |>
  subset(GEAR == 'HL')


yr_agg <- aggregate(FLENGTH ~ year + STOCK_ID2 + AGE + SEX, data = dat2, mean, na.rm = TRUE)
ages <- unique(yr_agg$AGE)
unique(yr_agg$STOCK_ID2)

par(mfrow = c(2, 2))

plot(yr_agg$year, yr_agg$FLENGTH,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages){
  with(subset(yr_agg, AGE == i & SEX =='Female' & STOCK_ID2 == 'Gulf'),
       lines(year, FLENGTH, col = i, lwd = 2))
  with(subset(yr_agg, AGE == i & SEX =='Male' & STOCK_ID2 == 'Gulf'),
       lines(year, FLENGTH, col = i, lwd = 2, lty = 3))
}
# legend('topright', legend = ages[1:8], col = ages[1:8], lty = 1)

plot(yr_agg$year, yr_agg$FLENGTH,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: SoAtl')
for(i in ages[1:8]){
  with(subset(yr_agg, AGE == i & SEX =='Female' & STOCK_ID2 == 'Atlantic'),
       lines(year, FLENGTH, col = i, lwd = 2))
  with(subset(yr_agg, AGE == i & SEX =='Male' & STOCK_ID2 == 'Atlantic'),
       lines(year, FLENGTH, col = i, lwd = 2, lty = 3))
}

plot(yr_agg$year, yr_agg$FLENGTH,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: Mixing')
for(i in ages[1:8]){
  with(subset(yr_agg, AGE == i & SEX =='Female' & STOCK_ID2 == 'Mixing'),
       lines(year, FLENGTH, col = i, lwd = 2))
  with(subset(yr_agg, AGE == i & SEX =='Male' & STOCK_ID2 == 'Mixing'),
       lines(year, FLENGTH, col = i, lwd = 2, lty = 3))
}



setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")
dat3 <- read.csv('GOM_KM_LAA_241102.csv')

dat3$age <- round(dat3$AGE..years.)

table(dat3$YEAR, dat3$MONTH)
table(dat3$YEAR, dat3$REGION)

dat4 <- subset(dat3, age < 9 & age >1)

yr_agg2 <- aggregate(LENGTH..FL.mm. ~ YEAR + age + SEX, data = dat4, mean, na.rm = TRUE)
ages2 <- unique(yr_agg2$age)

plot(yr_agg2$YEAR, yr_agg2$LENGTH..FL.mm.,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages2){
  with(subset(yr_agg2, age == i & SEX =='Female'),
       lines(YEAR, LENGTH..FL.mm., col = i, lwd = 2))
  with(subset(yr_agg2, age == i & SEX =='Male'),
       lines(YEAR, LENGTH..FL.mm., col = i, lwd = 2, lty = 3))
}
