
library(lubridate)
library(readxl)

setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")

kmk_gom <- read_excel('Km_len_analysis_GoM_050322.xlsx')
kmk_gom <- as.data.frame(kmk_gom)

matplot((kmk_gom[,-1]), typ ='l', col = c(rep(1,8), rep(2,8)))
matplot(t(kmk_gom[,-1]), typ ='b', col = c(rep(1,8), rep(2,8)))

boxplot(kmk_gom$F0~kmk_gom$YEAR)


plot(kmk_gom$YEAR, seq(0,1200,length.out=32), typ = 'n')
for(i in 1:8){
       points(kmk_gom[,1] ,kmk_gom[,i+1], col = i, typ = 'o')
  print(summary(lm(kmk_gom[,i+1] ~ kmk_gom[,1])))
  print(i-1)
}

plot(kmk_gom$YEAR, kmk_gom$F7/10, typ = 'o')

apply(kmk_gom[,-1]/10,2,range)

boxplot(unlist(kmk_gom[3,2:8]))

plot(kmk_gom$YEAR, seq(0,1200,length.out=32), typ = 'n')
for(i in 1:32){
  boxplot(unlist(kmk_gom[i,2:8]), at = kmk_gom$YEAR[i], add=T)
}


dat <- read.csv('GOM_KM_LAA_241102.csv')
table(dat$YEAR, dat$SEX)
table(dat$YEAR, dat$REGION)
table(dat$YEAR, dat$MODE)
table(dat$YEAR, dat$GEAR)
table(dat$YEAR, dat$MONTH)
table(dat$MONTH, dat$REGION)

boxplot(dat$LENGTH..FL.mm. ~ dat$SEX)
boxplot(dat$LENGTH..FL.mm. ~ dat$REGION)
boxplot(dat$LENGTH..FL.mm. ~ dat$MODE)
boxplot(dat$LENGTH..FL.mm. ~ dat$MONTH)
boxplot(dat$LENGTH..FL.mm. ~ dat$YEAR)
boxplot(dat$AGE..years. ~ dat$YEAR)
boxplot(dat$AGE..years. ~ dat$SEX)
boxplot(dat$AGE..years. ~ dat$REGION)
boxplot(dat$AGE..years. ~ dat$MODE)
boxplot(dat$AGE..years. ~ dat$MONTH)

plot(dat$AGE..years., dat$LENGTH..FL.mm., col = dat$YEAR, pch = 19)
plot(dat$AGE..years., dat$LENGTH..FL.mm., col = as.factor(dat$SEX), pch = 19)

lth_yr <- aggregate(LENGTH..FL.mm. ~ YEAR, data = dat, mean)

plot(lth_yr$YEAR, lth_yr$LENGTH..FL.mm., typ = 'o')

lth_mth <- aggregate(LENGTH..FL.mm. ~ MONTH, data = dat, mean)

plot(lth_mth$MONTH, lth_mth$LENGTH..FL.mm., typ = 'o')


kmk_lth <- read_excel('KMK_LAdata.xlsx')
kmk_lth$YEAR <- year(kmk_lth$DATE)
table(kmk_lth$MODE)
table(kmk_lth$GEAR)
table(kmk_lth$REGION)
kmk_lth$REGION <- toupper(kmk_lth$REGION)
kmk_lth$SEX <- toupper(kmk_lth$SEX)

table(kmk_lth$AGE, kmk_lth$YEAR)

with(subset(kmk_lth, SEX=='MALE'),
  table(AGE, YEAR))
with(subset(kmk_lth, SEX=='FEMALE'),
     table(AGE, YEAR))


lth_yr <- aggregate(FLENGTH ~ YEAR + AGE + REGION, data = kmk_lth, mean)

plot(lth_yr$YEAR, lth_yr$FLENGTH, typ = 'o')
