
### FAO EDA

dat <- read.csv('Global_production_quantity.csv')
countries <- read.csv('CL_FI_COUNTRY_GROUPS.csv')
status_codes <- read.csv('CL_FI_SYMBOL_SDMX.csv')

kmk <- subset(dat, SPECIES.ALPHA_3_CODE=='KGM')

table(kmk$MEASURE) # tonnes - live wieght, multiplier = 1
table(kmk$AREA.CODE)
table(kmk$COUNTRY.UN_CODE)
table(kmk$STATUS)

?match

c_short <- countries[,c(1,5)]
names(c_short)[1] <- names(kmk)[1]
kmk_wco <- merge(kmk, c_short, by = names(kmk)[1])
unique(kmk_wco$Name_En)
names(kmk_wco)

table(kmk_wco$Name_En, kmk_wco$AREA.CODE)

with(subset(kmk_wco, Name_En=='United States of America' & AREA.CODE==31),
     plot(PERIOD, VALUE, typ = 'l'))

kmk_countries <- sort(unique(kmk_wco$Name_En))

for(i in kmk_countries){
  with(subset(kmk_wco, Name_En==i),
       plot(PERIOD, VALUE, typ = 'o', lwd = 2))
  abline(v = seq(1950,2020,10), h = 0, lty = 5, col = 'gray40')
  mtext(i)
}
