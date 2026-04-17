
### FAO EDA
setwd("C:/Users/brendan.turley/Documents/CMP/data/FAO/GlobalProduction_2025.1.0")
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
table(kmk_wco$Name_En, kmk_wco$PRODUCTION_SOURCE_DET.CODE)

setwd("~/R_projects/Looking_4_CMP/figs")
png('FAO_US_MX.png', width = 8, height = 6, units = 'in', res = 300)
with(subset(kmk_wco, Name_En=='Mexico' & AREA.CODE==31),
     plot(PERIOD, VALUE, typ = 'o', pch = 16,
          panel.first = grid(),
          las = 1, xlab = 'Year', ylab = 'FAO Commercial landings (MT)',
          main = 'King Mackerel'))
with(subset(kmk_wco, Name_En=='United States of America' & AREA.CODE==31),
     points(PERIOD, VALUE, typ = 'o', pch = 16, col = 2))
legend('topleft',c('US (Gulf & SE)','Mexico'),pch=16,col=c(2,1), bty = 'n')
dev.off()

us <- subset(kmk_wco, Name_En=='United States of America' & AREA.CODE==31,
       select = c(PERIOD, VALUE))
mx <- subset(kmk_wco, Name_En=='Mexico' & AREA.CODE==31,
       select = c(PERIOD, VALUE))
usmx_merge <- merge(us, mx, by = 'PERIOD')

plot(usmx_merge$PERIOD ,usmx_merge$VALUE.y / usmx_merge$VALUE.x, typ = 'o',
     panel.first = abline(h = 1, lty = 5))

kmk_usmx <- subset(kmk_wco, Name_En=='Mexico' & AREA.CODE==31 |
                     Name_En=='United States of America' & AREA.CODE==31)

kmk_countries <- sort(unique(kmk_wco$Name_En))

for(i in kmk_countries){
  with(subset(kmk_wco, Name_En==i),
       plot(PERIOD, VALUE, typ = 'o', lwd = 2, col = AREA.CODE))
  abline(v = seq(1950,2020,10), h = 0, lty = 5, col = 'gray40')
  mtext(i)
}


with(subset(kmk_wco, Name_En=='United States of America' & AREA.CODE==31),
     plot(PERIOD, VALUE, typ = 'o', lwd = 2, col = AREA.CODE, ylim = c(0, max(VALUE))))
with(subset(kmk_wco, Name_En=='United States of America' & AREA.CODE==21),
     points(PERIOD, VALUE, typ = 'o', lwd = 2, col = AREA.CODE))
legend('topleft',c('Gulf','SoAtl'),pch=1,col=c(31,21),lwd=2)
