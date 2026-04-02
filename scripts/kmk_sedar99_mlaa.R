
library(readxl)

setwd("C:/Users/brendan.turley/Downloads")

kmk_mlaa <- read_xlsx('KM_com_mlaa_8424_20260313.xlsx', sheet = 3)

kmk <- kmk_mlaa[,-c(1,16:28)] |> as.data.frame()
kmk[kmk==0] <- NA

plot(kmk$FISHING_YEAR, kmk$lbar_12,
     ylim = range(kmk[,-1], na.rm = T),
     typ = 'n')
for(i in 4: 10){
  points(kmk$FISHING_YEAR, as.vector(kmk[,i]), typ = 'l', col = i, lwd = 2)
}


kmk_mlaa <- read_xlsx('KM_com_mlaa_8424_20260313.xlsx', sheet = 2)

kmk <- kmk_mlaa[,-c(1,16:28)] |> as.data.frame()
kmk[kmk==0] <- NA

plot(kmk$FISHING_YEAR, kmk$lbar_12,
     ylim = range(kmk[,-1], na.rm = T),
     typ = 'n')
for(i in 3:14){
  points(kmk$FISHING_YEAR, as.vector(kmk[,i]), typ = 'l', col = i, lwd = 2)
}
