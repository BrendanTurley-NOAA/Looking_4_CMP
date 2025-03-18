
library(lubridate)
library(readxl)

setwd("C:/Users/brendan.turley/Documents/CMP/data/head-boat")

hb_lwt <- read_excel('KM_rec_catSRHS_8622_20240126C.xlsx', sheet = 3)
hb_ndis <- read_excel('KM_rec_catSRHS_8622_20240126C.xlsx', sheet = 5)
hb_ndis <- type.convert(hb_ndis)
hb_stlwt <- as.matrix(read_excel('KM_rec_catSRHS_8622_20240126C.xlsx', sheet = 7))
hb_stlwt <- cbind(hb_stlwt[,1], hb_stlwt[,2:5]/hb_stlwt[,6])

matplot(hb_lwt$year, hb_lwt[,2:4], typ = 'l', lwd = 2, lty = 1)
legend('topleft', names(hb_lwt[2:4]), col = 1:3, lty = 1, bty = 'n', lwd = 2)

matplot(hb_ndis$year, hb_ndis[,2:4], typ = 'l', lwd = 2, lty = 1)
legend('topright', names(hb_ndis[2:4]), col = 1:3, lty = 1, bty = 'n', lwd = 2)

barplot(t(hb_stlwt[,2:5]), names.arg = hb_stlwt[,1], col = 1:4)

