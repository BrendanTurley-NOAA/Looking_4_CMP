

setwd("C:/Users/brendan.turley/Downloads")

men_bio <- read.csv('fisheries_assessment.csv')
men_ind <- read.csv('fishery_data.csv')

setwd("C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/figs")
png('menhaden.png', width = 7, height = 7, units = 'in', res = 300)
par(mfrow = c(2,1), mar = c(3,6,1,1))
plot(men_ind$Year, men_ind$Gillnet,
     typ = 'o', pch = 16, lwd = 2,
     xlab = '', ylab = '',
     ylim = range(men_ind[,c(2,4)], na.rm = T),
     panel.first = grid(), las = 1)
mtext('Menhaden Abundance Index', 2, line = 4)
points(men_ind$Year, men_ind$Seine,
       typ = 'o', pch = 16, lwd = 2, col = 2)
legend('topleft', c('LA Gillnet', 'AL, GA & LA Seine'),
       col = c(1,2), pch = 16)

plot(men_bio$Year, men_bio$SSB,
     typ = 'o', pch = 16, lwd = 2, col = 4,
     xlab = '', ylab = '',
     panel.first = grid(), las = 1)
mtext('Menhaden SSB (billions of eggs)', 2, line = 4)
dev.off()