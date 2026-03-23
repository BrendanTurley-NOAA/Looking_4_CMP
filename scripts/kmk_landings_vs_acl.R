

comdat <- read.csv('commercial_landings_vs_acl.csv')
for(i in 2:8){
  comdat[,i] <- gsub(',','',comdat[,i])
}
comdat <- type.convert(comdat)

recdat <- read.csv('rec_landings_vs_acl.csv')
for(i in 2:3){
  recdat[,i] <- gsub(',','',recdat[,i])
}
recdat <- type.convert(recdat)

setwd("~/R_projects/Looking_4_CMP/figs")
png('com_rec_landings.png', width = 9, height = 9, unit = 'in', res = 300)
par(mfrow = c(2,1), mar = c(4,5,1,1))

plot(1:nrow(recdat), recdat$Landings/1e6, 
     typ = 'l', lwd = 2, 
     xaxt = 'n', las = 2,
     xlab = '', ylab = 'Recreational Landings (x 1,000,000 lbs.)',
     panel.first = grid(), ylim = c(min(recdat$Landings/1e6), max(recdat$ACL/1e6)))
points(1:nrow(recdat), recdat$ACL/1e6, typ = 'l', lwd = 2, col = 2, lty = 5)
# legend('bottomleft',c('Landings', 'ACL'), lty = c(1,5), col = c(1,2), lwd = 2)
axis(1,1:nrow(recdat), labels = F, cex.axis = .8, tck = -.05)
text(x = 1:nrow(recdat), y = par("usr")[3] - .5, cex = .7,
     labels = recdat$Fishing.Year, srt = 45, adj = 1, xpd = TRUE)

plot(1:nrow(comdat), comdat$Com.Landings/1e6, 
     typ = 'l', lwd = 2, 
     xaxt = 'n', las = 2,
     xlab = '', ylab = 'Commerical Landings (x 1,000,000 lbs.)',
     panel.first = grid())
points(1:nrow(comdat), comdat$Com.ACL/1e6, typ = 'l', lwd = 2, col = 2, lty = 5)
legend('bottomleft',c('Landings', 'ACL'), lty = c(1,5), col = c(1,2), lwd = 2)
axis(1,1:nrow(comdat), labels = F, cex.axis = .8, tck = -.05)
text(x = 1:nrow(comdat), y = par("usr")[3] - 0.2, cex = .7,
     labels = comdat$Fishing.Year, srt = 45, adj = 1, xpd = TRUE)


dev.off()
