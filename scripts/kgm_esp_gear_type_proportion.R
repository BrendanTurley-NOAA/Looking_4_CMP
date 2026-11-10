
### Share of King Mackerel Landings by Gear Type in a) 2000 and B) 2024

sh_2000 <- c(.548,.124,.321,.007)
sh_2024 <- c(.407,.305,.279,.009)
sum(sh_2000)
1-sum(sh_2024)

com <- cbind(sh_2000, sh_2024)

par(mar=c(3,4,3,7))
barplot(com, 
        las = 1,
        names.arg = c('2000','2024'),
        # main = "Comparison of Shares in 2000 and 2024", 
        ylab = "Share Proportion",
        # legend.text = c('TR','GN','HL','O'), 
        col = c(4,3,"gold","purple2"))
legend('topright', xpd = T, inset = c(-0.15, 0),
       legend = rev(c('TR','GN','HL','O')), fill = rev(c(4,3,"gold","purple2")))
