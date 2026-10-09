
setwd("~/CMP/data/landings")

foss <- read.csv('KMK_USGulf_FOSS_landings.csv',skip=1) ### this does not have kgm and cero
foss$Pounds <- gsub(foss$Pounds, pattern = ",", replacement = "") |> as.numeric()

states <- sort(unique(foss$State))
sector <- sort(unique(foss$Collection))
# states2 <- sort(unique(foss2$State))

foss_com <- subset(foss, Collection == "Commercial")
foss_com$Pounds <- foss_com$Pounds/1e6

yr_com <- aggregate(Pounds ~ Year, data = foss_com, sum, na.rm = T)
plot(yr_com$Year, yr_com$Pounds, typ = 'o', pch = 19, 
     xlab = 'Year', ylab = 'Landings (millions of pounds)', main = 'Commercial Landings')

plot(foss_com$Year, foss_com$Pounds, typ = 'n')
for(i in states){
  points(foss_com$Year[foss_com$State==i], foss_com$Pounds[foss_com$State==i], 
         col = which(i==states), pch = 19, typ = 'o')
}
legend('topleft',states, pch = 16, col = 1:length(states), bty = 'n', cex = 0.8)

foss_rec <- subset(foss, Collection == "Recreational")
foss_rec$Pounds <- foss_rec$Pounds/1e6
plot(foss_rec$Year, foss_rec$Pounds, typ = 'n')
for(i in states){
  points(foss_rec$Year[foss_rec$State==i], foss_rec$Pounds[foss_rec$State==i], 
         col = which(i==states), pch = 19, typ = 'o')
}
legend('topright',states, pch = 16, col = 1:length(states), bty = 'n', cex = 0.8)


foss2 <- read.csv('KMK_USGulf_FOSS_landings_2.csv',skip=1)
foss2$Pounds <- gsub(foss2$Pounds, pattern = ",", replacement = "") |> as.numeric()

foss_com <- subset(foss2, Collection == "Commercial")
foss_com$Pounds <- foss_com$Pounds/1e6
com_agg <- aggregate(Pounds ~ Year + State, data = foss_com, sum, na.rm = T)

yr_com <- aggregate(Pounds ~ Year, data = com_agg, sum, na.rm = T)
plot(yr_com$Year, yr_com$Pounds, typ = 'o', pch = 19, 
     xlab = 'Year', ylab = 'Landings (millions of pounds)', main = 'Commercial Landings')
grid()

plot(com_agg$Year, com_agg$Pounds, typ = 'n',
     panel.first = grid())
for(i in states){
  points(com_agg$Year[com_agg$State==i], com_agg$Pounds[com_agg$State==i], 
         col = which(i==states), pch = 19, typ = 'o')
}
legend('topleft',states, pch = 16, col = 1:length(states), bty = 'n', cex = 0.8)

foss_rec <- subset(foss2, Collection == "Recreational")
foss_rec$Pounds <- foss_rec$Pounds/1e6
rec_agg <- aggregate(Pounds ~ Year + State, data = foss_rec, sum, na.rm = T)

yr_rec <- aggregate(Pounds ~ Year, data = rec_agg, sum, na.rm = T)
plot(yr_rec$Year, yr_rec$Pounds, typ = 'o', pch = 19, 
     xlab = 'Year', ylab = 'Landings (millions of pounds)', main = 'Recreational Landings')
grid()

plot(rec_agg$Year, rec_agg$Pounds, typ = 'n',
     panel.first = grid())
for(i in states){
  points(rec_agg$Year[rec_agg$State==i], rec_agg$Pounds[rec_agg$State==i], 
         col = which(i==states), pch = 19, typ = 'o')
}
legend('topright',states, pch = 16, col = 1:length(states), bty = 'n', cex = 0.8)



fl_mth <- read.csv('KMK_FL_landings_month.csv', skip = 9) |>
  subset(Year>1984 & Year<2026)
monthly <- aggregate(Pounds ~ Month,
                     data = fl_mth,
                    mean, na.rm = TRUE)
### switch month names to number
monthly$mth_num <- match(monthly$Month, month.name)
monthly <- monthly[order(monthly$mth_num),]
barplot(monthly$Pounds)

fl_mat <- matrix(fl_mth$Pounds, nrow = 12, byrow = FALSE)
image(1985:2025, 1:12, t(fl_mat))

### devide each column by the sum of that column
fl_mat2 <- fl_mat/colSums(fl_mat, na.rm = TRUE)
image(1985:2025, 1:12, t(fl_mat2))
