
library(readxl)

setwd("~/CMP/data/kmk_lengths")
kmk_mlaa <- read_xlsx('KM_com_mlaa_8424_20260313.xlsx', sheet = 3)
year_st <- kmk_mlaa$FISHING_YEAR

kmk <- kmk_mlaa[,-c(1:3,16:28)] |> as.data.frame()
kmk[kmk==0] <- NA
kmk_n <- kmk_mlaa[,c(17:28)] |> as.data.frame()
# kmk <- kmk[-which(kmk_n<3)]

plot(year_st, kmk$lbar_8,
     ylim = range(kmk[,-1], na.rm = T),
     typ = 'n')
for(i in 2: 9){
  points(year_st, as.vector(kmk[,i]), typ = 'l', col = i, lwd = 2)
  # points(year_st, as.vector(kmk[,i]), col = i, cex = kmk_n[,i-1]|>log1p(), pch = 16)
}

plot(year_st, apply(kmk, 1, mean, na.rm=T))

# 1. Calculate row sums of the sample matrix (total samples per year)
total_samples_per_year <- rowSums(kmk_n, na.rm = TRUE)

# 2. Use matrix multiplication and divide by total samples
# We transpose the age matrix or loop, but a safe vectorized approach for matching rows is:
weighted_mean_by_year <- rowSums(kmk * kmk_n, na.rm = TRUE) / total_samples_per_year
plot(year_st, weighted_mean_by_year)

### gillnet data
# kmk_mlaa <- read_xlsx('KM_com_mlaa_8424_20260313.xlsx', sheet = 2)
# year_st <- kmk_mlaa$FISHING_YEAR
# 
# kmk <- kmk_mlaa[,-c(1:3,16:28)] |> as.data.frame()
# kmk[kmk==0] <- NA
# 
# plot(year_st, kmk$lbar_12,
#      ylim = range(kmk[,-1], na.rm = T),
#      typ = 'n')
# for(i in 3:14){
#   points(year_st, as.vector(kmk[,i]), typ = 'l', col = i, lwd = 2)
# }

### raw
kmk_laa <- read_xlsx('KMK_age_ALL_20260323C.xlsx', sheet = 1)
kmk_laa <- subset(kmk_laa, Stock == 'Gulf of America') |>
  subset(Fishery == 'COM' | Fishery == 'FI' | Fishery == 'REC') |>
  subset(Gear_Group_Code == 'HL')

n_sample <- table(kmk_laa$Year, kmk_laa$Final_Age) |> as.data.frame.matrix()

kmk_laa_s <- subset(kmk_laa, Final_Age>1 & Final_Age<9) |>
  filter(Final_Length_mm >= quantile(Final_Length_mm,.0025,na.rm=T),
         Final_Length_mm <= quantile(Final_Length_mm,.9975,na.rm=T))
mlaa <- aggregate(Final_Length_mm ~ Year + Final_Age,
                  data = kmk_laa_s, FUN = mean, na.rm = T) |>
  merge(expand.grid(Year = unique(kmk_laa_s$Year),
              Final_Age = unique(kmk_laa_s$Final_Age)),
        by = c('Year','Final_Age'), all = T)

plot(mlaa$Year, mlaa$Final_Length_mm, typ = 'n')
for(i in 2:8){
  points(mlaa$Year[mlaa$Final_Age==i],
         mlaa$Final_Length_mm[mlaa$Final_Age==i],
         typ = 'l', col = i, lwd = 2)  
}
b <- boxplot(Final_Length_mm ~ Year, data = kmk_laa_s, 
             pch = 16, lty = 1, varwidth = T, staplewex = 0, lwd = 2, outline = T)
for(i in 2:8){
  points(which(mlaa$Year[mlaa$Final_Age==i] %in% b$names),
         mlaa$Final_Length_mm[mlaa$Final_Age==i],
         typ = 'l', col = i, lwd = 2)  
}

