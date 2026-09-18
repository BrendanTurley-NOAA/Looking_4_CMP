
library(readxl)

setwd("~/CMP/data/kmk_lengths")

kmk_mlaa <- read_xlsx('KM_com_mlaa_8424_20260313.xlsx', sheet = 3)
year_st <- kmk_mlaa$FISHING_YEAR

kmk <- kmk_mlaa[,-c(1:3,16:28)] |> as.data.frame()
kmk[kmk==0] <- NA
kmk_n <- kmk_mlaa[,c(17:28)] |> as.data.frame()
# kmk <- kmk[-which(kmk_n<5)]

plot(year_st, kmk$lbar_12,
     ylim = range(kmk[,-1], na.rm = T),
     typ = 'n')
for(i in 2: 13){
  points(year_st, as.vector(kmk[,i]), typ = 'l', col = i, lwd = 2)
  points(year_st, as.vector(kmk[,i]), col = i, cex = kmk_n[,i-1]|>log1p(), pch = 16)
}

plot(year_st, apply(kmk, 1, mean, na.rm=T))

# 1. Calculate row sums of the sample matrix (total samples per year)
total_samples_per_year <- rowSums(kmk_n, na.rm = TRUE)

# 2. Use matrix multiplication and divide by total samples
# We transpose the age matrix or loop, but a safe vectorized approach for matching rows is:
weighted_mean_by_year <- rowSums(kmk * kmk_n, na.rm = TRUE) / total_samples_per_year
plot(year_st, weighted_mean_by_year)


kmk_mlaa <- read_xlsx('KM_com_mlaa_8424_20260313.xlsx', sheet = 2)

kmk <- kmk_mlaa[,-c(1,16:28)] |> as.data.frame()
kmk[kmk==0] <- NA

plot(kmk$FISHING_YEAR, kmk$lbar_12,
     ylim = range(kmk[,-1], na.rm = T),
     typ = 'n')
for(i in 3:14){
  points(kmk$FISHING_YEAR, as.vector(kmk[,i]), typ = 'l', col = i, lwd = 2)
}
