
library(cmocean)
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
kmk_laa$Macro_Sex <- toupper(kmk_laa$Macro_Sex)
table(kmk_laa$Fishery, kmk_laa$Gear_Group_Code)
table(kmk_laa$Fishing_Mode, kmk_laa$Fishery)
table(kmk_laa$Fishing_Mode, kmk_laa$Gear_Group_Code)

kmk_laa <- subset(kmk_laa, Stock == 'Gulf of America') |>
  filter(Mackerel_State != 'NL',
         Mackerel_State != 'UN',
         Mackerel_State != 'MEX') |>
  subset(Gear_Group_Code == 'HL')
  

n_sample <- table(kmk_laa$Year, kmk_laa$Final_Age) |> as.data.frame.matrix()

kmk_laa_s <- subset(kmk_laa, Final_Age>=2 & Final_Age<=8) |>
    subset(Macro_Sex == 'F' | Macro_Sex == 'M') 

# kmk_laa_s <- kmk_laa_s |>
#   group_by(Final_Age)|>
#   filter(Final_Length_mm >= quantile(Final_Length_mm,.0025,na.rm=T),
#          Final_Length_mm <= quantile(Final_Length_mm,.9975,na.rm=T))|>
#   ungroup()

mlaa <- aggregate(Final_Length_mm ~ Year + Final_Age,
                  data = kmk_laa_s, FUN = mean, na.rm = T) |>
  merge(expand.grid(Year = sort(unique(kmk_laa$Year)),
              Final_Age = sort(unique(kmk_laa$Final_Age))),
        by = c('Year','Final_Age'), all = T)
ages <- sort(unique(mlaa$Final_Age))

plot(mlaa$Year, mlaa$Final_Length_mm, typ = 'n')
for(i in ages){
  points(mlaa$Year[mlaa$Final_Age==i],
         mlaa$Final_Length_mm[mlaa$Final_Age==i],
         typ = 'o', col = i, lwd = 2)  
}

b <- boxplot(Final_Length_mm ~ Year, data = kmk_laa_s, 
             pch = 16, lty = 1, varwidth = T, staplewex = 0, lwd = 2, outline = T)
for(i in 2:8){
  points(which(mlaa$Year[mlaa$Final_Age==i] %in% b$names),
         mlaa$Final_Length_mm[mlaa$Final_Age==i],
         typ = 'l', col = i, lwd = 2)  
}


### proportion of samples from region
cols <- colorRampPalette(c('orangered1','gold','gray','cornflowerblue','purple4'))(8)
# cols <- colorRampPalette(c('gold','orangered1','magenta2','purple4','cornflowerblue'))(8) |> rev()
# cols <- cmocean('haline', direction = 1)(8)

yr_st <- table(kmk_laa$Year, kmk_laa$Mackerel_State) |> 
  as.data.frame.matrix()
yr_sums <- rowSums(yr_st)
yr_st_p <- sweep(yr_st, 1, FUN = '/', yr_sums)

par(mar = c(5,5,1,4))
barplot(t(yr_st_p), las = 2, col = cols, ylab = 'Proportion of Samples', xlab = 'Year')
legend('topright', inset = c(-.07,0), xpd = T,
       legend = colnames(yr_st_p), fill = cols, cex = .8)


