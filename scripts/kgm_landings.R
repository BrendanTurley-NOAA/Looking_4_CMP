
library(dplyr)
library(here)
library(readxl)
library(sf)
library(scatterpie)
library(ggplot2)
library(terra)
library(lubridate)
library(cmocean)


#### read data and subset ####--------------------------------------------------
gom_st <- c('FL', 'AL', 'MS', 'LA', 'TX') #|> sort()

setwd("C:/Users/brendan.turley/Documents/CMP/data/cflp")
cflp <- readRDS('CFLPblake.rds')
cflp <- subset(cflp, LAND_YEAR>1998 & CATCH_TYPE == 'CATCH') |>
  subset(REGION == 'GOM' & is.element(ST_ABRV, gom_st)) #|>
  # subset(AREA_FISHED!='1' & AREA_FISHED!='2' & !is.na(AREA_FISHED)) |>
  # filter(FLAG_GEAR == 0,
  #        FLAG_MULTIGEAR == 0,
  #        FLAG_MULTIAREA == 0,
  #        FLAG_MULTIREGION == 0,
  #        FLAG_REPORT == 0) |>
  # filter(FISHED/DAYS_AWAY<24) |>
  # filter(EFFORT >= 1)
gc()

### add fishing year
cflp$fish_yr <- ifelse(cflp$LAND_MONTH < 7, cflp$LAND_YEAR - 1, cflp$LAND_YEAR)


### total landings and by state
tot_lbs <- aggregate(TOTAL_WHOLE_POUNDS ~ fish_yr,
                     data = subset(cflp, COMMON_NAME=='MACKERELS, KING AND CERO' &
                                     fish_yr>1998 & fish_yr<2024),
                     sum, na.rm = T)

tot_lbs_st <- aggregate(TOTAL_WHOLE_POUNDS ~ fish_yr + ST_ABRV,
                        data = subset(cflp, COMMON_NAME=='MACKERELS, KING AND CERO' &
                                        fish_yr>1998 & fish_yr<2024),
                        sum, na.rm = T)

plot(tot_lbs$fish_yr, tot_lbs$TOTAL_WHOLE_POUNDS/1e6, typ = 'o', pch = 16,
     xlab = 'Year', ylab = 'Total Landings (x1,000,000 lbs)',
     ylim = c(0, max(tot_lbs$TOTAL_WHOLE_POUNDS/1e6)))

barplot(tot_lbs$TOTAL_WHOLE_POUNDS/1e6, names.arg = tot_lbs$fish_yr, las = 2,
        xlab = 'Year', ylab = 'Total Landings (x1,000,000 lbs)',
        col = 'gray50')

plot(tot_lbs_st$fish_yr, tot_lbs_st$TOTAL_WHOLE_POUNDS/1e6, typ = 'n',
     xlab = 'Year', ylab = 'Total Landings (x1,000,000 lbs)')
for(i in gom_st){
  lines(tot_lbs_st$fish_yr[tot_lbs_st$ST_ABRV==i],
        tot_lbs_st$TOTAL_WHOLE_POUNDS[tot_lbs_st$ST_ABRV==i]/1e6,
        typ = 'o', pch = 16, col = which(gom_st==i))
}

### make stacked barplots as proportion per year

reshaped_lbs_st <- reshape(tot_lbs_st, idvar = 'fish_yr', timevar = 'ST_ABRV', direction = 'wide')
reshaped_lbs_st[is.na(reshaped_lbs_st)] <- 0
yr_pro <- t(as.matrix(reshaped_lbs_st[,2:6]/tot_lbs$TOTAL_WHOLE_POUNDS))
# yr_pro <- yr_pro[order(gom_st),]

cols <- cmocean('deep', direction = 1)(length(gom_st))
# cols <- cmocean('phase', direction = 1)(length(gom_st)+1)[-1]

setwd("~/R_projects/King-Mackerel-ESP/figures/plots")
png('kgm_landings.png',
    width = 7, height = 6, units = 'in', res = 300, pointsize = 11)
par(mar = c(5,5,1,4),mfrow=c(2,1))
barplot(tot_lbs$TOTAL_WHOLE_POUNDS/1e6, names.arg = tot_lbs$fish_yr, las = 2,
        xlab = 'Year', ylab = 'Total Landings \n(x1,000,000 lbs)',
        col = 'gray')
barplot(yr_pro, beside = F, names.arg = tot_lbs$fish_yr, las = 2, col = cols,
        ylab = 'Landings by State', xlab = 'Year', yaxt = 'n')
axis(2,seq(0,1,0.2),labels = paste0(seq(0,100,20),'%'), las = 2)
legend('topright', inset = c(-.1,0) , rev(gom_st[order(gom_st)]), 
       fill = rev(cols), xpd = T, bty = 'n')
dev.off()


# ### load data ------------------------
# setwd("C:/Users/brendan.turley/Documents/R_projects/Fishing-Community-Resilience/data")
# # dat <- read.csv('lbcw_gom_combined_cleaned.csv')
# dat <- read.csv('lbcw_gom_combined_cleaned_v03.csv')
# dat <- dat[which(dat$shore.Adjacent == 1), ]
# ### dealers overtime
# kmk_dat <- subset(dat, Species.ITIS=='172435')
# 
# ### total landings and by state
# tot_lbs <- aggregate(Landed.Lbs ~ Year,
#                      data = kmk_dat,
#                      sum, na.rm = T)
# 
# tot_lbs_st <- aggregate(Landed.Lbs ~ Year + LandingState,
#                         data = kmk_dat,
#                         sum, na.rm = T)
# 
# plot(tot_lbs$Year, tot_lbs$Landed.Lbs/1e5, typ = 'o', pch = 16,
#      xlab = 'Year', ylab = 'Total Landings (x100,000 lbs)',
#      ylim = c(0, max(tot_lbs$Landed.Lbs/1e5)))
# 
# barplot(tot_lbs$Landed.Lbs/1e5, names.arg = tot_lbs$Year, las = 2,
#         xlab = 'Year', ylab = 'Total Landings (x100,000 lbs)')
# 
# plot(tot_lbs_st$Year, tot_lbs_st$Landed.Lbs/1e5, typ = 'n',
#      xlab = 'Year', ylab = 'Total Landings (x100,000 lbs)')
# for(i in gom_st){
#   lines(tot_lbs_st$Year[tot_lbs_st$LandingState==i],
#         tot_lbs_st$Landed.Lbs[tot_lbs_st$LandingState==i]/1e5,
#         typ = 'o', pch = 16, col = which(gom_st==i))
# }
# 
# ### make stacked barplots as proportion per year
# 
# reshaped_lbs_st <- reshape(tot_lbs_st, idvar = 'Year', timevar = 'LandingState', direction = 'wide')
# reshaped_lbs_st[is.na(reshaped_lbs_st)] <- 0
# yr_pro <- t(as.matrix(reshaped_lbs_st[,2:6]/tot_lbs$Landed.Lbs))
# 
# par(mar = c(5,5,1,4))
# barplot(yr_pro, beside = F, names.arg = tot_lbs$Year, las = 2, col = 1:5,
#         ylab = 'Proportion of Landings by State', xlab = 'Year')
# legend('topright', inset = c(-.08,0) ,gom_st, fill = 1:5, xpd = T, bty = 'n')
# 
