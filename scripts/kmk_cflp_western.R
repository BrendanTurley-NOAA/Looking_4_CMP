
### look at TX and LA landings to look for a northward progression


# natl_st <- c('ME', 'NH', 'MA', 'RI', 'CT', 'NY', 'NJ', 'DE', 'MD', 'VA')
# satl_st <- c('FL', 'GA', 'SC', 'NC')
gom_st <- c('FL', 'AL', 'MS', 'LA', 'TX')

setwd("C:/Users/brendan.turley/Documents/CMP/data/cflp")

#### read data and subset ####--------------------------------------------------
cflp <- readRDS('CFLPkarnauskas.rds')
# cflp_ne <- subset(cflp, LAND_YEAR<2024 & LAND_YEAR>1999 & CATCH_TYPE == 'CATCH') |>
#   subset(REGION == 'NATL' & is.element(ST_ABRV, natl_st))
cflp <- subset(cflp, LAND_YEAR<2024 & LAND_YEAR>1999 & CATCH_TYPE == 'CATCH') |>
  subset(REGION == 'GOM' & is.element(ST_ABRV, gom_st))
gc()

### pull out handlines only
# table(cflp$GEAR)
# gear_keep <- c('H', 'E', 'TR')
# gear_keep <- c('TR')
# cflp_hl <- subset(cflp , is.element(cflp$GEAR, gear_keep)) |>
  # subset(FLAG_MULTIGEAR==0 & FLAG_MULTIAREA==0)
# saveRDS(cflp_hl, 'cflp_gulfsa_temp.rds')
## cflp_hl <- readRDS('cflp_gulfsa_temp.rds')
# gc()
cflp_hl <- cflp

#### CPUE calculation and days away correction ####-----------------------------
## following methods by Walter & McCarthy 2014 (1993-2013SEDAR38-DW-10)
## CPUE = total kilograms of king mackerel/(number of lines fished*number of hooks per line*total hours fished)
## total whole pounds seems most appropriate; other 2 have lots of zeros
cflp_hl$tot_kg <- cflp_hl$TOTAL_WHOLE_POUNDS / 2.205
cflp_hl$pue <- (cflp_hl$NUMGEAR * cflp_hl$EFFORT * cflp_hl$FISHED)
cflp_hl$cpue <- cflp_hl$tot_kg / cflp_hl$pue # kg catch per number of hooks X hours
cflp_hl$cpue <- ifelse(is.infinite(cflp_hl$cpue), NA, cflp_hl$cpue)

### correct days_away
diff_time <- (cflp_hl$LAND_DATE - cflp_hl$DEPART_DATE)
units(diff_time) <- 'days'
cflp_hl$days_away_corrected <- round(diff_time + 1)
rm(diff_time)
gc()
### end

cflp_hl <- subset(cflp_hl, CNTY_FIPS_NAME!='UNKNOWN')
cflp_hl$cnty_st <- paste(cflp_hl$CNTY_FIPS_NAME, cflp_hl$ST_ABRV)
# distinct(cflp_hl, CNTY_FIPS_NAME, ST_ABRV) |> View()
unique(cflp_hl$cnty_st) |> sort()

aggregate(tot_kg ~ cnty_st + REGION, 
          data = subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO' &
                          REGION=='GOM'),
          sum, na.rm = T) |>
  arrange(desc(tot_kg))
aggregate(tot_kg ~ cnty_st + LAND_YEAR, 
          data = subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO' &
                          REGION=='GOM'),
          sum, na.rm = T) |>
  group_by(cnty_st) |>
  summarise(totkg_med = median(tot_kg, na.rm = T),
            totkg_sum = sum(tot_kg, na.rm = T),
            totkg_sd = sd(tot_kg, na.rm = T)) |>
  arrange(desc(totkg_sum)) |>
  print(n=70)


kmk_yr_m <- aggregate(tot_kg ~ cnty_st + LAND_YEAR, 
                       data = subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO' &
                                       REGION=='GOM'),
                       mean, na.rm = T) |>
  group_by(cnty_st) |>
  summarise(tot_kg = mean(tot_kg, na.rm = T))

kmk_mth_m <- aggregate(tot_kg ~ cnty_st + LAND_MONTH, 
                       data = subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO' &
                                       REGION=='GOM'),
                       mean, na.rm = T) |>
  group_by(cnty_st) |>
  summarise(tot_kg = mean(tot_kg, na.rm = T))



### pull texas counties

tx_kmk <- subset(cflp_hl, ST_ABRV=='TX' &  
                   COMMON_NAME=='MACKERELS, KING AND CERO' &
                   REGION=='GOM')
distinct(tx_kmk, CNTY_FIPS_NAME, ST_ABRV) |> View()

par(mfrow = c(2,2))
aggregate(tot_kg ~ LAND_MONTH,
          data = tx_kmk,
          sum, na.rm = T) |> plot(typ = 'o')
aggregate(tot_kg ~ LAND_MONTH,
          data = tx_kmk,
          median, na.rm = T) |> plot(typ = 'o')
aggregate(tot_kg ~ LAND_MONTH,
          data = tx_kmk,
          mean, na.rm = T) |> plot(typ = 'o')
aggregate(tot_kg ~ LAND_MONTH,
          data = tx_kmk,
          sd, na.rm = T) |> plot(typ = 'o')
par(mfrow = c(2,2))
aggregate(tot_kg ~ LAND_YEAR,
          data = tx_kmk,
          sum, na.rm = T) |> plot(typ = 'o')
aggregate(tot_kg ~ LAND_YEAR,
          data = tx_kmk,
          median, na.rm = T) |> plot(typ = 'o')
aggregate(tot_kg ~ LAND_YEAR,
          data = tx_kmk,
          mean, na.rm = T) |> plot(typ = 'o')
aggregate(tot_kg ~ LAND_YEAR,
          data = tx_kmk,
          sd, na.rm = T) |> plot(typ = 'o')

# aggregate(tot_kg ~ cnty_st + LAND_MONTH,
#           data = tx_kmk,
#           median, na.rm = T) |> View()
# 
# aggregate(tot_kg ~ cnty_st + LAND_MONTH,
#           data = tx_kmk,
#           sum, na.rm = T) |> View()

tx_cnty_seq <- c('CAMERON', 'WILLACY', 'NUECES', 'SAN PATRICIO', 'CALHOUN',
                 'BRAZORIA', 'GALVESTON', 'HARRIS', 'CHAMBERS') |> rev()

tx_yr <- aggregate(tot_kg ~ CNTY_FIPS_NAME + LAND_YEAR,
                    data = tx_kmk,
                    sum, na.rm = T)
yrs <- unique(tx_yr$LAND_YEAR) |> sort()

setwd("~/R_projects/Looking_4_CMP/figs")
# png('kmk_tx_cnty_yr.png', width = 5, height = 12, units = 'in', res = 300)
# par(mfrow = c(9,1), mar = c(2,4,2,1))
png('kmk_tx_cnty_yr2.png', width = 14, height = 12, units = 'in', res = 300)
par(mfcol = c(5,2), mar = c(2,4,2,1))
for(i in 1:length(tx_cnty_seq)){
  tmp <- subset(tx_yr, CNTY_FIPS_NAME==tx_cnty_seq[i]) |> 
    merge(data.frame(LAND_YEAR = yrs), all = T)
  b <- barplot(tmp$tot_kg, names = yrs, las = 1)
  abline(h=0)
  text(b[24,], max(tmp$tot_kg, na.rm = T), tx_cnty_seq[i], pos = 2, xpd = T)
}
dev.off()


tx_mth <- aggregate(tot_kg ~ CNTY_FIPS_NAME + LAND_MONTH,
                    data = tx_kmk,
                    median, na.rm = T)

setwd("~/R_projects/Looking_4_CMP/figs")
# png('kmk_tx_cnty_mth.png', width = 5, height = 12, units = 'in', res = 300)
# par(mfrow = c(9,1), mar = c(2,4,2,1))
png('kmk_tx_cnty_mth2.png', width = 14, height = 12, units = 'in', res = 300)
par(mfcol = c(5,2), mar = c(2,4,2,1))
for(i in 1:length(tx_cnty_seq)){
  tmp <- subset(tx_mth, CNTY_FIPS_NAME==tx_cnty_seq[i]) |> 
    merge(data.frame(LAND_MONTH = 1:12), all = T)
  b <- barplot(tmp$tot_kg, names = 1:12, las = 1)
  abline(h=0)
  text(b[12,], max(tmp$tot_kg, na.rm = T), tx_cnty_seq[i], pos = 2, xpd = T)
}
dev.off()


aggregate(days_away_corrected ~ LAND_YEAR,
          data = tx_kmk,
          sum, na.rm = T) |> plot(typ = 'o')
aggregate(NUMBER_OF_CREW  ~ LAND_YEAR,
          data = tx_kmk,
          mean, na.rm = T) |> plot(typ = 'o')

aggregate(cpue ~ LAND_YEAR,
          data = tx_kmk,
          mean, na.rm = T) |> plot(typ = 'o')

aggregate(SCHEDULE_NUMBER  ~ LAND_YEAR,
          data = tx_kmk,
          function (x) length(unique(x))) |> plot(typ = 'o')

aggregate(DEPTH ~ LAND_YEAR,
          data = tx_kmk,
          min, na.rm = T) |> plot(typ = 'o')

aggregate(DEPTH ~ LAND_YEAR,
          data = tx_kmk,
          quantile, .05, na.rm = T) |> plot(typ = 'o')

boxplot(DEPTH ~ LAND_YEAR,data = tx_kmk)


trip_id <- unique(tx_kmk$SCHEDULE_NUMBER)
tx_kmk_trips <- subset(cflp_hl, SCHEDULE_NUMBER %in% trip_id)

spp_ts <- tx_kmk_trips |> group_by(COMMON_NAME, LAND_YEAR) |>
  summarise(tot_kg = sum(tot_kg, na.rm = T), .groups = "drop") |>
  as.data.frame() |>  # base R reshape prefers standard data.frames over tibbles
  reshape(
    idvar = "LAND_YEAR",   # Rows stay unique by this column
    timevar = "COMMON_NAME",   # This column gets split into new headers
    direction = "wide"       # Swaps it to wide format
  )
apply(spp_ts[,-1],2,sum,na.rm = T) |> sort()
apply(spp_ts[,-1],2,sum,na.rm=T) |> quantile(.9,na.rm=T)
spp_ts[which(is.na(spp_ts),arr.in=T)] <- 0
spp_ts <- spp_ts[order(spp_ts$LAND_YEAR),]

tx_kmk_trips |>
  group_by(COMMON_NAME, LAND_YEAR) |>
  summarise(tot_kg = sum(tot_kg, na.rm = TRUE), .groups = "drop") |>
  xtabs(tot_kg ~ LAND_YEAR + COMMON_NAME, data = _) |>
  as.data.frame.matrix()

library(vegan)
richness <- specnumber(spp_ts[,-1])
shannon_div <- diversity(spp_ts[,-1], index = "shannon")
simpson_div <- diversity(spp_ts[,-1], index = "simpson") # Returns 1 - D
inv_simpson <- diversity(spp_ts[,-1], index = "inv")     # Returns 1/D
bray_dist <- vegdist(spp_ts[,-1], method = "bray")

par(mfrow = c(2,2))
plot(spp_ts$LAND_YEAR, richness, typ = 'o')
plot(spp_ts$LAND_YEAR, shannon_div, typ = 'o')
plot(spp_ts$LAND_YEAR, simpson_div, typ = 'o')
plot(spp_ts$LAND_YEAR, inv_simpson, typ = 'o')


spp_sums <- apply(spp_ts[,-1],2,sum)
spp_ts_pca <- spp_ts[,which(spp_sums > quantile(spp_sums,.75,na.rm=T))+1]
rownames(spp_ts_pca) <- spp_ts$LAND_YEAR

pc.cr <- princomp(scale(spp_ts_pca))
summary(pc.cr)
loadings(pc.cr)  # note that blank entries are small but not zero
## The signs of the columns of the loadings are arbitrary
plot(pc.cr) # shows a screeplot.
biplot(pc.cr)
# ordiplot(pc.cr)
plot(spp_ts$LAND_YEAR, pc.cr$scores[,1], typ = 'b')
plot(spp_ts$LAND_YEAR, pc.cr$scores[,2], typ = 'b')



### pull LA counties

la_kmk <- subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO' &
                   REGION=='GOM') |>
  subset(ST_ABRV=='LA')
# distinct(la_kmk, CNTY_FIPS_NAME, ST_ABRV) |> View()

la_cnty_seq <- c('CAMERON', 'CALCASIEU','JEFFERSON DAVIS', 'VERMILION', 'LAFAYETTE',
                 'IBERIA', 'ST MARY', 'IBERVILLE','TERREBONNE', 'LAFOURCHE', 
                 'JEFFERSON', 'ORLEANS','ST BERNARD','PLAQUEMINES') |> rev()

la_yr <- aggregate(tot_kg ~ CNTY_FIPS_NAME + LAND_YEAR,
                   data = la_kmk,
                   sum, na.rm = T)
yrs <- unique(la_yr$LAND_YEAR) |> sort()

setwd("~/R_projects/Looking_4_CMP/figs")
# png('kmk_tx_cnty_yr.png', width = 5, height = 12, units = 'in', res = 300)
# par(mfrow = c(9,1), mar = c(2,4,2,1))
png('kmk_la_cnty_yr2.png', width = 14, height = 12, units = 'in', res = 300)
par(mfcol = c(7,2), mar = c(2,4,2,1))
for(i in 1:length(la_cnty_seq)){
  tmp <- subset(la_yr, CNTY_FIPS_NAME==la_cnty_seq[i]) |> 
    merge(data.frame(LAND_YEAR = yrs), all = T)
  b <- barplot(tmp$tot_kg, names = yrs, las = 1)
  abline(h=0)
  text(b[24,], max(tmp$tot_kg, na.rm = T), la_cnty_seq[i], pos = 2, xpd = T)
}
dev.off()


la_mth <- aggregate(tot_kg ~ CNTY_FIPS_NAME + LAND_MONTH,
                    data = la_kmk,
                    median, na.rm = T)

setwd("~/R_projects/Looking_4_CMP/figs")
# png('kmk_tx_cnty_mth.png', width = 5, height = 12, units = 'in', res = 300)
# par(mfrow = c(9,1), mar = c(2,4,2,1))
png('kmk_la_cnty_mth2.png', width = 14, height = 12, units = 'in', res = 300)
par(mfcol = c(7,2), mar = c(2,4,2,1))
for(i in 1:length(la_cnty_seq)){
  tmp <- subset(la_mth, CNTY_FIPS_NAME==la_cnty_seq[i]) |> 
    merge(data.frame(LAND_MONTH = 1:12), all = T)
  b <- barplot(tmp$tot_kg, names = 1:12, las = 1)
  abline(h=0)
  text(b[12,], max(tmp$tot_kg, na.rm = T), la_cnty_seq[i], pos = 2, xpd = T)
}
dev.off()


aggregate(days_away_corrected ~ LAND_YEAR,
          data = la_kmk,
          sum, na.rm = T) |> plot(typ = 'o')
aggregate(NUMBER_OF_CREW  ~ LAND_YEAR,
          data = la_kmk,
          mean, na.rm = T) |> plot(typ = 'o')

aggregate(cpue ~ LAND_YEAR,
          data = la_kmk,
          mean, na.rm = T) |> plot(typ = 'o')

aggregate(SCHEDULE_NUMBER  ~ LAND_YEAR,
          data = la_kmk,
          function (x) length(unique(x))) |> plot(typ = 'o')

aggregate(DEPTH ~ LAND_YEAR,
          data = la_kmk,
          min, na.rm = T) |> plot(typ = 'o')

aggregate(DEPTH ~ LAND_YEAR,
          data = la_kmk,
          quantile, .05, na.rm = T) |> plot(typ = 'o')

boxplot(DEPTH ~ LAND_YEAR,data = la_kmk)


latx_cnty_seq <- c('CAMERON TX', 'WILLACY TX', 'NUECES TX', 'SAN PATRICIO TX', 'CALHOUN TX',
                   'BRAZORIA TX', 'GALVESTON TX', 'HARRIS TX', 'CHAMBERS TX',
                   'CAMERON LA', 'CALCASIEU LA','JEFFERSON DAVIS LA', 'VERMILION LA', 'LAFAYETTE LA',
                   'IBERIA LA', 'ST MARY LA', 'IBERVILLE LA','TERREBONNE LA', 'LAFOURCHE LA', 
                   'JEFFERSON LA', 'ORLEANS LA','ST BERNARD LA','PLAQUEMINES LA') |> rev()
latx_mth <- aggregate(tot_kg ~ cnty_st + LAND_MONTH,
                      data = latx_kmk,
                      median, na.rm = T)

par(mfrow = c(9,1), mar = c(2,3,2,1))
for(i in 1:length(latx_cnty_seq)){
  tmp <- subset(latx_mth, cnty_st==latx_cnty_seq[i]) |> 
    merge(data.frame(LAND_MONTH = 1:12), all = T)
  b <- barplot(tmp$tot_kg, names = 1:12, las = 1)
  abline(h=0)
  text(b[1,], max(tmp$tot_kg, na.rm = T)*.75, latx_cnty_seq[i])
}

barplot()

