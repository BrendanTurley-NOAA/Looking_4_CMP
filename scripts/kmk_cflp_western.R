
### look at TX and LA landings to look for a northward progression


natl_st <- c('ME', 'NH', 'MA', 'RI', 'CT', 'NY', 'NJ', 'DE', 'MD', 'VA')
satl_st <- c('FL', 'GA', 'SC', 'NC')
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


cflp_hl$cnty_st <- paste(cflp_hl$CNTY_FIPS_NAME, cflp_hl$ST_ABRV)
distinct(cflp_hl, CNTY_FIPS_NAME, ST_ABRV) |> View()
unique(cflp_hl$cnty_st)

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
  summarise(tot_kg = mean(tot_kg, na.rm = T)) |>
  arrange(desc(tot_kg)) |>
  print(n=61)


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


aggregate(tot_kg ~ LAND_MONTH,
          data = tx_kmk,
          sum, na.rm = T) |> plot(typ = 'o')
aggregate(tot_kg ~ LAND_MONTH,
          data = tx_kmk,
          median, na.rm = T) |> plot(typ = 'o')

aggregate(tot_kg ~ LAND_YEAR,
          data = tx_kmk,
          sum, na.rm = T) |> plot(typ = 'o')

aggregate(tot_kg ~ cnty_st + LAND_MONTH,
          data = tx_kmk,
          median, na.rm = T) |> View()

aggregate(tot_kg ~ cnty_st + LAND_MONTH,
          data = tx_kmk,
          sum, na.rm = T) |> View()

tx_cnty_seq <- c('CAMERON', 'WILLACY', 'NUECES', 'SAN PATRICIO', 'CALHOUN',
                 'BRAZORIA', 'GALVESTON', 'HARRIS', 'CHAMBERS') |> rev()

tx_mth <- aggregate(tot_kg ~ CNTY_FIPS_NAME + LAND_MONTH,
                    data = tx_kmk,
                    sum, na.rm = T)

par(mfrow = c(9,1), mar = c(2,3,2,1))
for(i in 1:length(tx_cnty_seq)){
  tmp <- subset(tx_mth, CNTY_FIPS_NAME==tx_cnty_seq[i]) |> 
    merge(data.frame(LAND_MONTH = 1:12), all = T)
  b <- barplot(tmp$tot_kg, names = 1:12, las = 1)
  abline(h=0)
  text(b[1,], max(tmp$tot_kg, na.rm = T)*.75, tx_cnty_seq[i])
}

tx_mth <- aggregate(cpue ~ CNTY_FIPS_NAME + LAND_MONTH,
                    data = tx_kmk,
                    median, na.rm = T)

par(mfrow = c(9,1), mar = c(2,3,2,1))
for(i in 1:length(tx_cnty_seq)){
  tmp <- subset(tx_mth, CNTY_FIPS_NAME==tx_cnty_seq[i]) |> 
    merge(data.frame(LAND_MONTH = 1:12), all = T)
  b <- barplot(tmp$cpue, names = 1:12, las = 1)
  abline(h=0)
  text(b[1,], max(tmp$cpue, na.rm = T)*.75, tx_cnty_seq[i])
}


### pull texas counties

latx_kmk <- subset(cflp_hl, COMMON_NAME=='MACKERELS, KING AND CERO' &
                   REGION=='GOM') |>
  subset(ST_ABRV=='TX' |  ST_ABRV=='LA')
distinct(latx_kmk, CNTY_FIPS_NAME, ST_ABRV) |> View()

latx_cnty_seq <- c('CAMERON', 'WILLACY', 'NUECES', 'SAN PATRICIO', 'CALHOUN',
                 'BRAZORIA', 'GALVESTON', 'HARRIS', 'CHAMBERS',
                 'CAMERON', 'CALCASIEU','JEFFERSON DAVIS', 'VERMILION', 'LAFAYETTE',
                 'IBERIA', 'ST MARY', 'IBERVILLE','TERREBONNE', 'LAFOURCHE', 
                 'JEFFERSON', 'ORLEANS','ST BERNARD','PLAQUEMINES') |> rev()

latx_mth <- aggregate(cpue ~ CNTY_FIPS_NAME + LAND_MONTH,
                    data = latx_kmk,
                    median, na.rm = T)

par(mfrow = c(9,1), mar = c(2,3,2,1))
for(i in 1:length(latx_cnty_seq)){
  tmp <- subset(latx_mth, CNTY_FIPS_NAME==latx_cnty_seq[i]) |> 
    merge(data.frame(LAND_MONTH = 1:12), all = T)
  b <- barplot(tmp$cpue, names = 1:12, las = 1)
  abline(h=0)
  text(b[1,], max(tmp$cpue, na.rm = T)*.75, latx_cnty_seq[i])
}

latx_cnty_seq <- c('CAMERON TX', 'WILLACY TX', 'NUECES TX', 'SAN PATRICIO TX', 'CALHOUN TX',
                   'BRAZORIA TX', 'GALVESTON TX', 'HARRIS TX', 'CHAMBERS TX',
                   'CAMERON LA', 'CALCASIEU LA','JEFFERSON DAVIS LA', 'VERMILION LA', 'LAFAYETTE LA',
                   'IBERIA LA', 'ST MARY LA', 'IBERVILLE LA','TERREBONNE LA', 'LAFOURCHE LA', 
                   'JEFFERSON LA', 'ORLEANS LA','ST BERNARD LA','PLAQUEMINES LA') |> rev()
latx_mth <- aggregate(cpue ~ cnty_st + LAND_MONTH,
                      data = latx_kmk,
                      median, na.rm = T)

par(mfrow = c(9,1), mar = c(2,3,2,1))
for(i in 1:length(latx_cnty_seq)){
  tmp <- subset(latx_mth, cnty_st==latx_cnty_seq[i]) |> 
    merge(data.frame(LAND_MONTH = 1:12), all = T)
  b <- barplot(tmp$cpue, names = 1:12, las = 1)
  abline(h=0)
  text(b[1,], max(tmp$cpue, na.rm = T)*.75, latx_cnty_seq[i])
}

barplot()

