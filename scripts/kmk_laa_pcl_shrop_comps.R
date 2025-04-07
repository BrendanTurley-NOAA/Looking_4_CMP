source('C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/scripts/kmk_convert_2fl.R')

library(dplyr)
library(lubridate)
library(readxl)
library(stringr)

# https://github.com/tidyverse/readxl/issues/716
# a function that takes a character vector that may contain dates in various formats, and attempts to convert each format to a date value appropriately
convert_excel_dates <-
  function(x){
    case_when(
      str_detect(x, "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$") ~ mdy(x),   # handles values imported as text values in the format "MM/DD/YYYY"
      str_detect(x, "^[0-9]{5}$")                         ~ x |> as.integer() |> as.Date(origin = as.Date("1899-12-30")),  # handles values imported as numbers expressed as days since 1899-12-30 (Microsoft's convention)
      str_detect(x, "\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\s\\d{4}\\b") ~ as.Date(paste0(x, " 1"), format = "%B %Y %d"),
      TRUE                                                ~ NA_Date_  # default case, no applicable date format, returns a missing date value
    )
  }


### import data from PCL
setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")

### SEDAR38 file
dat_38 <- read_xlsx('SEDAR38 PCLAB Age File New Mixing Zone_v2.xlsx', sheet = 1)
dat_38$date2 <- convert_excel_dates(dat_38$Date)
dat_38$month_num <- match(dat_38$Month,month.abb)
dat_38$State <- toupper(dat_38$State)
dat_38$Sex <- toupper(dat_38$Sex)

names(dat_38) <- str_replace_all(names(dat_38), ' #|#', '')
names(dat_38) <- str_replace_all(names(dat_38), ' ', '_')
names(dat_38) <- str_replace_all(names(dat_38), '\\(', '_')
names(dat_38) <- str_replace_all(names(dat_38), '\\)', '')
names(dat_38) <- tolower(names(dat_38))
dat_38$gear[which(dat_38$gear=='Hl')] = 'HL'

### SEDAR38 update file
dat_38u <- read_xlsx('2019SEDAR38UAge_Data (1).xlsx', sheet = 2)

### import data from Shropshire
setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")

### big file from shropshire
shrop1 <- read_xlsx('KMK_LAdata.xlsx')
shrop1$year <- year(shrop1$DATE)
shrop1$month <- month(shrop1$DATE)
shrop1$STATE <- toupper(shrop1$STATE)
shrop1$GEAR <- toupper(shrop1$GEAR)

### smaller file
shrop2 <- read.csv('GOM_KM_LAA_241102.csv')
shrop2$age <- round(shrop2$AGE..years.)

shrop3 <- read.csv('SA_KM_LAA_241102.csv')
shrop3$age <- round(shrop3$AGE..years.)


#### Gulf ------------------------
### SEDAR
subdat_38 <- subset(dat_38, sex == 'M' | sex == 'F') |>
  subset(final_age < 9 & final_age > 1) |>
  subset(gear == 'HL') |>
  subset(state != 'MEX') |>
  subset(stock_id_2 == 'Gulf') |>
  subset(year != 2013)

subdat_38u <- subset(dat_38u, MACRO_SEX == 'M' | MACRO_SEX == 'F') |>
  subset(CALENDAR_AGE < 9 & CALENDAR_AGE > 1) |>
  subset(GEAR_GROUP_CODE == 'HL') |>
  subset(STOCK_ID == 'GULF OF MEXICO')

### Shropshire
subshrop1 <- subset(shrop1, SEX == 'Male' | SEX == 'Female') |>
  subset(AGE < 9 & AGE > 1) |>
  subset(GEAR == 'HL') |>
  subset(STOCK_ID2 == 'Gulf') |>
  subset(year != 2013)

subshrop2 <- subset(shrop2, age < 9 & age > 1)



### by sex

### aggregate sedar data
laa_38 <- aggregate(fl_mm ~ year + final_age + sex,
                    data = subdat_38, mean, na.rm = T)
laa_38u <- aggregate(OBSERVED_FL_MM ~ CATCH_YEAR + CALENDAR_AGE + MACRO_SEX,
                     data = subdat_38u, mean, na.rm = T)
names(laa_38u) <- names(laa_38)
laa_38_all <- rbind(laa_38, laa_38u)


### aggregate shropshire data
laa_sh1 <- aggregate(FLENGTH ~ year + AGE + SEX,
                    data = subshrop1, mean, na.rm = TRUE)
laa_sh2 <- aggregate(LENGTH..FL.mm. ~ YEAR + age + SEX,
                     data = subshrop2, mean, na.rm = TRUE)
names(laa_sh2) <- names(laa_sh1) <- names(laa_38_all)
# laa_sh_all <- rbind(laa_sh1, laa_sh2)


### combine files and compare
laa_38_all$sex <- ifelse(laa_38_all$sex == 'F', 'Female', 'Male')

laa_merge1 <- merge(laa_38_all, laa_sh1, by = c('year', 'final_age', 'sex'))
laa_merge1$fl_mm.y <- laa_merge1$fl_mm.y * 10
laa_merge1$fl_mm.x <- signif(laa_merge1$fl_mm.x, 4)
laa_merge1$fl_mm.y <- signif(laa_merge1$fl_mm.y, 4)

laa_merge2 <- merge(laa_38_all, laa_sh2, by = c('year', 'final_age', 'sex'))
laa_merge2$fl_mm.x <- signif(laa_merge2$fl_mm.x, 4)
laa_merge2$fl_mm.y <- signif(laa_merge2$fl_mm.y, 4)

plot(laa_merge1$fl_mm.x, laa_merge1$fl_mm.y, asp = 1)
abline(0, 1, col = 2)

plot(laa_merge2$fl_mm.x, laa_merge2$fl_mm.y, asp = 1)
abline(0, 1, col = 2)

laa_merge1$diff <- laa_merge1$fl_mm.x - laa_merge1$fl_mm.y
laa_merge2$diff <- laa_merge2$fl_mm.x - laa_merge2$fl_mm.y

hist(laa_merge1$diff)
hist(laa_merge2$diff)

plot(sort(laa_merge1$diff),asp=1)
abline(h=0,col=2)
plot(sort(laa_merge2$diff),asp=1)
abline(h=0,col=2)

length(which(laa_merge1$diff == 0)) / nrow(laa_merge1)
length(which(laa_merge2$diff == 0)) / nrow(laa_merge2)

summary(laa_merge1$diff)
summary(laa_merge2$diff)

nrow(subdat_38)
nrow(subshrop1)
nrow(subdat_38u) + nrow(subdat_38)
nrow(subshrop2)

table(subdat_38$year)
table(subdat_38u$CATCH_YEAR)
table(subshrop1$SEX)
table(subshrop2$SEX)

table(subdat_38$sex)
table(subdat_38u$MACRO_SEX)
table(subshrop1$year)
table(subshrop2$YEAR)

table(subdat_38$mode)
table(subdat_38u$FISHING_MODE)
table(subshrop1$MODE)
table(subshrop2$MODE)

table(subdat_38$mode_2)
table(subdat_38u$FISHERY)

table(subdat_38$source)
table(subdat_38u$SOURCE_CODE)
table(subshrop1$SOURCE)
table(subshrop2$SOURCE)

table(subdat_38$gear)
table(subdat_38u$GEAR_GROUP_CODE)
table(subshrop1$GEAR)
table(subshrop2$GEAR)


setwd("C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/figs")
png('kmk_laa_comps_gom.png', width = 8, height = 9, units = 'in', res = 300)
par(mfcol = c(2, 2))

ages <- unique(laa_merge1$final_age)

plot(laa_merge1$year, laa_merge1$fl_mm.x,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
mtext('PCL File: SEDAR38')
for(i in ages){
  with(subset(laa_merge1, final_age == i & sex =='Female'),
       lines(year, fl_mm.x, col = i, lwd = 2))
  with(subset(laa_merge1, final_age == i & sex =='Male'),
       lines(year, fl_mm.x, col = i, lwd = 2, lty = 3))
}
legend ('topright', legend = ages, col = ages[1:8], lty = 1, bty = 'n', cex = .7)

plot(laa_merge1$year, laa_merge1$fl_mm.y,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
mtext('Shropshire File: KMK_LAdata.xlsx')
for(i in ages){
  with(subset(laa_merge1, final_age == i & sex =='Female'),
       lines(year, fl_mm.y, col = i, lwd = 2))
  with(subset(laa_merge1, final_age == i & sex =='Male'),
       lines(year, fl_mm.y, col = i, lwd = 2, lty = 3))
}


ages <- unique(laa_merge2$final_age)

plot(laa_merge2$year, laa_merge2$fl_mm.x,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
mtext('PCL File: SEDAR38U')
for(i in ages){
  with(subset(laa_merge2, final_age == i & sex =='Female'),
       lines(year, fl_mm.x, col = i, lwd = 2))
  with(subset(laa_merge2, final_age == i & sex =='Male'),
       lines(year, fl_mm.x, col = i, lwd = 2, lty = 3))
}
legend ('topright', legend = c('female', 'male'), col = 'gray40', lty = c(1,3), bty = 'n', cex = .7)

plot(laa_merge2$year, laa_merge2$fl_mm.y,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
mtext('Shropshire File: GOM_KM_LAA_241102.csv')
for(i in ages){
  with(subset(laa_merge2, final_age == i & sex =='Female'),
       lines(year, fl_mm.y, col = i, lwd = 2))
  with(subset(laa_merge2, final_age == i & sex =='Male'),
       lines(year, fl_mm.y, col = i, lwd = 2, lty = 3))
}
dev.off()



### sexes combined

### aggregate sedar data
laa_38 <- aggregate(fl_mm ~ year + final_age,
                    data = subdat_38, mean, na.rm = T)
laa_38u <- aggregate(OBSERVED_FL_MM ~ CATCH_YEAR + CALENDAR_AGE,
                     data = subdat_38u, mean, na.rm = T)
names(laa_38u) <- names(laa_38)
laa_38_all <- rbind(laa_38, laa_38u)


### aggregate shropshire data
laa_sh1 <- aggregate(FLENGTH ~ year + AGE,
                     data = subshrop1, mean, na.rm = TRUE)
laa_sh2 <- aggregate(LENGTH..FL.mm. ~ YEAR + age,
                     data = subshrop2, mean, na.rm = TRUE)
names(laa_sh2) <- names(laa_sh1) <- names(laa_38_all)
# laa_sh_all <- rbind(laa_sh1, laa_sh2)


### combine files and compare
laa_merge1 <- merge(laa_38_all, laa_sh1, by = c('year', 'final_age'))
laa_merge1$fl_mm.y <- laa_merge1$fl_mm.y * 10
laa_merge1$fl_mm.x <- signif(laa_merge1$fl_mm.x, 4)
laa_merge1$fl_mm.y <- signif(laa_merge1$fl_mm.y, 4)

laa_merge2 <- merge(laa_38_all, laa_sh2, by = c('year', 'final_age'))
laa_merge2$fl_mm.x <- signif(laa_merge2$fl_mm.x, 4)
laa_merge2$fl_mm.y <- signif(laa_merge2$fl_mm.y, 4)

plot(laa_merge1$fl_mm.x, laa_merge1$fl_mm.y, asp = 1)
abline(0, 1, col = 2)

plot(laa_merge2$fl_mm.x, laa_merge2$fl_mm.y, asp = 1)
abline(0, 1, col = 2)

laa_merge1$diff <- laa_merge1$fl_mm.x - laa_merge1$fl_mm.y
laa_merge2$diff <- laa_merge2$fl_mm.x - laa_merge2$fl_mm.y

hist(laa_merge1$diff)
hist(laa_merge2$diff)

plot(sort(laa_merge1$diff),asp=1)
abline(h=0,col=2)
plot(sort(laa_merge2$diff),asp=1)
abline(h=0,col=2)

length(which(laa_merge1$diff == 0)) / nrow(laa_merge1)
length(which(laa_merge2$diff == 0)) / nrow(laa_merge2)

summary(laa_merge1$diff)
summary(laa_merge2$diff)

nrow(subdat_38)
nrow(subshrop1)
nrow(subdat_38u) + nrow(subdat_38)
nrow(subshrop2)

table(subdat_38$year)
table(subdat_38u$CATCH_YEAR)
table(subshrop1$year)
table(subshrop2$YEAR)

table(subdat_38$mode)
table(subdat_38u$FISHING_MODE)
table(subshrop1$MODE)
table(subshrop2$MODE)

table(subdat_38$mode_2)
table(subdat_38u$FISHERY)

table(subdat_38$source)
table(subdat_38u$SOURCE_CODE)
table(subshrop1$SOURCE)
table(subshrop2$SOURCE)

table(subdat_38$gear)
table(subdat_38u$GEAR_GROUP_CODE)
table(subshrop1$GEAR)
table(subshrop2$GEAR)


par(mfcol = c(2, 2))

ages <- unique(laa_merge1$final_age)

plot(laa_merge1$year, laa_merge1$fl_mm.x,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages){
  with(subset(laa_merge1, final_age == i),
       lines(year, fl_mm.x, col = i, lwd = 2))
}

plot(laa_merge1$year, laa_merge1$fl_mm.y,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages){
  with(subset(laa_merge1, final_age == i),
       lines(year, fl_mm.y, col = i, lwd = 2))
}



ages <- unique(laa_merge2$final_age)

plot(laa_merge2$year, laa_merge2$fl_mm.x,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages){
  with(subset(laa_merge2, final_age == i),
       lines(year, fl_mm.x, col = i, lwd = 2))
}

plot(laa_merge2$year, laa_merge2$fl_mm.y,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages){
  with(subset(laa_merge2, final_age == i),
       lines(year, fl_mm.y, col = i, lwd = 2))
}


#### South Atlantic ------------------------

### SEDAR
sa_dat_38 <- subset(dat_38, sex == 'M' | sex == 'F') |>
  subset(final_age < 9 & final_age > 1) |>
  subset(gear == 'HL') |>
  subset(stock_id_2 == 'Atlantic') |>
  subset(year != 2013)

sa_dat_38u <- subset(dat_38u, MACRO_SEX == 'M' | MACRO_SEX == 'F') |>
  subset(CALENDAR_AGE < 9 & CALENDAR_AGE > 1) |>
  subset(GEAR_GROUP_CODE == 'HL') |>
  subset(STOCK_ID == 'SOUTH ATLANTIC')

### aggregate data
sa_laa_38 <- aggregate(fl_mm ~ year + final_age + sex,
                    data = sa_dat_38, mean, na.rm = T)
sa_laa_38u <- aggregate(OBSERVED_FL_MM ~ CATCH_YEAR + CALENDAR_AGE + MACRO_SEX,
                     data = sa_dat_38u, mean, na.rm = T)
names(sa_laa_38u) <- names(sa_laa_38)
sa_laa_38_all <- rbind(sa_laa_38, sa_laa_38u)


### Shropshire
sa_shrop1 <- subset(shrop1, SEX == 'Male' | SEX == 'Female') |>
  subset(AGE < 9 & AGE > 1) |>
  subset(GEAR == 'HL') |>
  subset(STOCK_ID2 == 'Atlantic') |>
  subset(year != 2013)

sa_shrop3 <- subset(shrop3, age < 9 & age > 1)

### aggregate data
sa_laa_sh1 <- aggregate(FLENGTH ~ year + AGE + SEX,
                     data = sa_shrop1, mean, na.rm = TRUE)
sa_laa_sh2 <- aggregate(LENGTH..FL.mm. ~ YEAR + age + SEX,
                     data = sa_shrop3, mean, na.rm = TRUE)
names(sa_laa_sh2) <- names(sa_laa_sh1) <- names(sa_laa_38_all)
# laa_sh_all <- rbind(laa_sh1, laa_sh2)


### combine files and compare
sa_laa_38_all$sex <- ifelse(sa_laa_38_all$sex == 'F', 'Female', 'Male')

sa_laa_merge1 <- merge(sa_laa_38_all, sa_laa_sh1, by = c('year', 'final_age', 'sex'))
sa_laa_merge1$fl_mm.y <- sa_laa_merge1$fl_mm.y * 10
sa_laa_merge1$fl_mm.x <- signif(sa_laa_merge1$fl_mm.x, 4)
sa_laa_merge1$fl_mm.y <- signif(sa_laa_merge1$fl_mm.y, 4)

sa_laa_merge2 <- merge(sa_laa_38_all, sa_laa_sh2, by = c('year', 'final_age', 'sex'))
sa_laa_merge2$fl_mm.x <- signif(sa_laa_merge2$fl_mm.x, 4)
sa_laa_merge2$fl_mm.y <- signif(sa_laa_merge2$fl_mm.y, 4)

plot(sa_laa_merge1$fl_mm.x, sa_laa_merge1$fl_mm.y, asp = 1)
abline(0, 1, col = 2)

plot(sa_laa_merge2$fl_mm.x, sa_laa_merge2$fl_mm.y, asp = 1)
abline(0, 1, col = 2)

sa_laa_merge1$diff <- sa_laa_merge1$fl_mm.x - sa_laa_merge1$fl_mm.y
sa_laa_merge2$diff <- sa_laa_merge2$fl_mm.x - sa_laa_merge2$fl_mm.y

hist(sa_laa_merge1$diff)
hist(sa_laa_merge2$diff)

plot(sort(sa_laa_merge1$diff),asp=1)
abline(h=0,col=2)
plot(sort(sa_laa_merge2$diff),asp=1)
abline(h=0,col=2)

length(which(sa_laa_merge1$diff == 0)) / nrow(sa_laa_merge1)
length(which(sa_laa_merge2$diff == 0)) / nrow(sa_laa_merge2)

summary(sa_laa_merge1$diff)
summary(sa_laa_merge2$diff)

nrow(sa_dat_38)
nrow(sa_shrop1)
nrow(sa_dat_38u) + nrow(sa_dat_38)
nrow(sa_shrop3)

table(sa_dat_38$sex)
table(sa_dat_38u$MACRO_SEX)
table(sa_shrop1$SEX)
table(sa_shrop3$SEX)

table(sa_dat_38$year)
table(sa_dat_38u$CATCH_YEAR)
table(sa_shrop1$year)
table(sa_shrop3$YEAR)

table(sa_dat_38$mode)
table(sa_dat_38u$FISHING_MODE)
table(sa_shrop1$MODE)
table(sa_shrop3$MODE)

table(sa_dat_38$mode_2)
table(sa_dat_38u$FISHERY)

table(sa_dat_38$source)
table(sa_dat_38u$SOURCE_CODE)
table(sa_shrop1$SOURCE)
table(sa_shrop3$SOURCE)

table(sa_dat_38$gear)
table(sa_dat_38u$GEAR_GROUP_CODE)
table(sa_shrop1$GEAR)
table(subshrop2$GEAR)



setwd("C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/figs")
png('kmk_laa_comps_sa.png', width = 8, height = 9, units = 'in', res = 300)
par(mfcol = c(2, 2))

ages <- unique(sa_laa_merge1$final_age)

plot(sa_laa_merge1$year, sa_laa_merge1$fl_mm.x,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: SA')
mtext('PCL File: SEDAR38')
legend ('topright', legend = ages, col = ages[1:8], lty = 1, bty = 'n', cex = .7)
for(i in ages){
  with(subset(sa_laa_merge1, final_age == i & sex =='Female'),
       lines(year, fl_mm.x, col = i, lwd = 2))
  with(subset(sa_laa_merge1, final_age == i & sex =='Male'),
       lines(year, fl_mm.x, col = i, lwd = 2, lty = 3))
}

plot(sa_laa_merge1$year, sa_laa_merge1$fl_mm.y,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: SA')
mtext('Shropshire File: KMK_LAdata.xlsx')
for(i in ages){
  with(subset(sa_laa_merge1, final_age == i & sex =='Female'),
       lines(year, fl_mm.y, col = i, lwd = 2))
  with(subset(sa_laa_merge1, final_age == i & sex =='Male'),
       lines(year, fl_mm.y, col = i, lwd = 2, lty = 3))
}



ages <- unique(sa_laa_merge2$final_age)

plot(sa_laa_merge2$year, sa_laa_merge2$fl_mm.x,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: SA')
mtext('PCL File: SEDAR38U')
for(i in ages){
  with(subset(sa_laa_merge2, final_age == i & sex =='Female'),
       lines(year, fl_mm.x, col = i, lwd = 2))
  with(subset(sa_laa_merge2, final_age == i & sex =='Male'),
       lines(year, fl_mm.x, col = i, lwd = 2, lty = 3))
}
legend ('topright', legend = c('female', 'male'), col = 'gray40', lty = c(1,3), bty = 'n', cex = .7)


plot(sa_laa_merge2$year, sa_laa_merge2$fl_mm.y,
     type = 'n', xlab = 'Year', ylab = 'Mean Length (mm)', main = 'Stock ID: SA')
mtext('Shropshire File: SA_KM_LAA_241102.csv')
for(i in ages){
  with(subset(sa_laa_merge2, final_age == i & sex =='Female'),
       lines(year, fl_mm.y, col = i, lwd = 2))
  with(subset(sa_laa_merge2, final_age == i & sex =='Male'),
       lines(year, fl_mm.y, col = i, lwd = 2, lty = 3))
}
dev.off()


