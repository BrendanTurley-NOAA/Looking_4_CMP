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

subdat_38 <- subset(dat_38, sex == 'M' | sex == 'F') |>
  subset(final_age < 9 & final_age > 1) |>
  subset(gear == 'HL') |>
  subset(state != 'MEX') |>
  subset(stock_id_2 == 'Gulf') |>
  subset(year != 2013)


### SEDAR38 update file
dat_38u <- read_xlsx('2019SEDAR38UAge_Data (1).xlsx', sheet = 2)

subdat_38u <- subset(dat_38u, MACRO_SEX == 'M' | MACRO_SEX == 'F') |>
  subset(CALENDAR_AGE < 9 & CALENDAR_AGE > 1) |>
  subset(GEAR_GROUP_CODE == 'HL') |>
  subset(STOCK_ID == 'GULF OF MEXICO')


### aggregate data
laa_38 <- aggregate(fl_mm ~ year + final_age + sex,
                    data = subdat_38, mean, na.rm = T)
laa_38u <- aggregate(OBSERVED_FL_MM ~ CATCH_YEAR + CALENDAR_AGE + MACRO_SEX,
                     data = subdat_38u, mean, na.rm = T)
names(laa_38u) <- names(laa_38)
laa_38_all <- rbind(laa_38, laa_38u)


### import data from Shropshire
setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")

### big file from shropshire
shrop1 <- read_xlsx('KMK_LAdata.xlsx')
shrop1$year <- year(shrop1$DATE)
shrop1$month <- month(shrop1$DATE)
shrop1$STATE <- toupper(shrop1$STATE)
shrop1$GEAR <- toupper(shrop1$GEAR)

subshrop1 <- subset(shrop1, SEX == 'Male' | SEX == 'Female') |>
  subset(AGE < 9 & AGE > 1) |>
  subset(GEAR == 'HL') |>
  subset(STOCK_ID2 == 'Gulf') |>
  subset(year != 2013)

### smaller file
shrop2 <- read.csv('GOM_KM_LAA_241102.csv')
shrop2$age <- round(shrop2$AGE..years.)

subshrop2 <- subset(shrop2, age < 9 & age > 1)


### aggregate data
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

nrow(subdat_38u) + nrow(subdat_38)
nrow(subshrop1)
nrow(subshrop2)

table(subdat_38$year)
table(subdat_38u$CATCH_YEAR)
table(subshrop1$year)
table(subshrop2$YEAR)
