
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


setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")
dat <- read_xlsx('2019SEDAR38UAge_Data (1).xlsx', sheet = 2)

barplot(table(dat$CATCH_YEAR))
barplot(table(dat$CATCH_MONTH))
addmargins(table(dat$CATCH_YEAR, dat$CATCH_MONTH))
addmargins(table(dat$STATE_LANDED, dat$CATCH_YEAR))
table(dat$STATE_LANDED, dat$CATCH_MONTH)
table(dat$STATE_LANDED, dat$COUNTY_LANDED)
table(dat$FISHING_MODE, dat$FISHERY)
table(dat$GEAR_CODE)
table(dat$GEAR_NAME, dat$FISHING_MODE)
table(dat$GEAR_NAME, dat$GEAR_GROUP_CODE)
table(dat$CATCH_YEAR, dat$GEAR_GROUP_CODE)
table(dat$CATCH_MONTH, dat$GEAR_GROUP_CODE)
table(dat$CATCH_YEAR, dat$FISHING_MODE)
table(dat$CATCH_MONTH, dat$FISHING_MODE)
table(dat$MACRO_SEX, dat$GEAR_GROUP_CODE)
table(dat$MACRO_SEX, dat$CALENDAR_AGE)
table(dat$MACRO_SEX, dat$CATCH_YEAR)
table(dat$MACRO_SEX, dat$CATCH_MONTH)
table(dat$MACRO_SEX, dat$STATE_LANDED)
table(dat$STOCK_ID, dat$CATCH_YEAR)
table(dat$STOCK_ID, dat$CATCH_MONTH)
table(dat$STOCK_ID, dat$MACRO_SEX)
table(dat$STOCK_ID, dat$CALENDAR_AGE)


dat2 <- read_xlsx('SEDAR38 PCLAB Age File New Mixing Zone_v2.xlsx', sheet = 1)
dat2$date2 <- convert_excel_dates(dat2$Date)
dat2$month_num <- match(dat2$Month,month.abb)
dat2$State <- toupper(dat2$State)
dat2$Sex <- toupper(dat2$Sex)

barplot(table(dat2$Year))
barplot(table(dat2$month_num))
addmargins(table(dat2$Year, dat2$month_num))
addmargins(table(dat2$State, dat2$Year))
addmargins(table(dat2$State, dat2$month_num))
table(dat2$Mode, dat2$`Mode 2`)
table(dat2$Gear, dat2$`Mode 2`)
table(dat2$Year, dat2$`Mode 2`)
table(dat2$month_num, dat2$`Mode 2`)
table(dat2$Sex, dat2$`Mode 2`)
table(dat2$Sex, dat2$`Final Age`)
table(dat2$Sex, dat2$Year)
table(dat2$Sex, dat2$month_num)
table(dat2$Sex, dat2$State)
table(dat2$`Stock ID 2`, dat2$Year)
table(dat2$`Stock ID 2`, dat2$month_num)
table(dat2$`Stock ID 2`, dat2$Sex)
table(dat2$`Stock ID 2`, dat2$`Final Age`)

### to combine data sources
# check overlap between because both have data from 2013
# which columns are needed to merge?
# record #, data provider, year, month, day, state, county, stock_id, fishing mode (mode), fishery (mode 2), gear,length, weight, age, fractional_Age, sex, outlier

names(dat)
names(dat)[c(4,5,7,8,9,11,13,14,15,16,17,22,23,24,32,33,40,41,48,49,51,62)]
dat_38u <- dat[,c(4,5,7,8,9,11,13,14,15,16,17,22,23,24,32,33,40,41,48,49,51,62)]

names(dat2)
names(dat2)[c(1,3,4,5,7,8,9,10,11,12,15,16,17,18,19,20,21,22,24)]
dat_38 <- dat2[,c(1,3,4,5,7,8,9,10,11,12,15,16,17,18,19,20,21,22,24)]

### reorder columns
# fish ID, source, year, date, month, day, state, county, stock ID, mode, mode 2, gear, length_tl, length_sl, weight_wh, weight_gu, age, sex, outlier

names(dat_38)[c(2,4,1,3,19,)]

names(dat_38u)

