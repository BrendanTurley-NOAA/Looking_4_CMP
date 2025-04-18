### this script will perform a length-at-age sensitivity analysis
### the data need to be characterized by sample sizes for components that are exmained
### Chris and Bev recommended the following:
### 1. look at sample sizes across years, ages, and regions/states
### 2. remove tournament and science survey samples
### 3. double check that the data use current mixing zone definitions

source('C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/scripts/helper_fxns.R')

library(dplyr)
library(lubridate)
library(readxl)
library(stringr)


### sedar38 data: 1986-2013
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

dat3 <- subset(dat_38, sex == 'M' | sex == 'F') |>
  subset(final_age < 9 & final_age > 1) |>
  subset(gear == 'HL') |>
  subset(state != 'MEX')


### SEDAR38U data: 2013-2017
setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")
dat_38u <- read_xlsx('2019SEDAR38UAge_Data (1).xlsx', sheet = 2)

dat2 <- subset(dat_38u, MACRO_SEX == 'M' | MACRO_SEX == 'F') |>
  subset(GEAR_GROUP_CODE == 'HL') |>
  subset(CALENDAR_AGE < 9 & CALENDAR_AGE > 1)

