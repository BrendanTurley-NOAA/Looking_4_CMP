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
dat_38u <- read_xlsx('2019SEDAR38UAge_Data (1).xlsx', sheet = 2)

dat2 <- subset(dat_38u, MACRO_SEX == 'M' | MACRO_SEX == 'F') #|>
  # subset(GEAR_GROUP_CODE == 'HL')


### find mean length at age per year
length(which(!is.na(dat2$OBSERVED_FL_MM)))
table(dat2$CATCH_YEAR, dat2$CALENDAR_AGE)
table(dat2$CATCH_YEAR, dat2$MACRO_SEX)

yr_agg <- aggregate(OBSERVED_FL_MM ~ CATCH_YEAR + CALENDAR_AGE + MACRO_SEX + STOCK_ID,
                    data = dat2, median, na.rm = T)
ages <- unique(yr_agg$CALENDAR_AGE)
unique(yr_agg$STOCK_ID)

par(mfrow = c(2, 2))

plot(yr_agg$CATCH_YEAR, yr_agg$OBSERVED_FL_MM,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages){
  with(subset(yr_agg, CALENDAR_AGE == i & MACRO_SEX =='F' & STOCK_ID == 'GULF OF MEXICO'),
       lines(CATCH_YEAR, OBSERVED_FL_MM, col = i, lwd = 2))
  with(subset(yr_agg, CALENDAR_AGE == i & MACRO_SEX =='M' & STOCK_ID == 'GULF OF MEXICO'),
       lines(CATCH_YEAR, OBSERVED_FL_MM, col = i, lwd = 2, lty = 3))
}
# legend('topright', legend = ages[1:8], col = ages[1:8], lty = 1)

plot(yr_agg$CATCH_YEAR, yr_agg$OBSERVED_FL_MM,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: SoAtl')
for(i in ages[1:8]){
  with(subset(yr_agg, CALENDAR_AGE == i & MACRO_SEX =='F' & STOCK_ID == 'SOUTH ATLANTIC'),
       lines(CATCH_YEAR, OBSERVED_FL_MM, col = i, lwd = 2))
  with(subset(yr_agg, CALENDAR_AGE == i & MACRO_SEX =='M' & STOCK_ID == 'SOUTH ATLANTIC'),
       lines(CATCH_YEAR, OBSERVED_FL_MM, col = i, lwd = 2, lty = 3))
}

plot(yr_agg$CATCH_YEAR, yr_agg$OBSERVED_FL_MM,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: Mixing')
for(i in ages[1:8]){
  with(subset(yr_agg, CALENDAR_AGE == i & MACRO_SEX =='F' & STOCK_ID == 'MIXING'),
       lines(CATCH_YEAR, OBSERVED_FL_MM, col = i, lwd = 2))
  with(subset(yr_agg, CALENDAR_AGE == i & MACRO_SEX =='M' & STOCK_ID == 'MIXING'),
       lines(CATCH_YEAR, OBSERVED_FL_MM, col = i, lwd = 2, lty = 3))
}



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
  subset(final_age <9) |>
  subset(gear == 'HL') |>
# subset(mode != 'TRN')
  subset(state != 'MEX') #|>
  # subset(state != 'TX')


### find mean length at age per year
length(which(!is.na(dat3$fl_mm)))
table(dat3$year, dat3$final_age)
table(dat3$year, dat3$sex)
table(dat3$gear)

yr_agg3 <- aggregate(fl_mm ~ year + final_age + sex + stock_id_2,
                    data = dat3, median, na.rm = T)
ages3 <- unique(yr_agg3$final_age)
unique(yr_agg3$stock_id_2)


par(mfrow = c(2, 2))

plot(yr_agg3$year, yr_agg3$fl_mm,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: GOM')
for(i in ages[1:8]){
  with(subset(yr_agg3, final_age == i & sex =='F' & stock_id_2 == 'Gulf'),
       lines(year, fl_mm, col = i, lwd = 2))
  with(subset(yr_agg3, final_age == i & sex =='M' & stock_id_2 == 'Gulf'),
       lines(year, fl_mm, col = i, lwd = 2, lty = 3))
}
# legend('topright', legend = ages[1:8], col = ages[1:8], lty = 1)

plot(yr_agg3$year, yr_agg3$fl_mm,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: SoAtl')
for(i in ages[1:8]){
  with(subset(yr_agg3, final_age == i & sex =='F' & stock_id_2 == 'Atlantic'),
       lines(year, fl_mm, col = i, lwd = 2))
  with(subset(yr_agg3, final_age == i & sex =='M' & stock_id_2 == 'Atlantic'),
       lines(year, fl_mm, col = i, lwd = 2, lty = 3))
}

plot(yr_agg3$year, yr_agg3$fl_mm,
     type = 'n', xlab = 'Age', ylab = 'Mean Length (mm)', main = 'Stock ID: Mixing')
for(i in ages[1:8]){
  with(subset(yr_agg3, final_age == i & sex =='F' & stock_id_2 == 'Winter Mixing'),
       lines(year, fl_mm, col = i, lwd = 2))
  with(subset(yr_agg3, final_age == i & sex =='M' & stock_id_2 == 'Winter Mixing'),
       lines(year, fl_mm, col = i, lwd = 2, lty = 3))
}

