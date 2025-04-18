### helper functions for the analysis of the data


# convert to fork length from total or standard length

library(units)

# https://www.fishbase.de/popdyn/LLRelationshipList.php?ID=120&GenusName=Scomberomorus&SpeciesName=cavalla&fc=416
# https://www.fishbase.se/manual/fishbasethe_lengthlength_table.htm
# https://www.fishbase.org.au/manual/English/PDF/FB_Book_CBinohlan_Length-Length_RF_JG.pdf

# a = -1.774
# b = 1.090
# alternative
# a = -1.714
# b = 1.096

# TL = a + b * FL
# or
# TL = b * FL


# From SEDAR38U report; convert between TL (cm) to FL (cm) and SL (cm)
# FL = -4.28 +0.963 * TL
# SL = 0.663 + 1.051 * FL

# convert_tl_2_fl = function(tl, a = -4.28, b = 0.963) {
#   return(a + b * tl)
# }

# convert_sl_2_fl = function(sl, a = 0.663, b = 1.051) {
#   return(a + b * sl)
# }

kmk_convert_2_fl <- function(lth, in_type = "TL") {
  # length needs to be in centimeters
  if(class(lth) != 'units'){
    stop("Length needs to be in units")
  }
  if(units(lth)$numerator != 'cm'){
    stop("Length needs to be in centimeters")
  }
  if (in_type == "TL") {
    a <- -4.28
    b <- 0.963
  } else if (in_type == "SL") {
    a <- 0.663
    b <- 1.051
  }
  return(a + b * lth)
}

# x <- 300
# units(x) <- "cm"
# set_units(x, 'mm')


library(dplyr)
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


cv <- 
  function (x) {
    return( sd(x, na.rm = T) / mean(x, na.rm = T) )
  }


se <- 
  function (x) {
    return( sd(x, na.rm = T) / sqrt(length(x)) )
  }