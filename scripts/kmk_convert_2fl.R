# convert FL to TL

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

convert_2_fl <- function(lth, in_type = "TL") {
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

