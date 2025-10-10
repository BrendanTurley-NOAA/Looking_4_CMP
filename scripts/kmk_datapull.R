### find occurrances in obis and gbif

library(usethis)
# usethis::edit_r_environ()

# GBIF_USER
# GBIF_PWD
# GBIF_EMAIL

library(robis)
library(rgbif)

spn <- 'Scomberomorus cavalla'

### obis
kmk_obis <- occurrence(spn)


### gbif
taxonKey <- name_backbone(spn)$usageKey

kmk_gbif <- occ_search(taxonKey = taxonKey, limit = 2000)
kmk_gbif$data |> names()

occ_download(pred("taxonKey", taxonKey), 
             user = GBIF_USER,
             pwd = GBIF_PWD,
             email = GBIF_EMAIL)
d <- occ_download_get('0006217-251009101135966') %>%
  occ_download_import()

unzip('0006217-251009101135966.zip')
test <- readLines('occurrence.txt')
test <- read.delim('occurrence.txt')
