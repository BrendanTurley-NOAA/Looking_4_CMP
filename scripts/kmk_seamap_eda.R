
library(dplyr)
library(lubridate)


### KMK sdm using seamap trawl data
### Scomberomorus cavalla


### load data
setwd("~/data/seamap/SEAMAPDATAV3CSV")

vessel <- read.csv('VESSELS.csv')
cruise <- read.csv('cruises.csv')
sta <- read.csv('STAREC.csv')
env <- read.csv('ENVREC.csv')
catch <- read.csv('BGSREC.csv')
lthfreq <- read.csv('GLFREC.csv')
bcodes <- read.csv('BIOCODES.csv')
merrec <- read.csv('MERREC.csv')


kmk <- bcodes[grep('Scomberomorus cavalla', bcodes$TAXON, ignore.case = T),]

catch_kmk <- subset(catch, BIO_BGS %in% kmk$BIOCODE)
merrec_kmk <- subset(merrec, BGSID %in% catch_kmk$BGSID)
lthfreq_kmk <- subset(lthfreq, BGSID %in% catch_kmk$BGSID)

sta_kmk <- subset(sta, STATIONID %in% catch_kmk$STATIONID)
plot(sta_kmk$DECSLON,sta_kmk$DECSLAT)


### pull location (lon/lat), date-time, cruise, vessel, kmk # and weight, depth, sst, chl, sbt, wind sp and dir, wave ht and dir, air temp, air press
# join cruise, vessel, station, env, catch

sta_cr <- left_join(sta, cruise, by = 'CRUISEID')
