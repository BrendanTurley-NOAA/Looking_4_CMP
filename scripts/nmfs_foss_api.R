

# foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/'
foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=0&limit=10000&q={"year":2010,"ts_afs_name":{"$like":"%SNAPPER%"}}'
foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=0&limit=10000&q={"year":2010,"ts_afs_name":{"$like":"%SNAPPER%"}}'
foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=0&limit=10000&q={"ts_afs_name":{"$like":"%SNAPPER%"}}'
foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=0&limit=10000&q={"tsn":"168853"}'

# foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/trade_data/?q={"year":2010,"name":{"$like":"%25POLLOCK%25"}}'
foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=0&limit=10000&q={"region_name":{"$like":"%Gulf%"}}'
# foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=0&limit=10000'
# foss_api <- 'https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=10001&limit=20000'

res <- httr::GET(url = foss_api)
dat1 <- jsonlite::fromJSON(base::rawToChar(res$content))$items

ind <- seq(1e4,1e6,1e4)
pb = txtProgressBar(min = 0, max = length(ind), initial = 0, style = 3) 
for(i in 1:(length(ind))){
  spot1 <- ifelse(i==1, 0, ind[i]+1)
  foss_api <- paste0('https://apps-st.fisheries.noaa.gov/ods/foss/landings/?offset=',
                     spot1,'&limit=',ind[i],'&q={"region_name":{"$like":"%Gulf%"}}')
  res <- httr::GET(url = foss_api)
  dat <- jsonlite::fromJSON(base::rawToChar(res$content))$items
  dat1[spot1:ind[i], ] <- dat
  setTxtProgressBar(pb, i)
}
table(dat1$region_name, useNA = 'always')

gulf <- subset(dat1, region_name=='Gulf')
sort(table(gulf$ts_afs_name))
table(gulf$year)

gulf_tot_yr <- aggregate(pounds ~ year + ts_afs_name + collection, data = gulf, sum, na.rm = T)

with(subset(gulf_tot_yr, ts_afs_name=='MACKEREL, KING AND CERO **'),
     plot(year, pounds, typ = 'b', pch = 16))



library(httr)
library(jsonlite)

base_url <- "https://apps-st.fisheries.noaa.gov/ods/foss/landings/"
results <- list()

for (year in 2010:2020) {
  query <- list(
    offset = 0,
    limit = 10000,
    q = toJSON(list(
      year = year,
      ts_afs_name = list(`$like` = "%SNAPPER%")
    ), auto_unbox = TRUE)
  )
  
  res <- GET(base_url, query = query)
  
  if (status_code(res) == 200) {
    data <- content(res, as = "parsed", type = "application/json")
    results[[as.character(year)]] <- data
    cat("Retrieved data for", year, "\n")
  } else {
    cat("Failed to get data for", year, "\n")
  }
}

# Optionally save or inspect:
# saveRDS(results, "snapper_landings_2010_2020.rds")
