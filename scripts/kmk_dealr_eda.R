
library(readxl)

setwd("C:/Users/brendan.turley/Documents/CMP/data")
coast_cnty <- read_xlsx('coastline-counties-list.xlsx')
gulf_coast <- subset(coast_cnty, `COASTLINE REGION`=='Gulf of Mexico')
gulf_coast <- toupper(gulf_coast$NAME)

dat <- readRDS("C:/Users/brendan.turley/Documents/data/gom_com_vul/GI.RData")
sort(unique(dat$county_name))
cnty_keep <- which(is.element(dat$county_name, gulf_coast))
dat <- dat[cnty_keep, ]
sort(unique(dat$county_name))

### remove duplicated dealers (this needs a better explanation for why the dataset have dups)
dealrs <- unique(dat[,c(3,5)])
dup_dealr <- merge(dealrs[which(duplicated(dealrs[,1],fromLast = T)),], 
                   dealrs[which(duplicated(dealrs[,1])),], 
                   by = 'dealer_id')
dealrs <- dealrs[!is.element(dealrs$dealer_id, dup_dealr$dealer_id),]
dat <- dat[!is.element(dat$dealer_id, dup_dealr$dealer_id),]

sort(unique(dat$common_name))
# spp_keep <- c('MACKEREL, KING', 
#               'MACKEREL, KING AND CERO',
#               'MACKERELS')
spp_keep <- c('MACKEREL, KING')

### find dealers that land kmk; what is the volume and proportion of landings
# dealr_spp_agg <- aggregate(dat$live_lbs, by = list(dat$dealer_id, dat$common_name), sum, na.rm = T) |>
#   setNames(c('dealer_id', 'common_name', 'live_lbs'))
# kmk_dealr <- unique(dealr_spp_agg$dealer_id[which(dealr_spp_agg$common_name==spp_keep)])
# kmk_dealr <- kmk_dealr[-1]
kmk_dealr <- sort(unique(dat$dealer_id[which(dat$common_name==spp_keep)]))
kmk_dealr <- kmk_dealr[-1]
kmk_dealr_dat <- subset(dat, is.element(dealer_id, kmk_dealr))



dealr_yr_all_agg <- aggregate(kmk_dealr_dat$live_lbs,
                              by = list(kmk_dealr_dat$dealer_id, kmk_dealr_dat$landed_year), 
                              sum, na.rm=T) |>
  setNames(c('dealer_id', 'landed_year', 'live_lbs'))

dealr_yr_kmk_agg <- with(subset(kmk_dealr_dat, common_name==spp_keep),
                         aggregate(live_lbs,
                                   by = list(dealer_id, landed_year),
                                   sum, na.rm=T)) |>
  setNames(c('dealer_id', 'landed_year', 'kmk_live_lbs'))


dealr_kmk_mean <- with(subset(kmk_dealr_dat, common_name==spp_keep),
                       aggregate(live_lbs, by = list(dealer_id),
                                 max, na.rm=T)) |>
  setNames(c('dealer_id', 'kmk_live_lbs'))
dealr_kmk_mean$Q <- cut(dealr_kmk_mean$kmk_live_lbs, 
                        quantile(dealr_kmk_mean$kmk_live_lbs), 
                        labels = c('Q1','Q2','Q3','Q4'))
quantile(dealr_kmk_mean$kmk_live_lbs)


kmk_dealr_merge <- merge(dealr_yr_kmk_agg, dealr_yr_all_agg, by = c('dealer_id','landed_year'), all = T)
kmk_dealr_merge$kmk_pro <- kmk_dealr_merge$kmk_live_lbs / kmk_dealr_merge$live_lbs
kmk_dealr_merge <- merge(kmk_dealr_merge, dealr_kmk_mean[,-2], by = 'dealer_id')
# kmk_dealr_merge2 <- merge(kmk_dealr_merge, dat[,c(3,5)], by = c('dealer_id'))

dealr_yrs <- aggregate(landed_year ~ dealer_id, data = kmk_dealr_merge, function(x) length(unique(x)))
hist(dealr_yrs$landed_year,breaks=seq(-.5,40.5,1))
summary(dealr_yrs$landed_year)

# boxplot(kmk_dealr_merge$kmk_pro~kmk_dealr_merge$Q, outline = T)

# with(subset(kmk_dealr_merge, Q=='Q1'),boxplot(kmk_pro~landed_year))
# with(subset(kmk_dealr_merge, Q=='Q2'),boxplot(kmk_pro~landed_year))
# with(subset(kmk_dealr_merge, Q=='Q3'),boxplot(kmk_pro~landed_year))
# with(subset(kmk_dealr_merge, Q=='Q4'),boxplot(kmk_pro~landed_year))


### find dealers that had data 2013 & 2017, but not in 2014-2016
# missing_drs <- aggregate(kmk_dealr_merge$landed_year, by = list(kmk_dealr_merge$dealer_id),
#                          function(x)
#                            any(x!=2014) &
#                            any(x!=2015) &
#                            any(x!=2016) &
#                            any(x==2013) &
#                            any(x==2017))
# missing_ids <- missing_drs$Group.1[which(missing_drs$x)]
# kmk_dealr_merge <- kmk_dealr_merge[which(!is.element(kmk_dealr_merge$dealer_id, missing_ids)), ]

missing_drs <- aggregate(kmk_dealr_merge$landed_year, by = list(kmk_dealr_merge$dealer_id),
                         function(x)
                           sum(any(x==2014),
                               any(x==2015),
                               any(x==2016),
                               any(x==2013),
                               any(x==2017)))
missing_ids <- missing_drs$Group.1[which(missing_drs$x==2)]
kmk_dealr_merge <- kmk_dealr_merge[which(!is.element(kmk_dealr_merge$dealer_id, missing_ids)), ]


kmk_dealr_agg <- aggregate(dealer_id ~ landed_year + Q, data = kmk_dealr_merge,
          function(x) length(unique(x)))

with(aggregate(dealer_id ~ landed_year, data = kmk_dealr_merge,
               function(x) length(unique(x))),
     plot(landed_year, dealer_id, typ = 'o'))


plot(kmk_dealr_agg$landed_year, kmk_dealr_agg$dealer_id, typ = 'n')
Qs <- sort(unique(kmk_dealr_agg$Q))
n <- 1
for(i in Qs){
  with(subset(kmk_dealr_agg, Q==i),
       lines(landed_year, dealer_id, col = n, typ = 'o'))
  n <- n + 1
}
grid()
quantile(dealr_kmk_mean$kmk_live_lbs)



kmk_dealr_merge2 <- merge(kmk_dealr_merge, dealrs, by = c('dealer_id'), all.x = T)
kmk_dealr_agg2 <- aggregate(dealer_id ~ landed_year + Q + dealer_state, data = kmk_dealr_merge2,
                           function(x) length(unique(x)))

tmp <- subset(kmk_dealr_agg2, dealer_state=='TX')
plot(tmp$landed_year, tmp$dealer_id, typ = 'n')
Qs <- sort(unique(tmp$Q))
n <- 1
for(i in Qs){
  with(subset(tmp, Q==i),
       lines(landed_year, dealer_id, col = n, typ = 'o'))
  n <- n + 1
}
grid()
quantile(dealr_kmk_mean$kmk_live_lbs)



macks <- dat[which(is.element(dat$common_name, spp_keep)),]
mack_dlr <- dat[which(is.element(dat$dealer_id, kmk_dealr)),]

macks$region <- ifelse(macks$dealer_state=='TX', 'Western', NA)
macks$region <- ifelse(macks$dealer_state=='LA', 'Western', NA)
macks$region <- ifelse(macks$dealer_state=='MS', 'Western', NA)
macks$region <- ifelse(macks$dealer_state=='AL', 'Western', NA)
macks$region[which(is.element(macks$county_name, c('COLLIER', 'MONROE')) &
               macks$dealer_state=='FL')] <- 'Southern'
macks$region[which(!is.element(macks$county_name, c('COLLIER', 'MONROE')) &
                     macks$dealer_state=='FL')] <- 'Northern'

mack_dlr$region <- ifelse(mack_dlr$dealer_state=='TX', 'Western', NA)
mack_dlr$region <- ifelse(mack_dlr$dealer_state=='LA', 'Western', NA)
mack_dlr$region <- ifelse(mack_dlr$dealer_state=='MS', 'Western', NA)
mack_dlr$region <- ifelse(mack_dlr$dealer_state=='AL', 'Western', NA)
mack_dlr$region[which(is.element(mack_dlr$county_name, c('COLLIER', 'MONROE')) &
                     mack_dlr$dealer_state=='FL')] <- 'Southern'
mack_dlr$region[which(!is.element(mack_dlr$county_name, c('COLLIER', 'MONROE')) &
                     mack_dlr$dealer_state=='FL')] <- 'Northern'


table(macks$landed_year, macks$dealer_state)

### find dealers that had data 2013 & 2017, but not in 2014-2016
missing_drs <- aggregate(mack_dlr$landed_year, by = list(mack_dlr$dealer_id),
                         function(x)
                           sum(any(x==2014),
                               any(x==2015),
                               any(x==2016),
                               any(x==2013),
                               any(x==2017)))
missing_ids <- missing_drs$Group.1[which(missing_drs$x==2)]
mack_dlr_missing <- mack_dlr[which(!is.element(mack_dlr$dealer_id, missing_ids)), ]

### who are the missing dealers?
dealrs <- unique(dat[,c(3:5)])
dlr_missing <- dealrs[is.element(dealrs$dealer_id, missing_ids),]
dlr_missing_merge <- aggregate(live_lbs ~ dealer_id, data = dat, mean, na.rm = T) |>
  merge(dlr_missing, by = 'dealer_id', all.y = T)
quantile(dlr_missing_merge$live_lbs)
hist(dlr_missing_merge$live_lbs)

### pounds landed per year-state
mck_yr_st_lbs <- aggregate(macks$live_lbs, 
                           by = list(macks$landed_year, macks$dealer_state), 
                           sum, na.rm = T) |>
  setNames(c('year', 'state', 'live_lbs'))


### number unique dealers per year-state
mck_yr_st_dlr <- aggregate(mack_dlr$corporate_name, 
                           by = list(mack_dlr$landed_year, mack_dlr$dealer_state), 
                           function(x) length(unique(x))) |>
  setNames(c('year', 'state', 'num_dealers'))

mck_yr_st_dlr_missing <- aggregate(mack_dlr_missing$corporate_name, 
                                   by = list(mack_dlr_missing$landed_year, mack_dlr_missing$dealer_state), 
                                   function(x) length(unique(x))) |>
  setNames(c('year', 'state', 'num_dealers'))



setwd("~/R_projects/misc-noaa-scripts/figs")
png('mck_dealer.png', width = 7, height = 7, res = 300, units = 'in')
par(mfrow=c(2,1), mar = c(5,4,1,1))

plot(mck_yr_st_lbs$year, mck_yr_st_lbs$live_lbs/10000, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'live lbs (x 10,000)', xlim = c(2000, 2020))
states <- unique(mck_yr_st_lbs$state)[c(2,1,4,3,5)]
for(i in 1:length(states)){
  mcks <- subset(mck_yr_st_lbs, state==states[i])
  lines(mcks$year, mcks$live_lbs/10000, typ= 'o', col = i, pch = i+15)
}
legend('topleft', states, col = 1:5, pch = (1:5)+15, bty = 'n')
grid()

plot(mck_yr_st_dlr$year, mck_yr_st_dlr$num_dealers, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Number of Dealers', xlim = c(2000, 2020))
states <- unique(mck_yr_st_dlr$state)[c(2,1,4,3,5)]
for(i in 1:length(states)){
  mcks <- subset(mck_yr_st_dlr, state==states[i])
  lines(mcks$year, mcks$num_dealers, typ= 'o', col = i, pch = i+15)
}
# legend('topleft', states, col = 1:5, pch = (1:5)+15, bty = 'n')
grid()
dev.off()


png('mck_dealer_missing.png', width = 7, height = 7, res = 300, units = 'in')
par(mfrow=c(2,1), mar = c(4,4,1,1))

plot(mck_yr_st_lbs$year, mck_yr_st_lbs$live_lbs/10000, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'live lbs (x 10,000)', xlim = c(2000, 2020))
states <- unique(mck_yr_st_lbs$state)[c(2,1,4,3,5)]
for(i in 1:length(states)){
  mcks <- subset(mck_yr_st_lbs, state==states[i])
  lines(mcks$year, mcks$live_lbs/10000, typ= 'o', col = i, pch = i+15)
}
legend('topleft', states, col = 1:5, pch = (1:5)+15, bty = 'n')
grid()

plot(mck_yr_st_dlr_missing$year, mck_yr_st_dlr_missing$num_dealers, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Number of Dealers', xlim = c(2000, 2020))
states <- unique(mck_yr_st_dlr_missing$state)[c(2,1,4,3,5)]
for(i in 1:length(states)){
  mcks <- subset(mck_yr_st_dlr_missing, state==states[i])
  lines(mcks$year, mcks$num_dealers, typ= 'o', col = i, pch = i+15)
}
# legend('topright', states, col = 1:5, pch = (1:5)+15, bty = 'n')
grid()
dev.off()



### pounds landed per year-region
aggregate(macks$live_lbs, 
          by = list(macks$region), 
          sum, na.rm = T)

mck_yr_st_lbs <- aggregate(macks$live_lbs, 
                           by = list(macks$landed_year, macks$region), 
                           sum, na.rm = T) |>
  setNames(c('year', 'region', 'live_lbs'))

mck_yr_st_lbs_dlr <- aggregate(macks$corporate_name, 
                           by = list(macks$landed_year, macks$region), 
                           function(x) length(unique(x))) |>
  setNames(c('year', 'region', 'num_dealers'))

mck_yr_st_lbs <- merge(mck_yr_st_lbs, mck_yr_st_lbs_dlr, by = c('year', 'region'))
mck_yr_st_lbs[which(mck_yr_st_lbs$num_dealers<=3),]
mck_yr_st_lbs$live_lbs[which(mck_yr_st_lbs$num_dealers<=3)] <- NA

### number unique dealers per year-state
mck_yr_st_dlr <- aggregate(mack_dlr$corporate_name, 
                           by = list(mack_dlr$landed_year, mack_dlr$region), 
                           function(x) length(unique(x))) |>
  setNames(c('year', 'region', 'num_dealers'))
mck_yr_st_dlr[which(mck_yr_st_dlr$num_dealers<=3),]
mck_yr_st_dlr$num_dealers[which(mck_yr_st_dlr$num_dealers<=3)] <- NA

mck_yr_st_dlr_missing <- aggregate(mack_dlr_missing$corporate_name, 
                                   by = list(mack_dlr_missing$landed_year, mack_dlr_missing$region), 
                                   function(x) length(unique(x))) |>
  setNames(c('year', 'region', 'num_dealers'))


setwd("~/R_projects/misc-noaa-scripts/figs")
png('mck_regions.png', width = 7, height = 7, res = 300, units = 'in')
par(mfrow=c(2,1), mar = c(5,4,1,1))

plot(mck_yr_st_lbs$year, mck_yr_st_lbs$live_lbs/10000, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'live lbs (x 10,000)', xlim = c(2000, 2020))
region <- unique(mck_yr_st_lbs$region)[c(3,1,2)]
for(i in 1:length(region)){
  # mcks <- subset(mck_yr_st_lbs, region==region[i])
  mcks <- mck_yr_st_lbs[which(mck_yr_st_lbs$region==region[i]),]
  lines(mcks$year, mcks$live_lbs/10000, typ= 'o', col = i, pch = i+15)
}
legend('topleft', region, col = 1:3, pch = 1:3+15, bty = 'n', cex = .8)
grid()

plot(mck_yr_st_dlr$year, mck_yr_st_dlr$num_dealers, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Number of Dealers', xlim = c(2000, 2020))
region <- unique(mck_yr_st_dlr$region)[c(3,1,2)]
for(i in 1:length(region)){
  mcks <- mck_yr_st_dlr[which(mck_yr_st_dlr$region==region[i]),]
  lines(mcks$year, mcks$num_dealers, typ= 'o', col = i, pch = i+15)
}
# legend('topleft', states, col = 1:5, pch = (1:5)+15, bty = 'n')
grid()
dev.off()


# setwd("~/R_projects/misc-noaa-scripts/figs")
# png('mck_regions_missing.png', width = 7, height = 7, res = 300, units = 'in')
# par(mfrow=c(2,1), mar = c(5,4,1,1))
# 
# plot(mck_yr_st_lbs$year, mck_yr_st_lbs$live_lbs/10000, typ = 'n', las = 1,
#      xlab = 'Year', ylab = 'live lbs (x 10,000)', xlim = c(2000, 2020))
# region <- unique(mck_yr_st_lbs$region)[c(3,1,2)]
# for(i in 1:length(region)){
#   # mcks <- subset(mck_yr_st_lbs, region==region[i])
#   mcks <- mck_yr_st_lbs[which(mck_yr_st_lbs$region==region[i]),]
#   lines(mcks$year, mcks$live_lbs/10000, typ= 'o', col = i, pch = i+15)
# }
# legend('topleft', region, col = 1:3, pch = 1:3+15, bty = 'n', cex = .8)
# grid()
# 
# plot(mck_yr_st_dlr_missing$year, mck_yr_st_dlr_missing$num_dealers, typ = 'n', las = 1,
#      xlab = 'Year', ylab = 'Number of Dealers', xlim = c(2000, 2020))
# region <- unique(mck_yr_st_dlr_missing$region)[c(3,1,2)]
# for(i in 1:length(region)){
#   mcks <- mck_yr_st_dlr_missing[which(mck_yr_st_dlr_missing$region==region[i]),]
#   lines(mcks$year, mcks$num_dealers, typ= 'o', col = i, pch = i+15)
# }
# # legend('topleft', states, col = 1:5, pch = (1:5)+15, bty = 'n')
# grid()
# dev.off()


### pounds landed per year-county
cnty_tot <- aggregate(live_lbs ~ county_name,
                      data = macks, 
                      sum, na.rm = T)
cnty_tot[order(cnty_tot$live_lbs),]
head(cnty_tot[order(cnty_tot$live_lbs, decreasing = T),],n=10)
top3 <- head(cnty_tot[order(cnty_tot$live_lbs, decreasing = T),],n=5)[,1]
quantile(cnty_tot$live_lbs)

cnty_tot_dlr <- aggregate(corporate_name ~ county_name,
                          data = macks, 
                          function(x) length(unique(x)))

mck_yr_st_lbs <- aggregate(macks$live_lbs, 
                           by = list(macks$landed_year, macks$county_name), 
                           sum, na.rm = T) |>
  setNames(c('year', 'county', 'live_lbs'))
last <- mck_yr_st_lbs[which(mck_yr_st_lbs$year==2020),]

mck_yr_st_lbs_dlr <- aggregate(macks$corporate_name, 
                           by = list(macks$landed_year, macks$county_name), 
                           function(x) length(unique(x))) |>
  setNames(c('year', 'county', 'num_dealers'))

mck_yr_st_lbs <- merge(mck_yr_st_lbs, mck_yr_st_lbs_dlr, by = c('year', 'county'))
mck_yr_st_lbs[which(mck_yr_st_lbs$num_dealers<=3),]
mck_yr_st_lbs$live_lbs[which(mck_yr_st_lbs$num_dealers<=3)] <- NA

### number unique dealers per year-state
cnty_tot_dlr <- aggregate(corporate_name ~ county_name,
                      data = macks, 
                      function(x) length(unique(x)))
cnty_tot_dlr[order(cnty_tot_dlr$corporate_name),]
head(cnty_tot_dlr[order(cnty_tot_dlr$corporate_name, decreasing = T),],n=10)
top3_dlr <- head(cnty_tot_dlr[order(cnty_tot_dlr$corporate_name, decreasing = T),],n=5)[,1]
quantile(cnty_tot_dlr$corporate_name)

mck_yr_st_dlr <- aggregate(mack_dlr$corporate_name, 
                           by = list(mack_dlr$landed_year, mack_dlr$county_name), 
                           function(x) length(unique(x))) |>
  setNames(c('year', 'county', 'num_dealers'))
mck_yr_st_dlr[which(mck_yr_st_dlr$num_dealers<=3),]
mck_yr_st_dlr$num_dealers[which(mck_yr_st_dlr$num_dealers<=3)] <- NA


png('mck_county.png', width = 7, height = 7, res = 300, units = 'in')
par(mfrow=c(2,1), mar = c(4,4,1,1))

plot(mck_yr_st_lbs$year, mck_yr_st_lbs$live_lbs/10000, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'live lbs (x 10,000)', xlim = c(2000, 2020))
county <- unique(mck_yr_st_lbs$county)
for(i in 1:length(county)){
  # mcks <- subset(mck_yr_st_lbs, region==region[i])
  mcks <- mck_yr_st_lbs[which(mck_yr_st_lbs$county==county[i]),]
  lines(mcks$year, mcks$live_lbs/10000, typ= 'l', col = 'gray')
}
for(i in 1:5){
  mcks <- mck_yr_st_lbs[which(mck_yr_st_lbs$county==top3[i]),]
  lines(mcks$year, mcks$live_lbs/10000, typ= 'o', col = i, pch = i+15, lwd = 2)
}
legend('topleft', top3, col = 1:5, pch = 1:5+15, bty = 'n')
grid()

plot(mck_yr_st_dlr$year, mck_yr_st_dlr$num_dealers, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Number of Dealers', xlim = c(2000, 2020))
county <- unique(mck_yr_st_dlr$county)
for(i in 1:length(county)){
  # mcks <- subset(mck_yr_st_lbs, region==region[i])
  mcks <- mck_yr_st_dlr[which(mck_yr_st_dlr$county==county[i]),]
  lines(mcks$year, mcks$num_dealers, typ= 'l', col = 'gray')
}
for(i in 1:5){
  mcks <- mck_yr_st_dlr[which(mck_yr_st_dlr$county==top3_dlr[i]),]
  lines(mcks$year, mcks$num_dealers, typ= 'o', col = i, pch = i+15, lwd = 2)
}
legend('topright', top3_dlr, col = 1:5, pch = 1:5+15, bty = 'n')
grid()
dev.off()








yrs <- sort(unique(kmk_dealr_dat$landed_year))[-1]
out <- matrix(NA,length(yrs),3)
for(i in yrs){
  tmp <- unique(kmk_dealr_dat$dealer_id[which(kmk_dealr_dat$landed_year==i)])
  tmp_n <- unique(kmk_dealr_dat$dealer_id[which(kmk_dealr_dat$landed_year<=(i-1))])
  tmp_n1 <- unique(kmk_dealr_dat$dealer_id[which(kmk_dealr_dat$landed_year==(i-1))])
  tmp_p1 <- unique(kmk_dealr_dat$dealer_id[which(kmk_dealr_dat$landed_year>=(i))])
  out[i-1978, 3] <- length(setdiff(tmp_n1, tmp))
  out[i-1978, 2] <- length(which(!is.element(tmp, tmp_n)))
  out[i-1978, 1] <- i
}
plot(out[,1],out[,2], typ = 'h', lwd = 4, col = 2, ylim=c(-200,100))
points(out[,1],-out[,3], typ = 'h', lwd = 4, col = 4)
points(out[,1],out[,2]-out[,3],pch=16)
abline(h = 0, lty = 5, lwd = 2, col = 'gray50')
grid()


### zoom into FL
### number unique dealers per year-state
fl_macks <- subset(macks, dealer_state=='FL')
flmck_yr_st_dlr <- aggregate(fl_macks$corporate_name, 
                           by = list(fl_macks$landed_year, fl_macks$county_name), 
                           function(x) length(unique(x))) |>
  setNames(c('year', 'county', 'num_dealers'))

flmck_yr_st_dlr$num_dealers[which(flmck_yr_st_dlr$num_dealers<3)] <- NA

plot(flmck_yr_st_dlr$year, flmck_yr_st_dlr$num_dealers, typ = 'n', las = 1,
     xlab = 'Year', ylab = 'Number of Dealers')
cnty <- unique(flmck_yr_st_dlr$county)
for(i in 1:length(cnty)){
  mcks <- subset(flmck_yr_st_dlr, county==cnty[i])
  lines(mcks$year, mcks$num_dealers, typ= 'o', col = i, pch = i+15)
}
legend('topleft', cnty, col = 1:35, pch = 1:35+15, bty = 'n', cex = .8)
grid()


### number unique dealers per year-state
mck_st_cnty <- aggregate(macks$live_lbs, 
                           by = list(macks$dealer_state, macks$county_name), 
                           sum, na.rm = T) |>
  setNames(c('state', 'county','live_lbs'))

mck_st_cnty <- aggregate(macks$dealer_id, 
                         by = list(macks$dealer_state, macks$county_name), 
                         function(x) length(unique(x))) |>
  setNames(c('state', 'county','num_dealr'))


### FL keys dealers
FLK_mack <- subset(macks, county_name=='MONROE')
aggregate(FLK_mack$corporate_name, by = list(FLK_mack$landed_year), function(x) length(unique(x)))


### quantile plots
quantile(macks$live_lbs, seq(0,1,.1))
quantile(macks$live_lbs, seq(0,1,.2))
quantile(macks$live_lbs, seq(0,1,.25))
quantile(macks$live_lbs, seq(.75,1,.05))

macks$quants <- cut(macks$live_lbs, quantile(macks$live_lbs, seq(0,1,.25)))

dealer_qs <- aggregate(macks$dealer_id, by = list(macks$landed_year, macks$quants), function(x) length(unique(x))) |>
  setNames(c('year', 'quant', 'num_dealer'))

n <- 1
plot(dealer_qs$year, dealer_qs$num_dealer, typ = 'n')
for(i in unique(dealer_qs$quant)){
  with(subset(dealer_qs, quant==i),
       lines(year, num_dealer, col = n, lwd = 2))
  n <- n + 1
}
legend('topleft',paste(seq(0.25,1,.25)), col = 1:5, lty = 1, lwd = 2)

dealer_st_qs <- aggregate(macks$dealer_id, by = list(macks$landed_year, macks$dealer_state, macks$quants), function(x) length(unique(x))) |>
  setNames(c('year', 'state', 'quant', 'num_dealer'))

subst <- with(subset(dealer_st_qs, state=='TX' | state=='LA'),
              aggregate(num_dealer, by = list(year, quant), sum)) |>
  setNames(c('year','quant','num_dealer'))
subst <- with(subset(dealer_st_qs, state=='FL' | state=='AL'),
              aggregate(num_dealer, by = list(year, quant), sum)) |>
  setNames(c('year','quant','num_dealer'))
n <- 1
plot(subst$year, subst$num_dealer, typ = 'n')
for(i in unique(subst$quant)){
  with(subset(subst, quant==i),
       lines(year, num_dealer, col = n, lwd = 2))
  n <- n + 1
}
legend('topleft',paste(seq(0.2,1,.2)), col = 1:5, lty = 1, lwd = 2)
