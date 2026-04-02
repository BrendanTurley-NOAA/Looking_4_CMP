
library(lubridate)
dat_eez$date <- as.Date(dat_eez$time)

yr_mean <- aggregate(sst_degC ~ year(date), data = dat_eez, mean, na.rm = T)
yr_mean <- aggregate(sst_degC ~ year(date), 
                     data = subset(dat_eez, month(date)>6 & month(date)<11), 
                     mean, na.rm = T)

plot(yr_mean, typ = 'b')

jdates <- subset(dat_eez, year(date)==2025, select = 'date')
jdates$jday <- yday(jdates$date)
jdates$days <- day(jdates$date)

plot(yday(dat_eez$time), dat_eez$sst_degC, typ = 'n', xaxt = 'n')
for(i in 1982:2025){
  tmp <- subset(dat_eez, year(date)==i)
  
  points(yday(tmp$time), tmp$sst_degC,
         typ = 'l', col = ifelse(i==2023, 'firebrick',
                                 ifelse(i==2024, 'red', 'gray')),
         lwd = ifelse(i>2022 & i<2025, 2, 1))

  # points(yday(tmp$time), tmp$sst_degC,
  #        typ = 'l', col = ifelse(i>2014, 'red', 'gray'),
  #        lwd = ifelse(i>2014, 2, 1))
  
  # ifelse(i>2022 & i<2025, 'red', 'gray')
}
abline(h = c(26, 31),
       v = c(jdates$jday[which(jdates$days==1 & month(jdates$date)==7 |
                                 jdates$days==1 & month(jdates$date)==10)]),
       lty = 5, lwd = 1.5)
axis(1, jdates$jday[which(jdates$days==1)],
     month.abb)
legend('bottomright',c('2023', '2024'),col = c('firebrick', 'red'), lwd = 2, bty = 'n')

library(strucchange) 

find.bp.f= function(x, y, method="BIC"){ 
  library(strucchange) 
  if (method=="BIC"){ 
    bp= breakpoints(y ~ x) 
    bp.t= breakpoints(bp)$breakpoints 
    bp.times= x[bp.t] 
    # line.prediction= fitted(bp) 
  } 
  if (method=="F.test"){ 
    bp <- Fstats(y ~ x) 
    bp.x= breakpoints(bp)$breakpoints 
    bp.times= x[bp.x] 
  } 
  
  plot(x,y,type="p",xlab="Year",ylab="",las=1)
  abline(v=bp.times,col="grey",lty=2,lwd=2) 
  lines(x,fitted(bp),lwd=2) # need to make this for the F test too. 
  if (is.na(bp.times[1])) message("no credible breakpoint by chosen method") else bp.times 
} 

find.bp.f(yr_mean$`year(date)`, yr_mean$sst_degC) 
