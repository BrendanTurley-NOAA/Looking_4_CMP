### this script will perform a length-at-age sensitivity analysis
### the data need to be characterized by sample sizes for components that are exmained
### Chris and Bev recommended the following:
### 1. look at sample sizes across years, ages, and regions/states
### 2. remove tournament and science survey samples
### 3. double check that the data use current mixing zone definitions

source('C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/scripts/helper_fxns.R')

library(dplyr)
library(here)
library(lubridate)
library(readxl)
library(scales)
library(stringr)
library(viridis)
library(fishgrowth)
library(FSA)
library(nlstools)
library(vctrs)
library(minpack.lm)
library(MASS)


setwd(paste0(here()))
setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")
### sedar38 data: 1986-2013
dat_38 <- read_xlsx('SEDAR38 PCLAB Age File New Mixing Zone_v2.xlsx', sheet = 1, guess_max = 5e5)
dat_38$`Gut Kg`[which(dat_38$`Gut Kg`=='adv to 1 yr')] <- NA
dat_38$`Gut Kg` <- as.numeric(dat_38$`Gut Kg`)
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

# dat3 <- subset(dat_38, sex == 'M' | sex == 'F') |>
#   # subset(final_age < 9 & final_age > 1) |>
#   subset(gear == 'HL') |>
#   subset(state != 'MEX')


### SEDAR38U data: 2013-2017
setwd("C:/Users/brendan.turley/Documents/CMP/data/kmk_lengths")
dat_38u <- read_xlsx('2019SEDAR38UAge_Data.xlsx', sheet = 2, guess_max = 1e5)

# dat2 <- subset(dat_38u, MACRO_SEX == 'M' | MACRO_SEX == 'F') |>
#   subset(GEAR_GROUP_CODE == 'HL') #|>
#   # subset(CALENDAR_AGE < 9 & CALENDAR_AGE > 1)



# Combine 38 & 38U data ---------------------------------------------------

### which columns are common and needed for length-at-age growth over time?
# 1. fishing year (drop because 38U does not have this column)
# 2. year
# 3. month
# 4. state
# 5. gear
# 6. sex
# 7. mode
# 8. stock ID
# 9. length
# 10. age
# 11. weight

names(dat_38)
col_keep_38 <- c(1,7,11,16,17,18,19,20,21,22,24,25)
names(dat_38)[col_keep_38]
dat_38r <- dat_38[ ,col_keep_38]
dat_38r <- subset(dat_38r, year<2013)

names(dat_38u)
col_keep_38u <- c(4,7,8,13,14,15,16,24,32,40,41,48,51)
names(dat_38u)[col_keep_38u]
dat_38ur <- dat_38u[ ,col_keep_38u]

### common gears: HL, GN, TRW, OTH, UNK
oth1 <- c(120, 200, 943, 989, 'LL', 'PDV', 'SP')
dat_38r$gear_common <- dat_38r$gear
dat_38r$gear_common[which(dat_38r$gear=='TRAWL')] <- 'TRW'
dat_38r$gear_common[which(is.element(dat_38r$gear, oth1))] <- 'OTH'
dat_38r$gear_common[which(dat_38r$gear=='Unknown')] <- 'UNK'
# table(dat_38r$gear_common)

dat_38ur$gear_common <- dat_38ur$GEAR_GROUP_CODE
dat_38ur$gear_common[which(dat_38ur$gear_common=='SP')] <- 'OTH'
dat_38ur$gear_common[which(dat_38ur$gear_common=='UA')] <- 'UNK'
# table(dat_38ur$gear_common)

### reorder columns: date, year, month, stock id, mode/fishery, gear, sex, lth, wh-wt, gut-wt, age
dat_38ur$date2 <- as.Date(paste(dat_38ur$CATCH_YEAR, dat_38ur$CATCH_MONTH, dat_38ur$CATCH_DAY, sep = '-'))
dat_38ur$wh_kg <- dat_38ur$WHOLE_WEIGHT_G/1000
dat_38ur$gut_kg <- dat_38ur$GUTTED_WEIGHT_G/1000

keep_38r <- c(11,1,2,12,3,5,13,7,6,9,10,8)
keep_38ur <- c(15,6,7,4,1,3,14,13,9,16,17,12)
# cbind(names(dat_38r)[keep_38r],
# names(dat_38ur)[keep_38ur])
dat_38rr <- dat_38r[ ,keep_38r] 
dat_38urr <- dat_38ur[ ,keep_38ur]
names(dat_38urr) <- names(dat_38rr)
dat_all <- rbind(dat_38rr, dat_38urr)

dat_all$stock_id_2[grep('atlantic',dat_all$stock_id_2,ignore.case = T)] <- 'SATL'
dat_all$stock_id_2[grep('gulf',dat_all$stock_id_2,ignore.case = T)] <- 'GULF'
dat_all$stock_id_2[grep('mix',dat_all$stock_id_2,ignore.case = T)] <- 'MIXING'
dat_all$sex[grep('[dinu]',dat_all$sex,ignore.case = T)] <- 'U'
dat_all$mode_2[which(dat_all$mode_2=='CM')] <- 'COM'



# Growth per year ---------------------------------------------------------

vb2 <- makeGrowthFun(type="von Bertalanffy",pname="Original")


gulf_dat <- subset(dat_all, stock_id_2 == 'GULF') |>
  subset(state!="MEX" & state!='MA' & state!='VA') |>
  subset(gear_common=='HL') |>
  subset(mode_2=="COM" | mode_2=='REC') |>
  subset(final_age>1 & sex == 'F' & final_age<10)

gulf_full <- nlsLM(fl_mm ~ vb2(final_age, Linf, K, L0), data=gulf_dat, 
                   start=c(Linf=1300, K=.2, L0=10),
                   control = nls.lm.control(maxiter = 1024, maxfev = 10000))
gulf_full_out <- rbind(coef(gulf_full))
gulf_full_out

yrs <- sort(unique(gulf_dat$year))
per_yr <- rep(NA, length(yrs))
r <- rep(NA, length(yrs))
for(i in yrs){
  tmp <- subset(gulf_dat, year==i)
  
  resv1 <- nlsLM(fl_mm ~ gulf_full_out[1] - (gulf_full_out[1] - gulf_full_out[3]) * exp(-K * final_age),
                 data=tmp, 
                 start=c(K=.2),
                 control = nls.lm.control(maxiter = 1024, maxfev = 10000))
  per_yr[i-1985] <- rbind(coef(resv1))
}

# plot(yrs, per_yr, typ = 'o')
# points(smooth.spline(yrs, per_yr, spar = .5), typ = 'l', col = 2, lwd = 2)


### cohort plots

# gulf_dat <- subset(dat_all, stock_id_2 == 'GULF') |>
#   subset(state!="MEX" & state!='MA' & state!='VA') |>
#   subset(gear_common=='HL') |>
#   subset(mode_2=="COM" | mode_2=='REC') |>
#   subset(final_age>1 & sex == 'F' & final_age<10)
# plot(gulf_dat$final_age, gulf_dat$fl_mm)
# plot(gulf_dat$year, gulf_dat$fl_mm)
# image(kde2d(gulf_dat$year, gulf_dat$final_age, 
#             n = c(length(unique(gulf_dat$year)),length(unique(gulf_dat$final_age)))), asp = 2)

vb1 <- makeGrowthFun(type="von Bertalanffy",pname="Typical")
vb1_form <- formula(fl_mm ~ Linf * (1 - exp(-K * (final_age - t0))))

gulf_full <- nlsLM(fl_mm ~ vb2(final_age, Linf, K, L0), data=gulf_dat, 
                   start=c(Linf=1300, K=.2, L0=10),
                   control = nls.lm.control(maxiter = 1024, maxfev = 10000))
gulf_full_out <- rbind(coef(gulf_full))
gulf_full_out

years <- sort(unique(gulf_dat$year))
cohort <- rep(NA, length(years))
span <- sort(unique(gulf_dat$final_age))
for(i in years){
  out <- mapply(function(x,y) subset(gulf_dat, year==x & final_age==y),
                i:(i+length(span)-1),span, SIMPLIFY = F)
  co_i <- do.call(rbind, out)
  
  resv1 <- nlsLM(fl_mm ~ gulf_full_out[1] - (gulf_full_out[1] - gulf_full_out[3]) * exp(-K * final_age),
                 data=co_i, 
                 start=c(K=.2),
                 control = nls.lm.control(maxiter = 1024, maxfev = 10000))
  cohort[i-1985] <- rbind(coef(resv1))
  
}


plot(yrs, per_yr, typ = 'o', pch = 16, col = 1,
     panel.first = grid())
points(smooth.spline(yrs, per_yr, spar = .5), typ = 'l', col = 2, lwd = 2)

points(years, cohort, typ = 'o', pch = 16, col = 4)
points(smooth.spline(years, cohort, spar = .5), typ = 'l', col = 3, lwd = 2)



library(strucchange) 

find.bp.f= function(x, y, method="BIC"){ 
  library(strucchange) 
  if (method=="BIC"){ 
    bp= breakpoints(y ~ x) 
    bp.t= breakpoints(bp)$breakpoints 
    bp.times= x[bp.t] 
    line.prediction= fitted(bp) 
  } 
  if (method=="F.test"){ 
    bp <- Fstats(y ~ x) 
    bp.x= breakpoints(bp)$breakpoints 
    bp.times= x[bp.t] 
  } 
  
  plot(x,y,type="p",las=1,
       xlab="Year",ylab="Number of Days Open")#,
       # xaxt="n",las=1)
  abline(v=bp.times,col="grey",lty=2,lwd=2) 
  lines(x,fitted(bp),lwd=2) # need to make this for the F test too. 
  if (is.na(bp.times[1])) message("no credible breakpoint by chosen method") else bp.times 
} 


find.bp.f(yrs, per_yr)
find.bp.f(yrs, cohort)


### NWF only

gulf_dat <- subset(dat_all, stock_id_2 == 'GULF') |>
  # subset(state=='NWF') |>
  # subset(state=='NWF' | state=='SWF' | state=='WF' | state=='SF') |>
  subset(state=='TX' | state=='LA' | state=='AL' | state=='MS') |>
  subset(gear_common=='HL') |>
  subset(mode_2=="COM" | mode_2=='REC') |>
  subset(final_age>1 & sex == 'F' & final_age<10) #|>
  # subset(year>1990)

plot(gulf_dat$final_age, gulf_dat$fl_mm, col = alpha(1, .1), pch = 16)
gulf_full <- nlsLM(fl_mm ~ vb2(final_age, Linf, K, L0), data=gulf_dat, 
                   start=c(Linf=1200, K=.2, L0=100),
                   control = nls.lm.control(maxiter = 1024, maxfev = 10000))
gulf_full_out <- rbind(coef(gulf_full))
gulf_full_out

yrs <- sort(unique(gulf_dat$year))
per_yr <- rep(NA, length(yrs))
r <- rep(NA, length(yrs))
for(i in yrs){
  tmp <- subset(gulf_dat, year==i)
  
  resv1 <- nlsLM(fl_mm ~ gulf_full_out[1] - (gulf_full_out[1] - gulf_full_out[3]) * exp(-K * final_age),
                 data=tmp,
                 start=c(K=.2),
                 control = nls.lm.control(maxiter = 1024, maxfev = 10000))
  per_yr[i-(yrs[1]-1)] <- rbind(coef(resv1))
}

if(length(yrs)<length(per_yr)){
  yrs <- yrs[1]:yrs[length(yrs)]
}

plot(yrs, per_yr, typ = 'o', pch = 16, col = 1)
points(smooth.spline(yrs, per_yr, spar = .5), typ = 'l', col = 2, lwd = 2)

summary(lm(per_yr ~ yrs))
