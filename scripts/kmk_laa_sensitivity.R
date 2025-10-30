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
library(stringr)
library(viridis)


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



# Growth modeling ---------------------------------------------------------
library(fishgrowth)
library(FSA)
library(nlstools)

gulf_dat <- subset(dat_all, stock_id_2 == 'GULF')
satl_dat <- subset(dat_all, stock_id_2 == 'SATL')

plot(gulf_dat$final_age, gulf_dat$fl_mm, log = 'xy')
summary(lm(log(fl_mm) ~ log(final_age), data = subset(gulf_dat, final_age>0)))

init <- list(log_L1=log(700), log_L2=log(1000), log_k=log(0.2),
             log_sigma_min=log(3), log_sigma_max=log(3))
dat <- list(Aoto=gulf_dat$final_age, Loto=gulf_dat$fl_mm, t1=1, t2=10)
# vonbert_objfun(init, dat)
model <- vonbert(init, dat)
fit <- nlminb(model$par, model$fn, model$gr,
              control=list(eval.max=1e4, iter.max=1e4))
report <- model$report()
sdreport <- sdreport(model)


init <- list(log_Linf=log(1200), log_k=log(0.1), t0=-1,
             log_sigma_min=log(3), log_sigma_max=log(3))
dat <- list(Aoto=gulf_dat$final_age, Loto=gulf_dat$fl_mm)
# vonberto_objfun(init, dat)
model <- vonberto(init, dat)
fit <- nlminb(model$par, model$fn, model$gr,
              control=list(eval.max=1e5, iter.max=1e5))
report <- model$report()
sdreport <- sdreport(model)


vb1 <- makeGrowthFun(type="von Bertalanffy",pname="Typical")
vb1_form <- formula(fl_mm ~ Linf * (1 - exp(-K * (final_age - t0))))

vsv1 <- findGrowthStarts(fl_mm ~ final_age, data = gulf_dat,
                           type = "von Bertalanffy", pname = "Typical", plot = T)
vsv2 <- findGrowthStarts(fl_mm ~ final_age, data = gulf_dat,
                         type = "von Bertalanffy", pname = "Typical", plot = T,
                         fixed = c(t0 = -1))

resv1 <- nls(fl_mm ~ vb1(final_age, Linf, K, t0), data=gulf_dat, start=vsv1)
cbind(Est=coef(resv1),confint(resv1))

plot(fl_mm ~ final_age, data = gulf_dat)
lines(0:24, vb1(0:24,Linf = coef(resv1)[1], K = coef(resv1)[2], t0 = coef(resv1)[3]),
      col = 2, lwd = 2)

typ <- "von Bertalanffy"
prm <- "Francis"
vb2 <- makeGrowthFun(type=typ,pname=prm)
cv2 <- c(t1 = 2, t3 = 8)
rsv2  <- findGrowthStarts(fl_mm ~ final_age, data = gulf_dat,
                          type = typ, pname = prm,
                          constvals = cv2, plot = TRUE)
resv2 <- nls(fl_mm ~ vb2(final_age, L1, L2, L3, t1 = cv2),
             data = gulf_dat,
             start = rsv2)
cbind(Est = coef(resv2), confint(resv2))
r <- (coef(resv2)[3] - coef(resv2)[2]) / (coef(resv2)[2] - coef(resv2)[1])
r



# Growth per year ---------------------------------------------------------

yrs <- sort(unique(gulf_dat$year))
output <- list()
r <- rep(NA, length(yrs))
for(i in yrs){
  tmp <- subset(gulf_dat, year==i)
  
  vsv1 <- findGrowthStarts(fl_mm ~ final_age, data = tmp,
                           type = "von Bertalanffy", pname = "Typical", plot = F,
                           fixed=c(t0=-2))
  mtext(i)

  resv1 <- nls(fl_mm ~ vb1(final_age, Linf, K, t0), data=tmp, start=vsv1)
  output[[i-1985]] <- cbind(Est=coef(resv1),confint(resv1))
  
  ### alternative
  rsv2  <- findGrowthStarts(fl_mm ~ final_age, data = tmp,
                            type = typ, pname = prm,
                            constvals = cv2, plot = F) 
  mtext(i)

  resv2 <- nls(fl_mm ~ vb2(final_age, L1, L2, L3, t1 = cv2),
               data = tmp,
               start = rsv2)
  r[i-1985] <- (coef(resv2)[3] - coef(resv2)[2]) / (coef(resv2)[2] - coef(resv2)[1])
}

k_ts <- unlist(output)[seq(2,9*32,9)]

plot(yrs, k_ts, typ = 'o')
plot(yrs, r, typ = 'o')


yrs <- sort(unique(satl_dat$year))
output <- list()
r <- rep(NA, length(yrs))
for(i in yrs){
  tmp <- subset(satl_dat, year==i)
  
  vsv1 <- findGrowthStarts(fl_mm ~ final_age, data = tmp,
                           type = "von Bertalanffy", pname = "Typical", plot = F)
                           # fixed=c(t0=-2))
  mtext(i)

  resv1 <- nls(fl_mm ~ vb1(final_age, Linf, K, t0), data=tmp, start=vsv1)
  output[[i-1985]] <- cbind(Est=coef(resv1),confint(resv1))
  
  ### alternative
  rsv2  <- findGrowthStarts(fl_mm ~ final_age, data = tmp,
                            type = typ, pname = prm,
                            constvals = cv2, plot = F) 
  mtext(i)
  
  resv2 <- nls(fl_mm ~ vb2(final_age, L1, L2, L3, t1 = cv2),
               data = tmp,
               start = rsv2)
  r[i-1985] <- (coef(resv2)[3] - coef(resv2)[2]) / (coef(resv2)[2] - coef(resv2)[1])
}

k_ts <- unlist(output)[seq(2,9*32,9)]

plot(yrs, k_ts, typ = 'o')
plot(yrs, r, typ = 'o')



# Sensitivities -----------------------------------------------------------
library(minpack.lm)

### gear, mode, sex, state, age

gulf_dat_age <- subset(gulf_dat, final_age > 0 & final_age < 15)
satl_dat <- subset(dat_all, stock_id_2 == 'SATL')

yrs <- sort(unique(gulf_dat_age$year))
output <- list()
r <- rep(NA, length(yrs))
for(i in yrs){
  tmp <- subset(gulf_dat_age, year==i)
  
  # vsv1 <- findGrowthStarts(fl_mm ~ final_age, data = tmp,
  #                          type = "von Bertalanffy", pname = "Typical", plot = T)
  # mtext(i)
  
  resv1 <- try(nls(fl_mm ~ vb1(final_age, Linf, K, t0), 
               data=tmp, start=c(Linf=1200, K=.1, t0=-2),
               control = nls.control(minFactor = 1/4096, maxiter = 10000, printEval = T),
               algorithm = "port"), silent = TRUE)

  # resv1 <- try(nlsLM(vb1_form, data = tmp, start=c(Linf=1200, K=.1, t0=-2), jac = NULL,
  #       algorithm = "LM", control = nls.lm.control(maxiter = 1024, maxfev = 10000),
  #       lower = NULL, upper = NULL, trace = FALSE), silent = TRUE)
  
  if (inherits(resv1, "try-error")) {
    output[[i-1985]] <- matrix(NA,3,3)
    message(paste("Error in iteration", i, ": Skipping."))
  } else {
    output[[i-1985]] <- cbind(Est=coef(resv1),confint(resv1))
  }
  # output[[i-1985]] <- cbind(Est=coef(resv1),confint(resv1))
  
  ### alternative
  # rsv2  <- findGrowthStarts(fl_mm ~ final_age, data = tmp,
  #                           type = typ, pname = prm,
  #                           constvals = cv2, plot = T) 
  # mtext(i)
  
  # resv2 <- nls(fl_mm ~ vb2(final_age, L1, L2, L3, t1 = cv2),
  #              data = tmp, start = rsv2,
  #              control = nls.control(minFactor = 1/4096, maxiter = 200),
  #              algorithm = "port")
  # r[i-1985] <- (coef(resv2)[3] - coef(resv2)[2]) / (coef(resv2)[2] - coef(resv2)[1])
}

ks <- data.frame(year = yrs,
                 k_ts = unlist(output)[seq(2,9*32,9)],
                 lcl = unlist(output)[seq(5,9*32,9)],
                 ucl = unlist(output)[seq(8,9*32,9)]) #|>
  # na.omit()

plot(ks$year, log(ks$k_ts), typ = 'o', pch = 16)

plot(ks$year, ks$k_ts, typ = 'o', pch = 16,
     ylim = c(min(ks$lcl,na.rm=T), max(ks$ucl,na.rm=T)))
abline(h = mean(ks$k_ts,na.rm=T), lty = 5, col = 'gray')
points(ks$year, ks$lcl, typ = 'l', lty = 1, col = 2)
points(ks$year, ks$ucl, typ = 'l', lty = 1, col = 2)
grid()
### end


### gear, mode, sex, state, age


par(mfrow=c(3,3))

gulf_all <- findGrowthStarts(fl_mm ~ final_age, data = gulf_dat,
                          type = "von Bertalanffy", pname = "Typical", plot = T)

table(gulf_dat$gear_common)
gulf_gear <- subset(gulf_dat, gear_common=="HL" | gear_common=='GN')
table(gulf_gear$gear_common)
gg_vb <- findGrowthStarts(fl_mm ~ final_age, data = gulf_gear,
                         type = "von Bertalanffy", pname = "Typical", plot = T)

table(gulf_dat$mode_2)
gulf_mode <- subset(gulf_dat, mode_2=="COM" | mode_2=='REC')
table(gulf_mode$mode_2)
gm_vb <- findGrowthStarts(fl_mm ~ final_age, data = gulf_mode,
                          type = "von Bertalanffy", pname = "Typical", plot = T)

table(gulf_dat$state)
gulf_state <- subset(gulf_dat, state!="MEX" & state!='MS' & state!='AL')
table(gulf_state$state)
gs_vb <- findGrowthStarts(fl_mm ~ final_age, data = gulf_state,
                          type = "von Bertalanffy", pname = "Typical", plot = T)

table(gulf_dat$final_age)
gulf_age <- subset(gulf_dat, final_age<10 & final_age>0)
table(gulf_age$final_age)
ga_vb <- findGrowthStarts(fl_mm ~ final_age, data = gulf_age,
                          type = "von Bertalanffy", pname = "Typical", plot = T)

table(gulf_dat$sex)
gulf_sex <- subset(gulf_dat, sex=='F')
table(gulf_sex$sex)
gxf_vb <- findGrowthStarts(fl_mm ~ final_age, data = gulf_sex,
                          type = "von Bertalanffy", pname = "Typical", plot = T)
gulf_sexm <- subset(gulf_dat, sex=='M')
table(gulf_sexm$sex)
gxm_vb <- findGrowthStarts(fl_mm ~ final_age, data = gulf_sexm,
                          type = "von Bertalanffy", pname = "Typical", plot = T)


rbind(gulf_all,
      gg_vb,
      gm_vb,
      gs_vb,
      ga_vb,
      gxf_vb,
      gxm_vb)




dat_laa <- subset(gulf_dat, final_age > 1 & final_age < 9) |>
  subset(state!="MEX" & state!='MS' & state!='AL') |>
  subset(gear_common=="HL" | gear_common=='GN') |>
  subset(mode_2=="COM" | mode_2=='REC')

dat_laa <- subset(gulf_dat, state=='NWF' | state=='LA') |>
  subset(final_age > 1 & final_age < 9) |>
  subset(gear_common=="HL" | gear_common=='GN') |>
  subset(mode_2=="COM" | mode_2=='REC')

dat_laa <- subset(gulf_dat, final_age > 1 & final_age < 9) |>
  subset(state!="MEX" & state!='MS' & state!='AL') |>
  subset(gear_common=='GN') |>
  subset(mode_2=="COM" | mode_2=='REC')

dat_laa <- subset(gulf_dat, final_age > 1 & final_age < 9) |>
  subset(state!="MEX" & state!='MS' & state!='AL') |>
  subset(gear_common=='HL') |>
  subset(mode_2=="COM" | mode_2=='REC')


gulf_st_yr <- aggregate(fl_mm ~ year + state, data = dat_laa, length) |>
  arrange(year, state) |>
  reshape(idvar = 'state', timevar = 'year', direction = 'wide')
gulf_st_yr1 <- t(data.matrix(gulf_st_yr[,-1]))
gulf_st_yr1[which(is.na(gulf_st_yr1))] <- 0
gulf_st_pro1 <- (gulf_st_yr1)/rowSums(gulf_st_yr1)
colnames(gulf_st_pro1) <- gulf_st_yr$state
cols <- rev(turbo(ncol(gulf_st_pro1)))

par(mar=c(3,3,1,5))
b1 <- barplot(t(gulf_st_pro1), names.arg = 1986:2017, las = 2, col = cols)
legend(b1[length(b1)] + 1, 1, gulf_st_yr$state, fill = cols, xpd = T, bty = 'n', cex = .8)

boxplot(fl_mm ~ gear_common, data = dat_laa, varwidth = T)
boxplot(fl_mm ~ mode_2, data = dat_laa, varwidth = T)
boxplot(fl_mm ~ state, data = dat_laa, varwidth = T)
boxplot(final_age ~ state, data = dat_laa, varwidth = T)
barplot(table(dat_laa$state))

gulf_lth <- aggregate(fl_mm ~ year, data = dat_laa, mean)
gulf_lth_st <- aggregate(fl_mm ~ year + state, data = dat_laa, mean)
gulf_st_samp <- aggregate(fl_mm ~ year + state, data = dat_laa, length)
gulf_lth_yr <- aggregate(fl_mm ~ year + final_age, data = dat_laa, mean)
gulf_samp <- aggregate(fl_mm ~ year + final_age, data = dat_laa, length)

ages <- sort(unique(gulf_lth_yr$final_age))
states <- sort(unique(gulf_lth_st$state))

plot(fl_mm ~ year, data = gulf_lth_yr, typ = 'n')
grid()
for(i in ages){
  tmp <- subset(gulf_lth_yr, final_age==i)
  tmp2 <- subset(gulf_samp, final_age==i)
       points(tmp$year, tmp$fl_mm, typ = 'o', bg = i,
              cex = (tmp2$fl_mm)/mean((gulf_samp$fl_mm))*2, pch = 21)
              # cex = sqrt(tmp2$fl_mm)/2, pch = 21)
}
points(gulf_lth$year, gulf_lth$fl_mm, lwd = 5, typ = 'l')

pts <- rep(c(21,22,23,24,25),2)

plot(fl_mm ~ year, data = gulf_lth_st, typ = 'n')
grid()
for(i in states){
  tmp <- subset(gulf_lth_st, state==i)
  tmp2 <- subset(gulf_st_samp, state==i)
  points(tmp$year, tmp$fl_mm, typ = 'o', bg = which(i==states), col = which(i==states),
         cex = (tmp2$fl_mm)/mean((gulf_st_samp$fl_mm))*2, pch = 21)
         # cex = log(tmp2$fl_mm), pch = pts[which(i==states)])
}
points(gulf_lth$year, gulf_lth$fl_mm, lwd = 5, typ = 'l')
legend('topright',states, pch = pts[1:9], pt.bg = 1:9)


ages <- sort(unique(gulf_lth_yr$final_age))
years <- sort(unique(gulf_lth_yr$year))


### cohort plots
### for each year, pull age 2, create a vector (year, length) for that class
m <- 1
n <- 0
cohorts <- matrix(NA,7*length(years),4) |> as.data.frame() |>
  setNames(c('year','final_age','fl_mm','cohort'))
for(i in years){
  out <- mapply(function(x,y) subset(gulf_lth_yr, year==x & final_age==y),
                i:(i+6),2:8, SIMPLIFY = T) |> 
    unlist()
  out <- matrix(out, 3, length(out)/3) |> t() |> as.data.frame()
  n <- n + nrow(out)
  cohorts[m:n, ] <- cbind(out, i)
  m <- n + 1
}
cohorts <- cohorts[which(!is.na(cohorts$year)), ]

plot(fl_mm ~ year, data = gulf_lth_yr, typ = 'n')
grid()
for(i in ages){
  tmp <- subset(gulf_lth_yr, final_age==i)
  tmp2 <- subset(gulf_samp, final_age==i)
  points(tmp$year, tmp$fl_mm, typ = 'o', bg = i,
         cex = (tmp2$fl_mm)/mean((gulf_samp$fl_mm))*2, pch = 21)
  # cex = sqrt(tmp2$fl_mm)/2, pch = 21)
}
points(gulf_lth$year, gulf_lth$fl_mm, lwd = 5, typ = 'l')

colc <- colorRampPalette(c('gray','purple2'))(length(years))
for(i in years){
  tmp <- subset(cohorts, cohort==i)
  points(tmp$year, tmp$fl_mm, typ='l', lwd = 3, col = colc[which(i==years)])
}

plot(fl_mm ~ final_age, data = gulf_lth_yr, typ = 'n')
for(i in years){
  tmp <- subset(cohorts, cohort==i)
  points(tmp$final_age, tmp$fl_mm, typ='o', lwd = 3, col = colc[which(i==years)])
  # abline(lm(log10(fl_mm) ~ log10(final_age),data=tmp), col = colc[which(i==years)])
}



# Growth: length versus weight --------------------------------------------
# The length-weight relationship equation.
# Equation: \(W=a*L^{b}\)\
# (W\): Weight of the fish\
# (L\): Length of the fish\
# (a\): A constant, often near zero\
# (b\): An exponent that shows how weight increases relative to length
# If \(b=3\), it represents isometric growth, where the fish grows proportionally in length and weight.
# If \(b<3\), it indicates negative allometric growth, meaning the fish becomes slimmer as it gets longer.
# If \(b>3\), it indicates positive allometric growth, meaning the fish gains weight more quickly than its length.

dat_laa <- subset(gulf_dat, final_age > 1 & final_age < 9) |>
  subset(state!="MEX" & state!='MS' & state!='AL') |>
  subset(gear_common=="HL" | gear_common=='GN') |>
  subset(mode_2=="COM" | mode_2=='REC')

dat_laa$wh_kg[which(dat_laa$wh_kg>100)] <- NA
boxplot(wh_kg ~ gear_common, data = dat_laa, varwidth = T)
boxplot(wh_kg ~ mode_2, data = dat_laa, varwidth = T)
boxplot(wh_kg ~ state, data = dat_laa, varwidth = T)


library(propagate)
library(scales)

plot(dat_all$fl_mm, dat_all$wh_kg, ylim = c(0,35), pch = 16, col = alpha(1, .2))
points(dat_laa$fl_mm, dat_laa$wh_kg, pch = 16, , col = alpha(4, .2))

lw_eq <- formula(wh_kg ~ a * fl_mm ^ b)

lw_res_all <- nls(lw_eq, data = dat_all, start = c(a = .5, b = 3))
summary(lw_res_all)
residuals(lw_res_all) |> hist()
preds_all <- data.frame(x = dat_all$fl_mm[-lw_res_all$na.action],
                    y = predict(lw_res_all,newdata = dat_all$fl_mm)) |>
  arrange(x)

lw_res_laa <- nls(lw_eq, data = dat_laa, start = c(a = .5, b = 3))
summary(lw_res_laa)
residuals(lw_res_laa) |> hist()
preds_laa <- data.frame(x = dat_laa$fl_mm[-lw_res_laa$na.action],
                    y = predict(lw_res_laa,newdata = dat_laa$fl_mm)) |>
  arrange(x)
# preds2 <- predictNLS(lw_res_all, interval = c("confidence"), alpha = 0.05, nsim = 10000)

dat_laa_om <- dat_laa[-lw_res_laa$na.action, ]
dat_laa_om$resid <- residuals(lw_res_laa)

boxplot(resid ~ year, data = dat_laa_om)
abline(h=0, lwd = 1.5, col = 2, lty = 5)
boxplot(wh_kg ~ year, data = dat_laa_om)
boxplot(wh_kg ~ final_age, data = dat_laa_om)


plot(dat_laa$fl_mm, dat_laa$wh_kg, ylim = c(0,35))
lines(preds$x, preds$y, lwd = 3, col = 2)

plot(dat_all$fl_mm, dat_all$wh_kg, ylim = c(0,35), pch = 16, col = alpha(1, .2))
points(dat_laa$fl_mm, dat_laa$wh_kg, pch = 16, , col = alpha(4, .2))
lines(preds_all$x, preds_all$y, lwd = 3, col = 2)
lines(preds_laa$x, preds_laa$y, lwd = 3, col = 3)



# GSI ---------------------------------------------------------------------

table(dat_38u$REPRO_PHASE, dat_38u$CATCH_MONTH)

gsi <- dat_38u$GONAD_WEIGHT_FRESH_G/dat_38u$WHOLE_WEIGHT_G

boxplot(gsi~dat_38u$CATCH_YEAR)
boxplot(gsi~dat_38u$CATCH_MONTH)
boxplot(gsi~dat_38u$STATE_LANDED)
