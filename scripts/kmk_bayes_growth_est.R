library(AquaticLifeHistory)

gulf_dat <- subset(dat_all, stock_id_2 == 'GULF') |>
  subset(state!="MEX" & state!='MA' & state!='VA') |>
  subset(gear_common=='HL') |>
  subset(mode_2=="COM" | mode_2=='REC') |>
  subset(sex == 'F' & final_age>0 & final_age<11)


vb2 <- makeGrowthFun(type="von Bertalanffy",pname="Original")
vb1 <- makeGrowthFun(type="von Bertalanffy",pname="typical")

gulf_full1 <- nlsLM(fl_mm ~ vb1(final_age, Linf, K, t0), data=gulf_dat, 
                   start=c(Linf=1300, K=.2, t0=2),
                   control = nls.lm.control(maxiter = 1024, maxfev = 10000))
gulf_full_out1 <- rbind(coef(gulf_full1))
gulf_full_out1


gulf_full2 <- nlsLM(fl_mm ~ vb2(final_age, Linf, K, L0), data=gulf_dat, 
                   start=c(Linf=1300, K=.2, L0=2),
                   control = nls.lm.control(maxiter = 1024, maxfev = 10000))
gulf_full_out2 <- rbind(coef(gulf_full2))
gulf_full_out2
g2_preds <- data.frame(final_age = gulf_dat$final_age,
                       fl_mm = predict(gulf_full2))
g2_preds <- g2_preds[order(g2_preds$final_age),]

plot(jitter(gulf_dat$final_age), gulf_dat$fl_mm, 
     pch = 16, col = alpha(1, .1))
points(g2_preds$final_age, g2_preds$fl_mm, col = 2, typ = 'l')


growthdata <- subset(dat_all, stock_id_2 == 'GULF') |>
  subset(state!="MEX" & state!='MA' & state!='VA') |>
  subset(gear_common=='HL') |>
  subset(mode_2=="COM" | mode_2=='REC') |>
  subset(sex == 'F' & final_age>0, select = c('final_age', 'fl_mm')) |>
  rename(Age = final_age, Length = fl_mm)

Estimate_Growth(growthdata, n.bootstraps = 1000,
                Birth.Len = 2)
