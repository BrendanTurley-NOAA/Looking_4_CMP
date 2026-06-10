
library(MARSS)
library(readxl)


setwd("C:/Users/brendan.turley/Downloads")


us <- read.csv('sedar99-us-com-landings.csv', skip=1)
us$Gillnet..ww.lbs. <- gsub(',','',us$Gillnet..ww.lbs.) |> as.numeric()
us$Handline.Plus..ww.lbs. <- gsub(',','',us$Handline.Plus..ww.lbs.) |> as.numeric()

units(us$Gillnet..ww.lbs.) <- 'lb'
units(us$Handline.Plus..ww.lbs.) <- 'lb'
units(us$Gillnet..ww.lbs.) <- 't'
units(us$Handline.Plus..ww.lbs.) <- 't'


mx <- read_xlsx('Fishery landing by fleet of king mackerel 1970-2024.xlsx') |> as.data.frame()
units(mx$`Gillnet catch`) <- 't'
units(mx$`Trolling catch`) <- 't'
units(mx$`Total catch`) <- 't'


head(mx)

us_sub <- subset(us, Fishing.Year>=1970) |>
  setNames(c('year','us_gn','us_tr'))
mx_sub <- subset(mx, Year<=2023) |>
  setNames(c('year','mx_gn','mx_tr','mx_tot'))
mx_sub <- mx_sub[,-4]

mx_us <- merge(mx_sub,us_sub,by='year')
mx_us_std <- scale(mx_us[,2:5])

par(mfrow=c(2,3))
for(i in 1:4){
  plot(mx_us$year, mx_us_std[,i], typ = 'o',
       panel.first = abline(h=0,lty=5))
}


# https://atsa-es.github.io/atsa-labs/sec-dfa.html
# https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf

# 2. Define the DFA model with, for example, m = 2 trends
dfa_model <- list(m = 2, R = "diagonal and equal")

# 3. Fit the model using form="dfa"
# Note: For faster convergence, you can add method="BFGS" or method="TMB"
dfa_1 <- MARSS(mx_us_std, model = dfa_model, form = "dfa", control = list(maxit=1000))

## get the estimated ZZ
Z_est <- coef(dfa_1, type = "matrix")$Z
## get the inverse of the rotation matrix
H_inv <- varimax(Z_est)$rotmat
## rotate factor loadings
Z_rot = Z_est %*% H_inv
## rotate processes
proc_rot = solve(H_inv) %*% dfa_1$states

# ylbl <- phytoplankton
w_ts <- mx_us$year
mm <- 4
layout(matrix(c(1, 2, 3, 4, 5, 6), mm, 2), widths = c(2, 1))
## par(mfcol=c(mm,2), mai = c(0.5,0.5,0.5,0.1), omi =
## c(0,0,0,0))
par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))
## plot the processes
for (i in 1:mm) {
  ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
  ## set up plot area
  plot(w_ts, proc_rot[i, ], type = "n", bty = "L", ylim = ylm, 
       xlab = "", ylab = "", xaxt = "n")
  ## draw zero-line
  abline(h = 0, col = "gray")
  ## plot trend line
  lines(w_ts, proc_rot[i, ], lwd = 2)
  lines(w_ts, proc_rot[i, ], lwd = 2)
  ## add panel labels
  mtext(paste("State", i), side = 3, line = 0.5)
  axis(1, 12 * (0:dim(dat_1980)[2]) + 1, yr_frst + 0:dim(dat_1980)[2])
}
## plot the loadings
minZ <- 0
ylm <- c(-1, 1) * max(abs(Z_rot))
for (i in 1:mm) {
  plot(c(1:N_ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, 
                                                                    i]) > minZ, i]), type = "h", lwd = 2, xlab = "", ylab = "", 
       xaxt = "n", ylim = ylm, xlim = c(0.5, N_ts + 0.5), col = clr)
  for (j in 1:N_ts) {
    if (Z_rot[j, i] > minZ) {
      text(j, -0.03, ylbl[j], srt = 90, adj = 1, cex = 1.2, 
           col = clr[j])
    }
    if (Z_rot[j, i] < -minZ) {
      text(j, 0.03, ylbl[j], srt = 90, adj = 0, cex = 1.2, 
           col = clr[j])
    }
    abline(h = 0, lwd = 1.5, col = "gray")
  }
  mtext(paste("Factor loadings on state", i), side = 3, line = 0.5)
}
