
library(lubridate)
library(readxl)

setwd("C:/Users/brendan.turley/Documents/CMP/data/ICCAT")
dat <- read_xlsx('_tagSMT_20250131.xlsx', skip = 4)

kmk_tag <- subset(dat, speciescode=='KGM')

kmk <- subset(dat, speciescode=='KGM') |>
  subset(RcYear!='No Recovery')

nrow(kmk) / nrow(kmk_tag)

table(kmk_tag$ReLenType)
table(kmk_tag$RcLenType)
table(kmk_tag$ReYear)
table(kmk_tag$RcYear)

### use gear codes to assess catchability / selectivity

kmk$tal <- (kmk$RcDate - kmk$ReDate) |> as.numeric(units = 'days')
hist(kmk$tal)
summary(kmk$tal)

subset(kmk, tal<366) |> nrow()
subset(kmk, tal>366) |> nrow()

which(!is.na(kmk$RcLenCM) & !is.na(kmk$ReLenCM)) |> length()
kmk_lth <- subset(kmk, !is.na(RcLenCM) & !is.na(ReLenCM)) |>
  subset(RcLenType=='SFL' | RcLenType=='TLE' | RcLenType=='LJF') |>
  subset(ReLenType=='SFL' | ReLenType=='TLE' | ReLenType=='LJF')
table(kmk_lth$RcLenType, kmk_lth$ReLenType)
### assume LJF (lower jaw-fork length) ~ TLE, but slightly smaller

plot(kmk$ReLonX, kmk$ReLatY, bg = 3, pch = 24, asp = 1,
     xlim=c(-100,-70), ylim=c(17,40))
# points(kmk$RcLonX, kmk$RcLatY, bg = 2, pch = 21)
arrows(kmk$ReLonX, kmk$ReLatY,
       kmk$RcLonX, kmk$RcLatY, length = .1)
points(kmk$RcLonX, kmk$RcLatY, bg = 2, pch = 21)

hist(month(kmk$ReDate))
hist(month(kmk$RcDate))

par(mfrow=c(2,2))
with(subset(kmk_tag, month(ReDate)==12 | month(ReDate)<3),
     plot(ReLonX,ReLatY, bg = 3, pch = 24, asp = 1,
          xlim=c(-100,-70), ylim=c(17,40)))

with(subset(kmk_tag, month(ReDate)>2 & month(ReDate)<6),
     plot(ReLonX,ReLatY, bg = 3, pch = 24, asp = 1,
          xlim=c(-100,-70), ylim=c(17,40)))

with(subset(kmk_tag, month(ReDate)>5 & month(ReDate)<9),
     plot(ReLonX,ReLatY, bg = 3, pch = 24, asp = 1,
          xlim=c(-100,-70), ylim=c(17,40)))

with(subset(kmk_tag, month(ReDate)>8 & month(ReDate)<12),
     plot(ReLonX,ReLatY, bg = 3, pch = 24, asp = 1,
          xlim=c(-100,-70), ylim=c(17,40)))

tal_thres <-  90 #75
kmk$ReFleetCode <- factor(kmk$ReFleetCode, levels = c('UNCL.FLEETS','MEX','USA'))
kmk$RcFleetCode <- factor(kmk$RcFleetCode, levels = c('UNCL.FLEETS','MEX','USA'))

par(mfrow=c(2,2))
plot(kmk$ReLonX, kmk$ReLatY, typ='n', asp = 1,
     xlim=c(-100,-70), ylim=c(17,40))
with(subset(kmk, tal<tal_thres & month(RcDate)==12 | month(RcDate)<3),
     arrows(ReLonX, ReLatY,
            RcLonX, RcLatY, length = .1))
with(subset(kmk, tal<tal_thres & month(RcDate)==12 | month(RcDate)<3),
     points(RcLonX,RcLatY, bg = RcFleetCode, pch = as.numeric(ReFleetCode)+23))

plot(kmk$ReLonX, kmk$ReLatY, typ='n', asp = 1,
     xlim=c(-100,-70), ylim=c(17,40))
with(subset(kmk, tal<tal_thres & month(RcDate)>2 & month(RcDate)<6),
     arrows(ReLonX, ReLatY,
            RcLonX, RcLatY, length = .1))
with(subset(kmk, tal<tal_thres & month(RcDate)>2 & month(RcDate)<6),
     points(RcLonX,RcLatY, bg = RcFleetCode, pch = as.numeric(ReFleetCode)+23))

plot(kmk$ReLonX, kmk$ReLatY, typ='n', asp = 1,
     xlim=c(-100,-70), ylim=c(17,40))
with(subset(kmk, tal<tal_thres & month(RcDate)>5 & month(RcDate)<9),
     arrows(ReLonX, ReLatY,
            RcLonX, RcLatY, length = .1))
with(subset(kmk, tal<tal_thres & month(RcDate)>5 & month(RcDate)<9),
     points(RcLonX,RcLatY, bg = RcFleetCode, pch = as.numeric(ReFleetCode)+23))

plot(kmk$ReLonX, kmk$ReLatY, typ='n', asp = 1,
     xlim=c(-100,-70), ylim=c(17,40))
with(subset(kmk, tal<tal_thres & month(RcDate)>8 & month(RcDate)<12),
     arrows(ReLonX, ReLatY,
            RcLonX, RcLatY, length = .1))
with(subset(kmk, tal<tal_thres & month(RcDate)>8 & month(RcDate)<12),
     points(RcLonX,RcLatY, bg = RcFleetCode, pch = as.numeric(ReFleetCode)+23))


boxplot(month(kmk$ReDate)~month(kmk$RcDate))


### how many are released in MX and recovered in USA &
### how many are released in USA and recovered in MX
table(kmk$ReFleetCode, kmk$RcFleetCode) |> addmargins()
table(kmk$ReFleetCode, kmk$ReDate |> month())
table(kmk$RcFleetCode, kmk$RcDate |> month())

with(subset(kmk, tal<tal_thres),
     table(ReFleetCode, RcFleetCode) |> addmargins())
with(subset(kmk, tal<tal_thres),
     table(ReFleetCode, ReDate |> month()))
with(subset(kmk, tal<tal_thres),
     table(RcFleetCode, RcDate |> month()))

