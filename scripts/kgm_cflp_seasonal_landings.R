
library(dplyr)
library(ggplot2)

#### read data and subset ####--------------------------------------------------
gom_st <- c('FL', 'AL', 'MS', 'LA', 'TX') |> sort()

setwd("~/CMP/data/cflp")
cflp <- readRDS('CFLPblake.rds')
cflp <- subset(cflp, LAND_YEAR>1998 & CATCH_TYPE == 'CATCH') |>
  subset(REGION == 'GOM' & is.element(ST_ABRV, gom_st)) |>
  subset(AREA_FISHED!='1' & AREA_FISHED!='2' & !is.na(AREA_FISHED)) |>
gc()

kgm <- subset(cflp, COMMON_NAME=='MACKERELS, KING AND CERO')
# names(cflp) <- tolower(names(cflp))



mth_land <- aggregate(kgm$TOTAL_WHOLE_POUNDS, by=list(kgm$LAND_MONTH), median, na.rm = T) |>
  setNames(c('month', 'catch'))

barplot(mth_land$catch)


mth_state <- aggregate(kgm$TOTAL_WHOLE_POUNDS, by=list(kgm$LAND_MONTH, kgm$ST_ABRV),
                       mean, na.rm = T) |>
  setNames(c('month', 'state', 'catch'))

### percentage landed per month per state
st_mth_pct <- mth_state |>
  group_by(state) |>
  mutate(pct = catch/sum(catch, na.rm = T))

plot(st_mth_pct$month, st_mth_pct$pct, col=as.factor(st_mth_pct$state), pch=19, cex=1.5,
     xlab='Month', ylab='Percentage of Landings', main='Percentage of Landings per Month per State')
for(i in gom_st){
  lines(st_mth_pct$month[st_mth_pct$state==i], st_mth_pct$pct[st_mth_pct$state==i], 
        col=which(i==gom_st), lwd=2)
}
legend('topright', legend=gom_st, col=1:length(gom_st), lwd=2, bty='n')


### percentage landed per month each year by state

kgm |>
  group_by(LAND_YEAR, LAND_MONTH, ST_ABRV) |>
  summarise(catch = sum(TOTAL_WHOLE_POUNDS, na.rm = TRUE), .groups = "drop") |>
  group_by(LAND_YEAR, ST_ABRV) |>
  mutate(
    pct = catch / sum(catch, na.rm = TRUE),
    LAND_MONTH = factor(LAND_MONTH, levels = 1:12, labels = month.abb)
  ) |>
  ungroup() |>
  ggplot(aes(x = LAND_YEAR, y = pct, fill = LAND_MONTH)) +
  geom_col()+
  scale_fill_viridis_d(option = 'H') +
  facet_wrap(~ST_ABRV) +
  labs(
    title = "Percentage of Landings per Month per State by Year",
    x = "Year",
    y = "Percentage of Landings",
    fill = "Month"
  ) +
  theme_minimal()
setwd("~/R_projects/Looking_4_CMP/figs")
ggsave('kgm_st_mth.png',
       width = 8,
       height = 5,
       units = 'in',
       dpi = 300)

kgm |>
  group_by(LAND_YEAR, LAND_MONTH, ST_ABRV) |>
  summarise(catch = sum(TOTAL_WHOLE_POUNDS, na.rm = TRUE), .groups = "drop") |>
  group_by(LAND_YEAR, ST_ABRV) |>
  mutate(
    pct = catch / sum(catch, na.rm = TRUE),
    LAND_MONTH = factor(LAND_MONTH, levels = 1:12, labels = month.abb)
  ) |>
  ungroup() |>
  ggplot(aes(x = LAND_YEAR, y = pct, color = LAND_MONTH, group = LAND_MONTH)) +
  geom_line(linewidth = 1.2) +
  scale_color_viridis_d(option = 'H') +
  facet_wrap(~ST_ABRV) +
  labs(
    title = "Percentage of Landings per Month per State by Year",
    x = "Year",
    y = "Percentage of Landings",
    color = "Month"
  ) +
  theme_minimal()


library(dplyr)
library(tidyr)
library(ggplot2)

slopes_df <- kgm |>
  group_by(LAND_YEAR, LAND_MONTH, ST_ABRV) |>
  summarise(catch = sum(TOTAL_WHOLE_POUNDS, na.rm = TRUE), .groups = "drop") |>
  # Fill missing months within active state-years as 0 so zero-catch months affect the slope
  complete(nesting(LAND_YEAR, ST_ABRV), LAND_MONTH = 1:12, fill = list(catch = 0)) |>
  group_by(LAND_YEAR, ST_ABRV) |>
  mutate(
    pct = catch / sum(catch, na.rm = TRUE),
    LAND_MONTH = factor(LAND_MONTH, levels = 1:12, labels = month.abb)
  ) |>
  ungroup() |>
  mutate(LAND_YEAR = as.numeric(as.character(LAND_YEAR))) |>
  group_by(ST_ABRV, LAND_MONTH) |>
  summarise(
    slope = if (sum(!is.na(pct)) >= 2) coef(lm(pct ~ LAND_YEAR))[2] else NA_real_,
    .groups = "drop"
  )

ggplot(slopes_df, aes(x = LAND_MONTH, y = ST_ABRV, fill = slope)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", slope)), size = 2.8) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = 0,
    na.value = "grey90"
  ) +
  coord_fixed() +
  labs(
    title = "Annual Trend (Slope) in Monthly Share of Landings by State",
    x = "Month",
    y = "State",
    fill = "Slope\n(Δ Share/Yr)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())
setwd("~/R_projects/Looking_4_CMP/figs")
ggsave('kgm_st_mth_trend.png',
       width = 7,
       height = 5,
       units = 'in',
       dpi = 300)
