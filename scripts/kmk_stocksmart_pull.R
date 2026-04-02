

pak::pak("NOAA-EDAB/stocksmart")

library(stocksmart)


kmk <- subset(stock_assessment_data, scientific_name == 'Scomberomorus cavalla')

table(kmk$regional_ecosystem, kmk$assessment_year)

gulf_kmk_r2020 <- subset(kmk, 
                         regional_ecosystem == 'Gulf of Mexico'&
                           assessment_year == '2020' &
                           metric == 'Recruitment' &
                           year > 1940,
                         select = c('year', 'value', 'units')) |>
  mutate(logR = log(value))

sa_kmk_r2020 <- subset(kmk, 
                       regional_ecosystem == 'Southeast Shelf'&
                         assessment_year == '2020' &
                         metric == 'Recruitment' &
                         year > 1940,
                       select = c('year', 'value', 'units')) |>
  mutate(logR = log(value))

gulf_kmk_r2014 <- subset(kmk, 
                         regional_ecosystem == 'Gulf of Mexico'&
                           assessment_year == '2014' &
                           metric == 'Recruitment' &
                           year > 1940,
                         select = c('year', 'value', 'units')) |>
  mutate(logR = log(value))

sa_kmk_r2014 <- subset(kmk, 
                       regional_ecosystem == 'Southeast Shelf'&
                         assessment_year == '2014' &
                         metric == 'Recruitment' &
                         year > 1940,
                       select = c('year', 'value', 'units')) |>
  mutate(logR = log(value))

plot(gulf_kmk_r2020$year, gulf_kmk_r2020$logR - gulf_kmk_r2020$logR[1],
     typ = 'o', ylim = c(-1.5, 1.5), lwd = 2,
     panel.first = abline(h = 0, lty = 5))
points(sa_kmk_r2020$year, sa_kmk_r2020$logR - sa_kmk_r2020$logR[1],
       typ = 'o',  col = 2, lwd = 2)

plot(gulf_kmk_r2014$year, gulf_kmk_r2014$logR - gulf_kmk_r2014$logR[1],
     typ = 'o', ylim = c(-1.5, 1.5), lwd = 2,
     panel.first = abline(h = 0, lty = 5))
points(sa_kmk_r2014$year, sa_kmk_r2014$logR - sa_kmk_r2014$logR[1],
       typ = 'o',  col = 2, lwd = 2)




subset(kmk, metric == 'Recruitment') |> View()

with(subset(kmk, metric == 'Recruitment'),
     plot(year, value))

get_species_itis(stock = "King mackerel")

plot_ts(itis = 172435,
        stock = "King mackerel - Gulf of Mexico ",
        metric = "Catch",
        printfig = FALSE)
