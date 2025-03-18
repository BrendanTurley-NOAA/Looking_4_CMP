

library(lubridate)
library(readxl)
library(rotl)

setwd("C:/Users/brendan.turley/Documents/CMP/data/")
dat <- read_excel('GoMexSI_diet_data_2-2-18.xlsx')

kmk <- subset(dat, PRED_SOURCE_NAME=='Scomberomorus cavalla')

sort(table(kmk$PREY_DATABASE_NAME))

taxa <- unique(kmk$PREY_DATABASE_NAME)
taxa <- gsub(' spp.', '', taxa)
taxa <- gsub(' sp.', '', taxa)
taxa <- sort(unique(taxa))
taxa[which(taxa=='Decabrachia')] <- 'Decapodiformes'
taxa <- taxa[-which(taxa=='Organic matter')]

resolved_names <- tnrs_match_names(taxa)
resolved_names 

inspect(resolved_names, taxon_name = 'Teuthida')
resolved_names <- update(resolved_names,
                         taxon_name = "Teuthida",
                         new_row_number = 1
)

info <- taxonomy_taxon_info(5521375)
tax_rank(info)

my_tree <- tol_induced_subtree(ott_ids = resolved_names$ott_id[-c(38,66)])

plot(my_tree, no.margin = TRUE)
