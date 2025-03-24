
library(FishLife)

# https://james-thorson-noaa.github.io/FishLife/articles/Access.html

edge_names = c( FishBase_and_Morphometrics$tree$tip.label,
                FishBase_and_Morphometrics$tree$node.label[-1] ) # Removing root

#
which_g = match( "Scomberomorus cavalla", edge_names )
Table2023 = cbind( 
  Mean = FishBase_and_Morphometrics$beta_gv[which_g,],
  SE = sqrt(diag(FishBase_and_Morphometrics$Cov_gvv[which_g,,]))
)
knitr::kable( Table2023, digits=3)



Taxa = Search_species(Genus = "Scomberomorus",
                       Species = "cavalla")$match_taxonomy
Predict = Plot_taxa(Taxa,
                     mfrow=c(3,2) )


Taxa = c( Search_species(Genus="Scomberomorus",Species="cavalla",add_ancestors=FALSE)$match_taxonomy,
          Search_species(Genus="Scomberomorus",Species="maculatus",add_ancestors=FALSE)$match_taxonomy )
Plot_taxa( Taxa, mfrow=c(3,2) )


Taxa = Search_species( Genus = "Scomberomorus",source('C:/Users/brendan.turley/Documents/R_projects/Looking_4_CMP/scripts/kmk_convert_2fl.R')

library(dplyr)
library(lubridate)
library(readxl)
library(stringr)
                       Species="cavalla",
                       Database = FishBase )$match_taxonomy
params = matrix( c( "Loo", "K", "Winfinity", "tmax", 
                    "tm", "M", "Lm", "Temperature"), ncol=2 )
Predict = Plot_taxa( Taxa, 
                     mfrow=c(2,2),
                     Database = FishBase,
                     params = params )
