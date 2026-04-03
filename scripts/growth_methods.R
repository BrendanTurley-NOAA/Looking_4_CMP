# https://github.com/jonathansmart/BayesGrowth?tab=readme-ov-file
# https://github.com/haddonm/MQMF?tab=readme-ov-file
# https://haddonm.github.io/URMQMF/model-parameter-estimation.html#a-length-at-age-example
# https://github.com/arni-magnusson/fishgrowth
# https://jonathansmart.github.io/AquaticLifeHistory/articles/Growth_estimation.html
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0246734


library(BayesGrowth)
library(tidyverse)
library(BayesGrowth)
library(AquaticLifeHistory)
library(tidybayes)
library(bayesplot)
library(rstan)
library(pander)
library(cowplot)

# built in dataset for running examples
data("example_data")

## Biological info - lengths in mm
max_size <- 440
max_size_se <- 5
birth_size <- 0
birth_size_se <- 0.001 #  cannot be zero

# Use the function to estimate the rstan model
MCMC_example_results <- Estimate_MCMC_Growth(example_data, 
                                             Model = "VB" ,
                                             iter = 10000, 
                                             n.chains = 4,  # minimum of 3 chains recommended
                                             BurnIn = 1000, # default is 10% of iterations
                                             thin = 10,      # a thinning rate of 10 is applied to overcome autocorrelation
                                             Linf = max_size,
                                             Linf.se = max_size_se,
                                             L0 = birth_size,
                                             L0.se = birth_size_se,
                                             sigma.max = 100,
                                             verbose = TRUE,
                                             k.max = 1)
library(rstan)
summary(MCMC_example_results,pars = c("Linf", "k", "L0", "sigma"))$summary



### kmk
gulf_dat <- subset(dat_all, stock_id_2 == 'GULF' & final_age>0)
gulf_laa <- data.frame(Length = gulf_dat$fl_mm, Age = gulf_dat$final_age) |>
  na.omit()
which(is.na(gulf_laa))

## Biological info - lengths in mm
max_size <- 1300
max_size_se <- 5
birth_size <- 25
birth_size_se <- 0.001 #  cannot be zero

# Use the function to estimate the rstan model
MCMC_example_results <- Estimate_MCMC_Growth(gulf_laa, 
                                             Model = "VB" ,
                                             iter = 10000, 
                                             n.chains = 4,  # minimum of 3 chains recommended
                                             BurnIn = 2000, # default is 10% of iterations
                                             thin = 10,      # a thinning rate of 10 is applied to overcome autocorrelation
                                             Linf = max_size,
                                             Linf.se = max_size_se,
                                             L0 = birth_size,
                                             L0.se = birth_size_se,
                                             sigma.max = 100,
                                             verbose = TRUE,
                                             k.max = 1,
                                             n_cores = 1)
# n_cores = 3

# https://mc-stan.org/misc/warnings.html

summary(MCMC_example_results,pars = c("Linf", "k", "L0", "sigma"))$summary

mcmc_combo(MCMC_example_results,pars = c("Linf", "k", "L0", "sigma")) 

pairs(MCMC_example_results, pars = c("Linf", "k","L0", "sigma"))


list_of_draws <- extract(MCMC_example_results,c("Linf", "k","L0", "sigma")) %>% 
  as.data.frame() %>% 
  gather(Parameter, Value) %>% 
  filter(Parameter %in% c("Linf", "k","L0", "sigma"))


ggplot(list_of_draws, aes(Value))+
  geom_density(fill = "royalblue")+
  facet_wrap(~Parameter, scales = "free", ncol = 2)+
  theme_bw()


# Return a growth curve with 50th and 95th percentiles
growth_curve <- Calculate_MCMC_growth_curve(MCMC_example_results, Model = "VB",
                                            max.age = max(gulf_laa$Age), probs = c(.5,.95))


ggplot(growth_curve, aes(Age, LAA))+
  geom_point(data = gulf_laa, aes(Age, Length), alpha = .3)+
  geom_lineribbon(aes( ymin = .lower, ymax = .upper, fill = factor(.width)), size = .8) +
  labs(y = "Total Length (mm)", x = "Age (yrs)")+
  scale_fill_brewer(palette="BuPu", direction=-1,name = "Credibility interval")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), breaks = seq(0,13,1))+
  theme_bw()+
  theme(text = element_text(size = 14),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(colour = "black"))
