# validate and compare models with efficient approximate leave one out cross validation using pareto smoothed importance sampling

library(rstan)
library(loo)
library(dplyr)

source('phenology_functions.R')

# phenology data

phenbe <- filter_start_end()
ristodat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 
ristodat$i <- 1:nrow(dat)

# add gdd data

#dat$sum_forcing_gdd <-# 
dat <- filter_start_end(forcingname = "gdd") %>%
  select_data("FEMALE", "begin", keep_day = TRUE) %>%
  mutate(i = 1:n()) %>%
  rename(gdd = sum_forcing) %>%
  full_join(ristodat) %>%
  rename(ristos = sum_forcing)
  
fbfitcombined <- readRDS('2021-03-03FEMALE_beginSY.rds')
fbfitseparate <- readRDS('2021-03-09FEMALE_begin.rds')
fbfitgdd <- readRDS('2021-03-09FEMALE_begin_gdd.rds')

# loo calculations ##########

# too many p_waic estimates > 0.5 to use waic


loocombined <- rstan::loo(fbfitcombined, save_psis = TRUE, cores = 4)
looseparate <- rstan::loo(fbfitseparate, save_psis = TRUE, cores = 4)
loogdd <- rstan:loo(fbfitgdd, save_psis = TRUE, cores = 4)

loo_compare(loocombined, looseparate, loogdd)

dat$paretokcombined <-loocombined$diagnostics$pareto_k
dat$paretoseparate <- looseparate$diagnostics$pareto_k
dat$paretogdd <- loogdd$diagnostics$pareto_k

loodat <- pivot_longer(dat, starts_with("pareto"), names_to = "model", values_to = "pareto_k") %>%
  pivot_longer(cols = c("gdd", "ristos"), names_to = "forcingtype", values_to = "sum_forcing")

ggplot(loodat, aes(x=i, y=pareto_k, colour = Site)) +
  geom_point(shape = 3) +
  geom_hline(yintercept = c(0.5, 0.7), lty=2, colour = "darkgray") +
  facet_wrap("model")

### loo PIT (probability interval transform) #######3

parscombined <- fbfitcombined@model_pars
yrepcombined <- rstan::extract(fbfitcombined, pars = 'sum_forcing_rep')$sum_forcing_rep %>% as.matrix()
bayesplot::ppc_loo_pit_overlay(dat$ristos, yrepcombined, lw = weights(loocombined$psis_object)) + ggtitle("Combined - Ristos")

parsseparate <- fbfitseparate@model_pars
yrepseparate <- rstan::extract(fbfitseparate, pars = 'sum_forcing_rep')$sum_forcing_rep %>% as.matrix()
bayesplot::ppc_loo_pit_overlay(dat$ristos, yrepseparate, lw = weights(looseparate$psis_object)) + ggtitle("Separate - Ristos")

parsgdd <- fbfitgdd@model_pars
yrepgdd <- rstan::extract(fbfitgdd, pars = 'sum_forcing_rep')$sum_forcing_rep %>% as.matrix()
bayesplot::ppc_loo_pit_overlay(dat$gdd, yrepgdd, lw = weights(loogdd$psis_object)) + ggtitle("Separate - GDD")

### y vs yrep ####
bayesplot::ppc_dens_overlay(dat$ristos, yrepcombined[sample(1:nrow(yrepcombined), size = 500),]) + ggtitle("Combined - Ristos")
bayesplot::ppc_dens_overlay(dat$ristos, yrepseparate[sample(1:nrow(yrepseparate), size = 500),]) + ggtitle("Separate - Ristos")
bayesplot::ppc_dens_overlay(dat$gdd, yrepgdd[sample(1:nrow(yrepgdd), size = 500),]) + ggtitle("Separate - GDD")


# r squared #######3
r2combined <- rstantools::bayes_R2(yrepcombined, y=dat$ristos)
r2separate <- rstantools::bayes_R2(yrepseparate, y=dat$ristos)
r2gdd <- rstantools::bayes_R2(yrepgdd, y=dat$ristos)

r2df <- data.frame(r2combined, r2separate, r2gdd) %>%
  tidyr::pivot_longer(starts_with("r2"), names_to = "model", values_to = "r2")

# combined model has best R2 by far
ggplot(r2df, aes(x = r2, colour = model)) +
  geom_density() +
  ggtitle("Bayes R^2")

# kfold validation ####
