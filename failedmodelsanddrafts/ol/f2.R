# female begin

library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)

source('phenology_functions.R')

phendat <- flowers::lodgepole_phenology_event

phenf <- prepare_data(phendat)

dat <- filter(phenf, Sex == "FEMALE")
dat$State <- ordered(dat$State, levels = c("1", "2", "3"))

#divergences and low ESS with default priors and iter 2000
fit <- brm(State ~ 1 + sum_forcing + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat, family = cumulative("logit"),
           cores = 5,
           chains = 5,
           iter = 6000,
           file = "f1",
           file_refit = "on_change",
           #control = list(max_treedepth = 12),
           save_pars = save_pars(all = TRUE))

print(summary(fit))



