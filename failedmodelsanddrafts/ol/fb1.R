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

dat <- filter_sex_event(sex = "FEMALE", event = "begin", phenf)
nullmdl <- brm(self ~ (1|id), data=d, family=cumulative("logit"), threshold="flexible")

#initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(State ~ sum_forcing + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat, family = cumulative("logit"), threshold = "flexible",

           cores = 7,
           chains = 6,
    #       inits = initpars,
           iter = 4000,
           #control = list(max_treedepth = 12),
           save_pars = save_pars(all = TRUE))

print(summary(fit))

