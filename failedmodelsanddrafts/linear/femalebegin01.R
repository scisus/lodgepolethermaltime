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

dat <- phenf %>% filter(Sex == "FEMALE", Event_Obs == 2)

initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(sum_forcing ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           cores = 6,
           chains = 6,
           inits = initpars,
           iter = 6000,
           control = list(adapt_delta = 0.9),
           save_pars = save_pars(all = TRUE))

print(summary(fit))


