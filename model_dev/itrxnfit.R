# female end

library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)

source('phenology_functions.R')

phendat <- flowers::lodgepole_phenology_event

phenf <- prepare_data(phendat)

dat <- filter_sex_event(sex = "FEMALE", event = "end", phenf)
init_sigma <- lapply(1:4, function(id) list(sigma = 30 ))

fit <- brm(sum_forcing_centered | cens(censored, upper) ~ 0 + Intercept + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year) + (1|Site:Year), data = dat,
           cores = 5, inits = init_sigma,
           save_pars = save_pars(all = TRUE))

saveRDS(fit, "ecrxn.rds")

