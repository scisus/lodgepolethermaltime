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

dat <- filter_sex_event(sex = "FEMALE", event = "begin", dat = phenf)
dat$TreeID <- paste0(dat$Orchard, dat$Clone, dat$X, dat$Y)


initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(sum_forcing | cens(censored, upper) ~ 1  + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           prior = c(prior("normal(335,40)", class = "Intercept"),
                     prior("normal(0,20)", class = "sigma"),
                     prior("normal(0,10)", class = "sd")),
           cores = 6,
           chains = 6,
           inits = initpars,
           iter = 8000,
          #control = list(adapt_delta = 0.9),
           save_pars = save_pars(all = TRUE))



