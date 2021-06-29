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
dat$TreeID <- paste0(dat$Orchard, dat$X, dat$Y)

length(dat$TreeID)
length(unique(dat$TreeID))
initpars <- lapply(1:4, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year) + (1|TreeID), data = dat,
           prior = c(prior("student_t(3,335,50)", class = "Intercept"),
                     prior("normal(0,50)", class = "sigma"),
                     prior("student_t(3,0,20)", class = "sd")),
           cores = 5,
           inits = initpars,
           iter = 3000,
           control = list(adapt_delta=0.9),
           save_pars = save_pars(all = TRUE))

