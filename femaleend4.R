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

fit <- brm(sum_forcing_centered | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           prior = c(prior("normal(0,50)", class = "Intercept"),
                     prior("normal(0,50)", class = "sigma"),
                     prior("student_t(3,0,20)", class = "sd")),
           cores = 5, inits = init_sigma,
           save_pars = save_pars(all = TRUE))

