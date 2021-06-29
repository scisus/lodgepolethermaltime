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

init_sigma <- lapply(1:4, function(id) list(sigma = 30, Intercept = 310 ))
fit <- brm(sum_forcing | cens(censored, upper) ~ 0 + Intercept + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           prior = c(prior("normal(0,20)", class = "b"),
                     prior("normal(0,10)", class = "sigma"),
                      prior("student_t(3,0,10)", class = "sd")), iter = 3000, control = list(adapt_delta=0.9),
           cores = 5, inits = init_sigma,
           save_pars = save_pars(all = TRUE))



