
library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)
library(future)

source('phenology_functions.R')

phendat <- flowers::lodgepole_phenology_event

phenf <- prepare_data(phendat)

dat <- filter_sex_event(sex = "FEMALE", event = "end", phenf)
dat$TreeID <- paste0(dat$Orchard, dat$X, dat$Y)

init_ll <- lapply(1:4, function(id) list(sigma = 30, Intercept = 300 )) # interval censored models require big sigma inits to start sampling

ec <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
          prior = c(prior("student_t(3,350,50)", class = "Intercept"),
                    prior("student_t(3,0,30)", class = "sigma"),
                    prior("normal(0,20)", class = "sd")),
          cores = 5, inits = init_ll,
          save_pars = save_pars(all = TRUE))
saveRDS(ec, "model_dev/ec.rds")

fitec <- readRDS("model_dev/ec.rds")
no_cores <- availableCores() - 32
plan(multisession, workers = no_cores)
loo_ec <- loo(ec, reloo = TRUE, reloo_args = list(prior = c(prior("student_t(3,350,50)", class = "Intercept"),
                                                            prior("student_t(3,0,30)", class = "sigma"),
                                                            prior("normal(0,20)", class = "sd")),
                                                  future = TRUE,
                                                  inits = init_ll, iter = 3000))
saveRDS(loo_ec, "model_dev/loo_ec.rds")

looec <- readRDS("model_dev/loo_ec.rds")
