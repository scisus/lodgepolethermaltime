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

initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           prior = c(prior("student_t(3,350,50)", class = "Intercept"),
                     prior("student_t(3,0,50)", class = "sigma"),
                     prior("student_t(3,0,20)", class = "sd")),
           cores = 7,
           chains = 6,
           inits = initpars,
           iter = 3000,
           control = list(max_treedepth = 12),
           save_pars = save_pars(all = TRUE))

saveRDS(fit, "fe26.rds")

fitn <- readRDS("fe26.rds")
summary(fitn)
