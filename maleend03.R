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

dat <- filter_sex_event(sex = "MALE", event = "end", phenf)
dat$TreeID <- paste0(dat$Orchard, dat$X, dat$Y)

length(dat$TreeID)
length(unique(dat$TreeID))
initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           prior = c(prior("normal(555,80)", class = "Intercept"),
                     prior("normal(0,50)", class = "sigma"),
                     prior("normal(0,10)", class = "sd")),
           cores = 7,
           chains = 6,
           inits = initpars,
           iter = 3000,
           #control = list(max_treedepth = 12),
           save_pars = save_pars(all = TRUE))

# saveRDS(fit, "fe21.rds")
#
# fit <- readRDS("fe21.rds")
# summary(fit)
#
# summary(fit)
# plot(fit)
