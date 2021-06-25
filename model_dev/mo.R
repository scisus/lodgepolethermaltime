
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

init_ll <- lapply(1:4, function(id) list(sigma = 30, Intercept = 300 )) # interval censored models require big sigma inits to start sampling

mo <- brm(sum_forcing ~ 1, data = dat,
          prior = c(prior("normal(350,50)", class = "Intercept"),
                    prior("normal(0,20)", class = "sigma")),
          cores = 5, inits = init_ll)

saveRDS(mo, "model_dev/mo.rds")
