# female begin

library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)

source('phenology_functions.R')

phendat <- flowers::lodgepole_phenology_event

phenf <- prepare_data(phendat, forcing = "gdd")

dat <- filter_sex_event(sex = "MALE", event = "end", phenf) %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y))

initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year) + (1|Tree), data = dat,
           prior = c(prior("normal(400,100)", class = "Intercept"),
                     prior("normal(0,15)", class = "sigma"),
                     prior("normal(0,9)", class = "sd")),
           cores = 6,
           chains = 6,
           inits = initpars,
          iter = 4000,
          #control = list(adapt_delta = 0.95),
           save_pars = save_pars(all = TRUE))

print(summary(fit))


