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

dat <- filter_sex_event(sex = "FEMALE", event = "begin", dat = phenf) %>%
  mutate(sum_forcing = sum_forcing - 300, upper = upper - 300)


initpars <- lapply(1:6, function(id) list(sigma = 30))
fit <- brm(sum_forcing | cens(censored, upper) ~ 0 + Intercept + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           prior = c(prior("normal(0,50)", class = "b"),
                     prior("normal(0,50)", class = "sigma"),
                     prior("normal(0,10)", class = "sd")),
           cores = 7,
           chains = 6,
           inits = initpars,
           iter = 6000,
           control = list(adapt_delta = 0.9),
           save_pars = save_pars(all = TRUE))

# ggplot(data.frame(x = c(0,600)), aes(x = x)) + xlim(c(0, 600 )) +
#   stat_function(fun = dcauchy, args = list(location = 300, scale = 50), geom = "area",
#                 fill = "green", alpha = 0.25)


