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

mix <- mixture(gaussian, gaussian)
# prior <- c(
#   prior(normal(0, 7), Intercept, dpar = mu1),
#   prior(normal(5, 7), Intercept, dpar = mu2)
# )
# fit1 <- brm(bf(y ~ x + z), dat, family = mix,
#             prior = prior, chains = 2)



dat <- filter_sex_event(sex = "FEMALE", event = "begin", phenf)

initpars <- lapply(1:6, function(id) list(mu1 = 0, mu2 = 0, sigma1 = 0, sigma2 = 0))
fit <- brm(sum_forcing | cens(censored, upper) ~ 1, data = dat, family = mix,

           cores = 7,
           chains = 6,
           inits = initpars,
           iter = 2000,
           #control = list(max_treedepth = 12),
           save_pars = save_pars(all = TRUE))

# saveRDS(fit, "fe21.rds")
#
# fit <- readRDS("fe21.rds")
# summary(fit)
#
# summary(fit)
# plot(fit)
