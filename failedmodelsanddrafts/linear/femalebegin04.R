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

dat <- phenf %>% filter(Sex == "FEMALE", Event_Obs == 2) %>%
  mutate(z_sum_forcing = (sum_forcing - mean(sum_forcing))/sd(sum_forcing)) %>%
  select(-Tree) %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y))

#initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(z_sum_forcing ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year) + (1|Tree), data = dat,
           cores = 6,
           chains = 6,
          # inits = initpars,
           #iter = 6000,
           save_pars = save_pars(all = TRUE))

print(summary(fit))


