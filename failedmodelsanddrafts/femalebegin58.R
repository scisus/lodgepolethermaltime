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

fdat <- filter_sex_event(sex = "FEMALE", event = "begin", dat = phenf)


dat <- phenf %>% filter(Sex == "FEMALE", Event_Obs == 2) %>%
  group_by(Site, Year, Provenance) %>%
  summarise(us <- unique(sum_forcing)) %>%
  summarise(n=n()) %>%
  filter(n==1) %>%
  mutate(keep = 0) %>%
  full_join(fdat) %>%
  filter(is.na(keep) & 250 < sum_forcing & sum_forcing < 390)


initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))
fit <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
           prior = c(prior("normal(335,85)", class = "Intercept"),
                     prior("normal(0,20)", class = "sigma"),
                     prior("normal(0,10)", class = "sd")),
           cores = 7,
           chains = 6,
           inits = initpars,
           iter = 6000,
           #control = list(max_treedepth = 12),
           save_pars = save_pars(all = TRUE))

# saveRDS(fit, "fe21.rds")
#
# fit <- readRDS("fe21.rds")
# summary(fit)
#
# summary(fit)
# plot(fit)
