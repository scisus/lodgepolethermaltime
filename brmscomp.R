library(flowers)
library(dplyr)
library(brms)
library(bayesplot)

rstan::rstan_options(auto_write = TRUE)
source('phenology_functions.R')

phenbe <- filter_start_end() # filter phenology data for only start and end dates

censorbegin <- add_censor_indicator() # add a censor indicator to start events
factors <- c("Site", "Provenance", "Year", "Clone")

fbdat <- select_data(phendat = phenbe, censordat = censorbegin, factors = factors, sex = "FEMALE", event = "begin") %>%
  mutate(sum_forcing_centered = sum_forcing - mean(sum_forcing))

# fit the simplest model possible (mean only)

mo <- brm(sum_forcing_centered ~ 1, data = fbdat)
summary(mo)
plot(mo)
mo_yrep <- posterior_predict(mo, draws = 500)

color_scheme_set("brightblue")
ppc_dens_overlay(fbdat$sum_forcing_centered, mo_yrep[1:50,])

loo_mo <- loo(mo)

# fit a model with censorship

moc <- brm(sum_forcing_centered | cens(censored) ~ 1, data = fbdat)
summary(moc)
plot(moc)
moc_yrep <- posterior_predict(moc, draws = 500)

color_scheme_set("purple")
ppc_dens_overlay(fbdat$sum_forcing_centered, moc_yrep[1:50,])

# compare
loo_moc <- loo(moc)

# fit a model with effects
msypc <- brm(sum_forcing_centered ~ 1 + (1|Site) + (1|Provenance) + (1|Year) + (1|Clone), data = fbdat, cores = 5)
summary(msypc)
plot(msypc, pars = )

loo_msypc <- loo(msypc)

# fit a model with censorship and effects
msypc_c <- brm(sum_forcing_centered | cens(censored) ~ 1 + (1|Site) + (1|Provenance) + (1|Year) + (1|Clone), data = fbdat, cores = 5)
summary(msypc_c)
msypc_c_yrep <- posterior_predict(msypc_c, draws = 500)

loo_msypc_c <- loo(msypc_c)
color_scheme_set("green")
ppc_dens_overlay(fbdat$sum_forcing_centered, msypc_c_yrep[1:50,])

# fit a model with left censorship AND interval censorship
#
brms::loo_compare(loo_msypc_c, loo_msypc, loo_moc, loo_mo)
