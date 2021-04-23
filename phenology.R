# Fit a thermal time model to my phenology data

library(dplyr)
library(flowers)
library(rstan)
#library(tidybayes)

#rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#source('calculate_forcingunits.R')
source('phenology_functions.R')

# calculate censorship codes. 0 for uncensored, 1 for left censored, 2 for right censored, and 3 for no flowering record
phen <-  flowers::phenology

# Wagner censorship

wagnerbegin <- phen %>%
  filter(Source == "Rita Wagner") %>%
  group_by(Source, Index, Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
  summarize(censored = case_when(unique(First_RF) == min(DoY) ~ 1,
            unique(First_RF) > min(DoY) ~ 0,
            is.na(unique(First_RF)) ~ 3))

nawag <- wagnerbegin[(is.na(wagnerbegin$censored)),] #test no nas



walshbegin <- phen %>%
  filter(Source == "Chris Walsh") %>%
  group_by(Source, Sex, Year, Site, Orchard) %>%
  mutate(first_group_obs = min(DoY)) %>%
  ungroup() %>%
  group_by(Source, Index, Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
  summarize(censored = case_when(unique(First_RF) == first_group_obs ~ 1,
                                   unique(First_RF) > first_group_obs ~ 0,
                                   is.na(unique(First_RF)) ~ 3))



nawal <- walshbegin[(is.na(walshbegin$censored)),] #test no nas

censorbegin <- full_join(wagnerbegin, walshbegin) %>%
  ungroup() %>%
  mutate(Year = as.character(Year), Clone = as.character(Clone))

naall <- censorbegin[(is.na(censorbegin$censored)),] #test no nas


censorbegin %>%
  group_by(Site, Sex) %>%
  summarise(percent_censored = sum(100*censored)/n())

#nocensor <- filter(censorbegin, censored == 0)

# choose only phenology data that is the start or end date
phenbe <- filter_start_end()


# set thresholds
# - Site threshold: 250
# - Provenance threshold: 150


# fit models
#
library(ggplot2)
library(ggbeeswarm)
ggplot(filter(phenbe, Site == "Tolko"), aes(x = Year, y = DoY)) +
         geom_beeswarm()
ggplot(filter(phenbe, Site == "Tolko"), aes(x=Year, y = sum_forcing)) +
         geom_beeswarm()
female_begin <- fit_model(phendat = phenbe, sex = "FEMALE", censorship = censorbegin, event = "begin", maxtreedepth = 10)

fb <- readRDS("2021-04-21FEMALE_begin.rds")
nuts <- bayesplot::nuts_params(fb)
draws <- as.array(fb)
bayesplot::mcmc_parcoord(draws, pars = vars("mu", "sigma", starts_with("mu_"), starts_with("sigma_"), contains("alpha_site")), np=nuts, transform = function(x) {(x - mean(x)) / sd(x)})
bayesplot::mcmc_intervals(draws, pars = vars(starts_with("mu_"), starts_with("sigma_"), contains("delta_site"), "sigma"))
bayesplot::mcmc_intervals(draws, pars = vars( contains("year")))
bayesplot::mcmc_areas(draws, pars = vars(contains("sigma_")))
bayesplot::mcmc_areas(draws, pars = vars(contains("mu_")))
#female_begin_censored <- fit_model(phendat = censored, sex = "FEMALE", event = "begin")

library(shinystan)
launch_shinystan(fb)
female_end <- fit_model(phendat = phenbe, sex = "FEMALE", event = "end")

male_begin <- fit_model(phendat=phenbe, censored = censorbegin, sex = "MALE", event = "begin")
male_end <- fit_model(phendat = phenbe, sex="MALE", event = "end")


