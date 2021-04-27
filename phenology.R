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
phen <-  flowers::phenology %>%
  mutate(period = Last_RF - First_RF)

beta <- -1 * abs(rnorm(100, 0, 1))
summary(beta)
alpha <- rnorm(100, 0, 1)
summary(alpha)

days <- sample(seq(from = 0, to = 16, by = 1), size = 100, replace = TRUE)
p1 <- gtools::inv.logit(-2.8*0:16 + 0)
p3 <- gtools::inv.logit(-2.8*0:16 + 2)
p2 <- gtools::inv.logit(-2.8*0:16 - 2)
p4 <- gtools::inv.logit(-0.1* 0:16 + 2)
censor1 <- rbinom(n=100, size = 1, prob = 0.9)
censor2 <- rbinom(n=100, size = 1, prob = 0.1)
censor3 <- rbinom(n=100, size=1, prob= 2)
censor4 <- rbinom(n=100, size=1, prob=p4)

plot(days, censor1)
plot(days, censor2)
plot(days, censor3)
plot(days, censor4)


plot(0:16, p1, ylim = c(0,1))

lines(p1)
lines(p2)
lines(p3)
lines(p4)
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


