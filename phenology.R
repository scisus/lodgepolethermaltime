# Fit a thermal time model to lodgepole pine flowering phenology data

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

phenbe <- filter_start_end()

# compile model
phenologymodel <- rstan::stan_model("phenology.stan")


# set options for models
factors <- c("Site", "Provenance", "Year", "Clone")
factor_threshold_list <- list(Site = 250, Provenance = 150)
expars <- c("delta_ncp_site", "delta_cp_site",
           "delta_ncp_prov", "delta_cp_prov",
           "z_delta_clone")
init <- rep(list(list(mu = abs(rnorm(1,100,50)),
                     sigma = rexp(1,1),
                     sigma_site = rexp(1,1),
                     sigma_year = rexp(1,1),
                     sigma_prov = rexp(1,1),
                     sigma_clone = rexp(1,1))), 6)

# fit models
female_begin <- munge_and_fit(phendat = phenbe, sex = "FEMALE", event = "begin", appendname = "site", compiledmodel = phenologymodel,
                              factors = factors, factor_threshold_list = factor_threshold_list,
                              expars = expars, init = init)

#female_begin <- fit_model(phendat = phenbe, sex = "FEMALE", censorship = censorbegin, event = "begin", maxtreedepth = 12, iter = 4500, warmup = 1500)
#male_begin <- fit_model(phendat = phenbe, sex = "MALE", censorship = censorbegin, event = "begin", maxtreedepth = 12, iter = 4500, warmup = 1500)


female_end <- munge_and_fit(phendat = phenbe, sex = "FEMALE", event = "end", compiledmodel = phenologymodel,
                            factors = factors, factor_threshold_list = factor_threshold_list,
                            expars = expars, init = init)

male_begin <- munge_and_fit(phendat = phenbe, sex = "MALE", event = "begin", compiledmodel = phenologymodel,
                            factors = factors, factor_threshold_list = factor_threshold_list,
                            expars = expars, init = init)

male_end <- munge_and_fit(phendat = phenbe, sex = "MALE", event = "end", compiledmodel = phenologymodel,
                          factors = factors, factor_threshold_list = factor_threshold_list,
                          expars = expars, init = init)


