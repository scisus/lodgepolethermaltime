# Fit a thermal time model to lodgepole pine flowering phenology data

library(dplyr)
library(flowers)
library(rstan)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#source('calculate_forcingunits.R') #check if this is necessary here
source('phenology_functions.R')

# pull only phenology data that is the start or end date from the flowers package
phenbe <- filter_start_end() 

# compile model
phenologymodel <- rstan::stan_model("phenology.stan")

# fit models
female_begin <- munge_and_fit(phendat = phenbe, sex = "FEMALE", event = "begin", compiledmodel = phenologymodel)
female_end <- munge_and_fit(phendat = phenbe, sex = "FEMALE", event = "end", compiledmodel = phenologymodel)

male_begin <- munge_and_fit(phendat = phenbe, sex = "MALE", event = "begin", compiledmodel = phenologymodel)
male_end <- munge_and_fit(phendat = phenbe, sex = "MALE", event = "end", compiledmodel = phenologymodel)


