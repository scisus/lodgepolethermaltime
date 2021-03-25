# Fit a thermal time model to my phenology data

library(dplyr)
library(flowers)
library(rstan)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source('calculate_forcingunits.R')
source('phenology_functions.R')

# choose only phenology data that is the start or end date
phenbe <- filter_start_end(forcingname = "gdd") 



# set thresholds
# - Site threshold: 250
# - Provenance threshold: 150
# - Clone threshold: 10


# fit models
female_begin <- fit_model(phendat = phenbe, sex = "FEMALE", event = "begin", appendname = "gdd")
female_end <- fit_model(phendat = phenbe, sex = "FEMALE", event = "end")

male_begin <- fit_model(phendat=phenbe, sex="MALE", event = "begin")
male_end <- fit_model(phendat = phenbe, sex="MALE", event = "end")


