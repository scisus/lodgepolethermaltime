# Fit a thermal time model to my phenology data

library(dplyr)
library(flowers)
library(rstan)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source('calculate_forcingunits.R')
source('phenology_functions.R')

# calculate censorship codes. 0 for uncensored, 1 for left censored, 2 for right censored, and 3 for no flowering record
phen <-  flowers::phenology 

# Wagner censorship

wagnerbegin <- phen %>% 
  filter(Source == "Rita Wagner") %>%
  group_by(Source, Index, Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
  summarize(censorship = case_when(unique(First_RF) == min(DoY) ~ 1,
            unique(First_RF) > min(DoY) ~ 0,
            is.na(unique(First_RF)) ~ 3))

nawag <- wagnerbegin[(is.na(wagnerbegin$censorship)),] #test no nas



walshbegin <- phen %>%
  filter(Source == "Chris Walsh") %>%
  group_by(Source, Sex, Year, Site, Orchard) %>%
  mutate(first_group_obs = min(DoY)) %>%
  ungroup() %>%
  group_by(Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
  summarize(censorship = case_when(unique(First_RF) == min(DoY) ~ 1,
                                   unique(First_RF) > min(DoY) ~ 0,
                                   is.na(unique(First_RF)) ~ 3))

nawal <- walshbegin[(is.na(walshbegin$censorship)),] #test no nas

censorbegin <- full_join(wagnerbegin, walshbegin) %>%
  ungroup() %>%
  mutate(Year = as.character(Year), Clone = as.character(Clone)) 

naall <- censorbegin[(is.na(censorbegin$censorship)),] #test no nas

censorbegin %>%
  group_by(Site, Sex) %>%
  summarise(sum(censorship)/n())

# choose only phenology data that is the start or end date
phenbe <- filter_start_end() 
foo <- select_data(phenbe, sex = "FEMALE", event = "begin") %>%
  left_join(censorbegin)

phena <- foo[which(is.na(foo$censorship)),]


# set thresholds
# - Site threshold: 250
# - Provenance threshold: 150
# - Clone threshold: 10


# fit models
female_begin <- fit_model(phendat = phenbe,  censorship = censorbegin, sex = "FEMALE", event = "begin")
female_end <- fit_model(phendat = phenbe, sex = "FEMALE", event = "end")

male_begin <- fit_model(phendat=phenbe, sex="MALE", event = "begin")
male_end <- fit_model(phendat = phenbe, sex="MALE", event = "end")


