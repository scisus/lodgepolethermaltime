# This script fits a phenological model for lodgepole pine flowering


# depends #####
library(flowers)
library(dplyr)
library(brms)
#library(ggplot2)
#library(tidyr)
#library(tidybayes)
#library(forcats)
#library(ggbeeswarm)
#library(lubridate)

#theme_set(theme_dark())

source('phenology_functions.R')


# data ####

## phenology
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one

## forcing
dailyforc <- read.csv("data/dailyforc_1945_2012.csv", header=TRUE, stringsAsFactors = FALSE)

# meta
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)

prov_climate <- read.csv("../lodgepole_climate/data/climateBC/climatebc_parent_locs_Normal_1961_1990Y_v730.csv") %>%
  rename(Clone = id1, SPZ = id2)  %>%
  select(Clone, SPZ, MAT) %>%
  mutate(Clone = as.character(Clone))

## data preparation for phenology model ####
phenf <- prepare_data(phendat, clim = dailyforc, spu = spudat) %>%
  left_join(prov_climate) %>%
  filter(!is.na(MAT)) # Only keep clones with an associated source MAT - no breeding

saveRDS(phenf, file = "objects/phenf.rds")

# create 4 datasets for 4 models

fbdat <- filter_sex_event(sex = "FEMALE", event = "begin", phenf)
fedat <- filter_sex_event(sex = "FEMALE", event = "end", phenf)
mbdat <- filter_sex_event(sex = "MALE", event = "begin", phenf)
medat <- filter_sex_event(sex = "MALE", event = "end", phenf)

saveRDS(list(fbdat = fbdat, fedat = fedat, mbdat = mbdat, medat = medat), file = "objects/datlist.rds")


# model ####

# This model block is faster if you run models in parallel

# initialize parameter values with the right order of magnitude
initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))

# model formula
bform <- brmsformula(sum_forcing | cens(censored, upper) ~ MAT + (1|Site) + (1|Clone) + (1|Year) + (1|Tree))

# model prior
bprior <- c(prior("gamma(3.65, 0.01)", class = "Intercept"),
            prior("normal(0,15)", class = "sigma"),
            prior("normal(0,9)", class = "sd"),
            prior("normal(0,5)", class = "b"))

# mcmc/computation settings
niter <- 4000
ncores <- 6
nchains <- 6

# female/receptivity begin
fbfit <- brm(bform, data = fbdat,
             save_model = "female_begin.stan",
             file = "female_begin",
             prior = bprior,
             init = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "always")

# female/receptivity end
fefit <- brm(bform, data = fedat,
             save_model = "female_end.stan",
             file = "female_end",
             prior = bprior,
             init = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "always")

# male/pollen shed begin
mbfit <- brm(bform, data = mbdat,
             save_model = "male_begin.stan",
             file = "male_begin",
             prior = bprior,
             init = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "always")

# male/pollen shed end
mefit <- brm(bform, data = medat,
             save_model = "male_end.stan",
             file = "male_end",
             prior = bprior,
             init = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "always")

