# This script fits a phenological model for lodgepole pine flowering


# depends #####
library(flowers)
library(dplyr)
library(brms)

source('phenology_functions.R')


# data ####

## phenology
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Genotype, X, Y)) # create a unique Tree identifier since original data doesn't always have one

## forcing
dailyforc <- read.csv("data/forcing/dailyforc_1945_2012.csv", header=TRUE, stringsAsFactors = FALSE)

# meta
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)

prov_climate <- read.csv("../lodgepole_climate/data/climateBC/climatebc_parent_locs_Normal_1961_1990Y_v730.csv") %>%
  rename(Genotype = id1, SPZ = id2)  %>%
  select(Genotype, SPZ, MAT) %>%
  mutate(Genotype = as.character(Genotype))

## data preparation for phenology model ####
phenf <- prepare_data(phendat, clim = dailyforc, spu = spudat) %>%
  left_join(prov_climate, relationship = "many-to-many") %>%
  filter(!is.na(MAT)) # Only keep genotypes with an associated source MAT - no breeding

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
bform <- brmsformula(sum_forcing | cens(censored, upper) ~ MAT + (1|Site) + (1|Genotype) + (1|Year) + (1|Tree))

# model prior
bprior <- c(prior("gamma(3.65, 0.01)", class = "Intercept", lb = 0),
            prior("normal(0,15)", class = "sigma"),
            prior("normal(0,9)", class = "sd"),
            prior("normal(0,25)", class = "b"))

# mcmc/computation settings
niter <- 4500
ncores <- 6
nchains <- 6

# female/receptivity begin
fbfit <- brm(bform, data = fbdat,
             save_model = "stan_output/female_begin.stan",
             file = "stan_output/female_begin.rds",
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
             save_model = "stan_output/female_end.stan",
             file = "stan_output/female_end.rds",
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
             save_model = "stan_output/male_begin.stan",
             file = "stan_output/male_begin.rds",
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
             save_model = "stan_output/male_end.stan",
             file = "stan_output/male_end.rds",
             prior = bprior,
             init = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "always")

# write out package versions and seeds

pkgversions <- fbfit$version
modelseeds <- list(fb = rstan::get_seed(fb$fit),
                   fe = rstan::get_seed(fe$fit),
                   mb = rstan::get_seed(mb$fit),
                   me = rstan::get_seed(me$fit))

saveRDS(list(pkgversions, modelseeds), "objects/model_meta.rds")

