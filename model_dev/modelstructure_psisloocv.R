# This script reads in and prepares phenology and climate data, fits several models to phenology data, does leave one out cross validation with pareto smoothed importance sampling, and uses reloo (refits the model minus a given point) on points with high pareto k values. Model output and loo output is saved.
# The model fitting and loo calculations are done only if global variable freshrun == TRUE. Otherwise, prior runs are read in from memory.
# This script was created on a linux machine with 40 cores and 128 gb of memory. It is not fast and you may have memory issues on smaller machines.

library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)
library(ggfortify)
library(hrbrthemes)
library(future)

source('phenology_functions.R')

# Choose how many cores you can use for sequential reloo
cores <- 20

rstan::rstan_options(auto_write = TRUE)


# Read in data and prepare
phendat <- flowers::lodgepole_phenology_event
climdat <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
spudat <- spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  select(SPU_Name, Orchard)

phendat <- flowers::lodgepole_phenology_event

phenf <- prepare_data(phendat, clim = climdat, spu = spudat)

# limit to female end data

fedat <- filter_sex_event(sex = "FEMALE", event = "end", phenf)
fedat$TreeID <- paste0(fedat$Orchard, fedat$Clone, fedat$X, fedat$Y)

# code censorship for plotting
status <- mutate(fedat, status_y = case_when(censored == "interval" ~ 1,
                                             censored == "right" ~ 0) )


## Fit Models and calculate leave-one-out cross validation elpds
##

if (freshrun == TRUE) {

  init_ll <- lapply(1:4, function(id) list(sigma = 30, Intercept = 300 )) # interval censored models require big sigma inits to start sampling

  # mean only
  mo <- brm(sum_forcing ~ 1, data = fedat,
            prior = c(prior("normal(400,100)", class = "Intercept"),
                      prior("normal(0,15)", class = "sigma")),
            cores = 5, inits = init_ll)

  saveRDS(mo, "model_dev/mo.rds")
  loo_mo <- loo(mo)
  saveRDS(loo_mo, "model_dev/loo_mo.rds")


  # mean only with end censoring
  moc <- brm(sum_forcing | cens(censored_lronly) ~ 1, data = fedat,
             prior = c(prior("normal(400,100)", class = "Intercept"),
                       prior("normal(0,15)", class = "sigma")),
             cores = 5, inits = init_ll)
  saveRDS(moc, "model_dev/moc.rds")
  loo_moc <- loo(moc)
  saveRDS(loo_moc, "model_dev/loo_moc.rds")

  # mean only model with end and interval censoring


  mocf <- brm(sum_forcing | cens(censored, upper) ~ 1, data = fedat,
              prior = c(prior("normal(400,100)", class = "Intercept"),
                        prior("normal(0,15)", class = "sigma")),
              cores=5,  inits = init_ll)
  saveRDS(mocf, "model_dev/mocf.rds")
  loo_mocf <- loo(mocf)
  saveRDS(loo_mocf, "model_dev/loo_mocf.rds")

  # tidy
  rm(mo, moc, mocf)

  # mean + effects + end + interval censoring

  ec <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = fedat,
            prior =  c(prior("normal(400,100)", class = "Intercept"),
                       prior("normal(0,15)", class = "sigma"),
                       prior("normal(0,9)", class = "sd")),
            cores = 5, inits = init_ll,
            save_pars = save_pars(all = TRUE))
  saveRDS(ec, "model_dev/ec.rds")


  future::plan(multisession) # for use with the future = TRUE arg in for reloo
  loo_ec <- loo(ec, reloo = TRUE, reloo_args = list(prior = c(prior("normal(400,100)", class = "Intercept"),
                                                              prior("normal(0,15)", class = "sigma"),
                                                              prior("normal(0,9)", class = "sd")),
                                                    future = TRUE,
                                                    inits = init_ll, iter = 3000))
  saveRDS(loo_ec, "model_dev/loo_ec.rds")
  future::plan(sequential)

  # tidy up to increase chances your computer won't fall over
  rm(ec)
  gc()

  # mean + main effects + treeid + end + interval censoring
  ect <- brm(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year) + (1|TreeID), data = fedat,
             prior = c(prior("normal(400,100)", class = "Intercept"),
                       prior("normal(0,15)", class = "sigma"),
                       prior("normal(0,9)", class = "sd")),
             cores = 5, inits = init_ll, iter = 3000,
             save_pars = save_pars(all = TRUE))
  saveRDS(ect, "model_dev/ect.rds")

  # For reasons unclear to me, running loo on ect with plan(multisession) causes the following error: Error in unserialize(node$con) :
  # MultisessionFuture (<none>) failed to receive results from cluster RichSOCKnode #17 (PID 2.43777e+06 on ‘localhost’). The reason reported was ‘error reading from connection’. Post-mortem diagnostic: Detected a non-exportable reference (‘externalptr’) in one of the globals (‘.reloo’ of class ‘function’) used in the future expression. The total size of the 11 globals exported is 191.75 MiB. The three largest globals are ‘x’ (191.45 MiB of class ‘list’), ‘mf’ (224.65 KiB of class ‘list’) and ‘.reloo’ (42.86 KiB of class ‘function’)
  loo_ect <- loo(ect, reloo = TRUE, reloo_args = list(prior = c(prior("normal(400,100)", class = "Intercept"),
                                                                prior("normal(0,15)", class = "sigma"),
                                                                prior("normal(0,9)", class = "sd")),
                                                      cores = cores,
                                                      inits = init_ll, iter = 3000))


  saveRDS(loo_ect, "model_dev/loo_ect.rds") }

