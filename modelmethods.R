# This script creates a phenological model for lodgepole pine flowering


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

# climate
histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
   filter(forcing_type == "gdd")

# phenology
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one

# meta
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)

## data preparation for phenology model ####
phenf <- prepare_data(phendat, clim = histclim, spu = spudat)
saveRDS(phenf, file = "objects/phenf.rds")

# ggplot(phenf, aes(x = sum_forcing, color = Event_Label, linetype = Sex)) +
#   stat_ecdf() +
#   labs(title = "Cumulative distribution of accumulated forcing for flowering events", caption = "raw data") +
#   scale_colour_viridis_d() +
#   theme_dark(base_size = 18) +
#   ylab("") +
#   xlab("GDD")

# create 4 datasets for 4 models

fbdat <- filter_sex_event(sex = "FEMALE", event = "begin", phenf)
fedat <- filter_sex_event(sex = "FEMALE", event = "end", phenf)

mbdat <- filter_sex_event(sex = "MALE", event = "begin", phenf)
medat <- filter_sex_event(sex = "MALE", event = "end", phenf)

saveRDS(list(fbdat = fbdat, fedat = fedat, mbdat = mbdat, medat = medat), file = "objects/datlist.rds")


# model ####

# This model block is faster if you run models in parallel

# initialize parameter values on the right order of magnitude
initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))

# model formula
bform <- brmsformula(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year) + (1|Tree))

# model prior
bprior <- c(prior("normal(400,100)", class = "Intercept"),
            prior("normal(0,15)", class = "sigma"),
            prior("normal(0,9)", class = "sd"))

# mcmc/computation settings
niter <- 4000
ncores <- 6
nchains <- 6

# female/receptivity begin
fbfit <- brm(bform, data = fbdat,
             save_model = "female_begin.stan",
             file = "female_begin",
             prior = bprior,
             inits = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "on_change",
             control = list(adapt_delta=0.9))

# female/receptivity end
fefit <- brm(bform, data = fedat,
             save_model = "female_end.stan",
             file = "female_end",
             prior = bprior,
             inits = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "on_change")

# male/pollen shed begin
mbfit <- brm(bform, data = mbdat,
             save_model = "male_begin.stan",
             file = "male_begin",
             prior = bprior,
             inits = initpars,
             iter = 5000,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "on_change",
             control = list(adapt_delta = 0.9))

# male/pollen shed end
mefit <- brm(bform, data = medat,
             save_model = "male_end.stan",
             file = "male_end",
             prior = bprior,
             inits = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "on_change")

