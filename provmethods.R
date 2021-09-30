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
provclim <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv") %>% # climate for provenances
  select(SPU_Number, Latitude, MAT, MCMT, MWMT)
parentclim <- read.csv("../phd/data/OrchardInfo/ParentTrees/locations_for_climatena_Normal_1961_1990MSY.csv") %>%
  rename(Clone = ID1, Provenance = ID2) #%>%
  #mutate(Clone = as.character(Clone))

# phenology
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one

# meta
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  right_join(parentclim, by = c("SPU_Name" = "Provenance"))

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
iobform <- brmsformula(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year) + (1|Tree))

bform <- brmsformula(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + MCMT + (1|Clone) + (1|Year) + (1|Tree))

# model prior
bprior <- c(prior("normal(400,100)", class = "Intercept"),
            prior("normal(0,20)", class = "b"),
            prior("normal(0,15)", class = "sigma"),
            prior("normal(0,9)", class = "sd"))

iobprior <- c(prior("normal(400,100)", class = "Intercept"),
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
             file_refit = "on_change")

loofb <- brms::loo(fbfit, reloo = TRUE, reloo_args = list(prior=bprior),
             cores=20, future =FALSE, inits = initpars, iter = 3000)
saveRDS(loofb, "fbfitloo.rds")
gc()


fbfitio <- brm(iobform, data = fbdat,
               save_model = "female_beginio.stan",
               file = "female_beginio",
               prior = iobprior,
               inits = initpars,
               iter = niter,
               cores = ncores,
               chains = nchains,
               sample_prior = TRUE,
               save_pars = save_pars(all = TRUE),
               file_refit = "on_change")
loofbio <- loo(fbfitio, reloo = TRUE, reloo_args = list(prior=iobprior),
               cores = 20, inits = initpars, iter = 3000)
saveRDS(loofbio, "fbfitioloo.rds")

loofb <- readRDS("fbfitloo.rds")
loofbio <- readRDS("fbfitioloo.rds")

loo_compare(loofb, loofbio)
