# model with parent effects + orchard effects + prov

# This script creates a phenological model for lodgepole pine flowering


# depends #####
library(flowers)
library(dplyr)
library(brms)
library(forcats)
#library(ggplot2)
#library(tidyr)
#library(tidybayes)
#library(forcats)
#library(ggbeeswarm)
#library(lubridate)

#theme_set(theme_dark())

source('phenology_functions.R')

# data ####

# provenance climate
provclimdat <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv") %>%
  select(-Pl_SPU, -X_FREQ_)
climvarnames <- colnames(provclimdat)[-c(1:4)]

# site climate
histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
parclim <- read.csv("../phd/data/OrchardInfo/ParentTrees/locations_for_climatena_Normal_1961_1990MSY.csv") %>%
  select(Clone = ID1, Provenance = ID2, all_of(climvarnames)) %>% # keep only annual variables
  mutate(Clone = as.character(Clone)) # for joining purposes later
orchgen <- read.csv("../phd/data/OrchardInfo/OrchardGen.csv")

# parent data
parentdat <- read.csv("../phd/data/OrchardInfo/ParentTrees/parents.csv") %>%
  select(Clone = Parent.Tree.Number, Latitude, Longitude) %>%
  mutate(Clone = as.character(Clone))

parclim <- left_join(parclim, parentdat) %>%
  filter(!is.na(Latitude), !is.na(Longitude))
# meta provenance data
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)

# phenology
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one

# combine provenance climates with provenance names
provclim <- spudat %>%
  select(SPU_Number, SPU_Name) %>%
  distinct() %>%
  left_join(provclimdat) %>%
  rename(Provenance = SPU_Name) %>%
  select(-SPU_Number)

## data preparation for phenology model ####
## When missing parent location data, use provenance climate data
phenftemp <- prepare_data(phendat, clim = histclim, spu = spudat) %>%
  left_join(parclim)

phenfpar <- phenftemp %>%
  filter(!is.na(MAT))

phenfprov <- phenftemp %>%
  filter(is.na(MAT)) %>%
  select(-all_of(climvarnames), -Latitude, -Longitude) %>% # include lat long
  left_join(provclim) %>%
  select(colnames(phenfpar))

phenf <- full_join(phenfpar, phenfprov) %>%
  left_join(orchgen) %>%
  mutate(Generation = ordered(forcats::fct_relevel(Generation, c("1", "1.5", "1.75", "Advanced")))) #%>%

#saveRDS(phenf, file = "objects/phenf.rds")

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

# This model block is faster if you run models in parallel

# initialize parameter values on the right order of magnitude
initpars <- lapply(1:6, function(id) list(sigma = 10, Intercept = 300))

# model formula
bform <- brmsformula(sum_forcing | cens(censored, upper) ~ 1 + Latitude + EMT + MAT + SHM  + (1|Provenance) + (1|Site) + (1|Clone) + (1|Year) + (1|Tree) + (1|Orchard))


# model prior
bprior <- c(prior("normal(400,100)", class = "Intercept"),
            prior("normal(0,15)", class = "sigma"),
            prior("normal(0,9)", class = "sd"),
            prior("student_t(3,0,5)", class ="b"))
# prior("normal(0,9)", class = "b")) # length of phenological period per degree difference

# mcmc/computation settings
niter <- 5000
ncores <- 6
nchains <- 6

# female/receptivity begin
fbfit <- brm(bform, data = fbdat,
             save_model = "female_beginparorchprov.stan",
             file = "female_beginparorchprov",
             prior = bprior,
             inits = initpars,
             iter = niter,
             cores = ncores,
             chains = nchains,
             sample_prior = TRUE,
             save_pars = save_pars(all = TRUE),
             file_refit = "always",
             control = list(adapt_delta = 0.9))
print(summary(fbfit))

loo_fbfit <- loo(fbfit, model_names = "fbfitparorchprov",
                 reloo = TRUE, reloo_args = list(prior = c(prior("normal(400,100)", class = "Intercept"),
                                                           prior("normal(0,15)", class = "sigma"),
                                                           prior("normal(0,9)", class = "sd"),
                                                           prior("student_t(3,0,5)", class ="b")),

                                                 cores = 20,
                                                 inits = initpars, iter = 3000,
                                                 control=list(adapt_delta = 0.9)))
saveRDS(loo_fbfit, "model_dev/loo_parorchprov.rds")
