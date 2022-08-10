# Provenance model: clone offsets from thermal time model as a function of provenance

library(dplyr)
library(brms)
library(report)

# get data
# response
clone_offsets <- readRDS("objects/cloner.rds") %>%
  mutate(Clone = as.character(level))

# predictors
prov_climate <- read.csv("../phd/data/OrchardInfo/ParentTrees/locations_for_climatena_Normal_1961_1990MSY.csv") %>%
  rename(Clone = ID1, SPZ = ID2)  %>%
  select(Clone, SPZ, MAT) %>%
  mutate(Clone = as.character(Clone))


# munge data
dat <- clone_offsets %>%
  # summarize clone info
  group_by(model, Sex, event, Clone) %>%
  summarise(meanoffset = mean(.value), sdoffset = sd(.value)) %>%
  # combine with parent info
  left_join(prov_climate)
saveRDS(dat, "objects/clonedat.rds")

fbclone <- filter(dat, Sex == "FEMALE", event == "begin")
feclone <- filter(dat, Sex == "FEMALE", event == "end")

mbclone <- filter(dat, Sex == "MALE", event == "begin")
meclone <- filter(dat, Sex == "MALE", event == "end")

bformse <- brmsformula(meanoffset | se(sdoffset, sigma = TRUE) ~ MAT)

bprior <- c(prior("normal(0,10)", class = "Intercept"),
            prior("normal(0,9)", class = "sigma"),
            prior("normal(0,5)", class = "b"))

ncores = 6
nchains = 6
niter = 3000

fbfitclone <- brm(bformse, data = fbclone,
            save_model = "female_begin_clone.stan",
            file = "female_begin_clone",
            prior = bprior,
            iter = niter,
            cores = ncores,
            chains = nchains,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            file_refit = "on_change")

mbfitclone <- brm(bformse, data = mbclone,
                    save_model = "male_begin_clone.stan",
                    file = "male_begin_clone",
                    prior = bprior,
                    #inits = initpars,
                    iter = niter,
                    cores = ncores,
                    chains = nchains,
                    sample_prior = TRUE,
                    save_pars = save_pars(all = TRUE),
                    file_refit = "on_change")

fefitclone <- brm(bformse, data = feclone,
                    save_model = "female_end_clone.stan",
                    file = "female_end_clone",
                    prior = bprior,
                    #inits = initpars,
                    iter = niter,
                    cores = ncores,
                    chains = nchains,
                    sample_prior = TRUE,
                    save_pars = save_pars(all = TRUE),
                    file_refit = "on_change")

mefitclone <- brm(bformse, data = meclone,
                    save_model = "male_end_clone.stan",
                    file = "male_end_clone",
                    prior = bprior,
                    #inits = initpars,
                    iter = niter,
                    cores = ncores,
                    chains = nchains,
                    sample_prior = TRUE,
                    save_pars = save_pars(all = TRUE),
                    file_refit = "on_change")

