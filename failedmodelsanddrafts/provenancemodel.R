# Provenance model: genotype offsets from thermal time model as a function of provenance

library(dplyr)
library(brms)
library(report)

# get data
# response
genotype_offsets <- readRDS("objects/genotyper.rds") %>%
  mutate(Genotype = as.character(level))

# predictors
prov_climate <- read.csv("../lodgepole_climate/data/climateBC/climatebc_parent_locs_Normal_1961_1990Y_v730.csv") %>%
  rename(Genotype = id1, SPZ = id2)  %>%
  select(Genotype, SPZ, MAT) %>%
  mutate(Genotype = as.character(Genotype))


# munge data
dat <- genotype_offsets %>%
  # summarize genotype info
  group_by(model, Sex, event, Genotype) %>%
  summarise(meanoffset = mean(.value), sdoffset = sd(.value)) %>%
  # combine with parent info
  left_join(prov_climate)
saveRDS(dat, "objects/genotypedat.rds")

fbgenotype <- filter(dat, Sex == "FEMALE", event == "begin")
fegenotype <- filter(dat, Sex == "FEMALE", event == "end")

mbgenotype <- filter(dat, Sex == "MALE", event == "begin")
megenotype <- filter(dat, Sex == "MALE", event == "end")

bformse <- brmsformula(meanoffset | se(sdoffset, sigma = TRUE) ~ MAT)

bprior <- c(prior("normal(0,10)", class = "Intercept"),
            prior("normal(0,9)", class = "sigma"),
            prior("normal(0,5)", class = "b"))

ncores = 6
nchains = 6
niter = 3000

fbfitgenotype <- brm(bformse, data = fbgenotype,
            save_model = "female_begin_genotype.stan",
            file = "female_begin_genotype",
            prior = bprior,
            iter = niter,
            cores = ncores,
            chains = nchains,
            sample_prior = TRUE,
            save_pars = save_pars(all = TRUE),
            file_refit = "on_change")

mbfitgenotype <- brm(bformse, data = mbgenotype,
                    save_model = "male_begin_genotype.stan",
                    file = "male_begin_genotype",
                    prior = bprior,
                    #inits = initpars,
                    iter = niter,
                    cores = ncores,
                    chains = nchains,
                    sample_prior = TRUE,
                    save_pars = save_pars(all = TRUE),
                    file_refit = "on_change")

fefitgenotype <- brm(bformse, data = fegenotype,
                    save_model = "female_end_genotype.stan",
                    file = "female_end_genotype",
                    prior = bprior,
                    #inits = initpars,
                    iter = niter,
                    cores = ncores,
                    chains = nchains,
                    sample_prior = TRUE,
                    save_pars = save_pars(all = TRUE),
                    file_refit = "on_change")

mefitgenotype <- brm(bformse, data = megenotype,
                    save_model = "male_end_genotype.stan",
                    file = "male_end_genotype",
                    prior = bprior,
                    #inits = initpars,
                    iter = niter,
                    cores = ncores,
                    chains = nchains,
                    sample_prior = TRUE,
                    save_pars = save_pars(all = TRUE),
                    file_refit = "on_change")

