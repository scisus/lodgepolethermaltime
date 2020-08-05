# Fit a thermal time model to my phenology data
# Only fit the mean and sd, not considering groups

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(dplyr)
library(flowers)

phen <- flowers::phenology %>% # phenology data
    filter(Phenophase_Derived==2 & Year < 2012) %>%
    rename(state = Phenophase_Derived) # only consider days when trees are flowering

forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
    filter(forcing_type=="ristos") # only consider forcing units calculated based on work of Sarvas 1972

spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard) # provenance information for each orchard in phen

# start and end phenology
phense <- dplyr::filter(phen, DoY == First_RF | DoY == Last_RF) %>%
    dplyr::left_join(forcing)

# receptivity start
phenfs <- phense %>% dplyr::filter(Sex == "FEMALE" & DoY == First_RF)

# prepare data for stan

input <- list("N" = nrow(phenfs), "forcing" = phenfs$sum_forcing)

fit <- rstan::stan(file='meanfitreal.stan', chains=4, data=input)

# plot modeled and true data

get_variables(fit)
ypred <- tidybayes::gather_draws(model = fit, `y_ppc*`[i], regex=TRUE)

ggplot(ypred, aes(x=.value, group = as.factor(.chain), color="model")) +
    geom_density() +
    geom_density(data=phenfs, aes(x=sum_forcing, color="data"), inherit.aes = FALSE) +
    ggtitle("observed data and model output")
