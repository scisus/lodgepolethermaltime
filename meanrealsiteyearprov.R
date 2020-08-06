# Fit a thermal time model to my phenology data
# Only fit the mean and sd, not considering groups

library(dplyr)
library(flowers)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

phen <- flowers::phenology %>% # phenology data
    filter(Phenophase_Derived==2 & Year < 2012) %>%
    rename(state = Phenophase_Derived) # only consider days when trees are flowering

forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
    filter(forcing_type=="ristos") # only consider forcing units calculated based on work of Sarvas 1972

spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard) # provenance information for each orchard in phen

# start and end phenology
phense <- dplyr::filter(phen, DoY == First_RF | DoY == Last_RF) %>%
    dplyr::left_join(forcing) %>%
    dplyr::left_join(spus) %>%
    dplyr::mutate(Year = as.character(Year)) %>%
    dplyr::rename(Provenance = SPU_Name)


# receptivity start
phenfs <- phense %>% dplyr::filter(Sex == "FEMALE" & DoY == First_RF) %>%
    dplyr::select(Site, sum_forcing, Year, Provenance)

# prepare data for stan

input <- tidybayes::compose_data(phenfs)


fit <- rstan::stan(file='meanfitrealsiteyearprov.stan', chains=4, data=input, iter=3000)
print(fit)

library(shinystan)
launch_shinystan(fit)

# plot modeled and true data

get_variables(fit)

ypred <- fit %>%
    recover_types(phenfs) %>%
    gather_draws(y_ppc[Site])
# ypred <- tidybayes::gather_draws(model = fit, `y_ppc*`[i], regex=TRUE)
# ypred <- tidybayes::gather_draws(model = fit, y_ppc[Site])

ggplot(ypred, aes(x=.value, colour=Site)) +
    geom_density() +
    geom_density(data=phenfs, aes(x=sum_forcing, fill=Site), inherit.aes = FALSE, alpha=0.5) +
    ggtitle("observed data and model output with site and provenance") +
    facet_wrap("Site")

