# Fit a thermal time model to my phenology data
# Only fit the mean and sd, not considering groups

library(dplyr)
library(flowers)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


sex="FEMALE"

phen <- flowers::phenology %>% # phenology data
    filter(Phenophase_Derived==2 & Year <= 2012) %>%
    rename(state = Phenophase_Derived) # only consider days when trees are flowering

forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
    filter(forcing_type=="ristos") # only consider forcing units calculated based on work of Sarvas 1972

spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard) # provenance information for each orchard in phen

# start and end phenology
phense <- dplyr::filter(phen, DoY == First_RF | DoY == Last_RF) %>%
    dplyr::left_join(forcing) %>%
    dplyr::left_join(spus) %>%
    dplyr::mutate(Year = as.character(Year), Clone = as.character(Clone)) %>%
    dplyr::rename(Provenance = SPU_Name)


# receptivity start
phenfs <- phense %>% dplyr::filter(Sex == "FEMALE" & DoY == First_RF) %>%
    dplyr::select(sum_forcing, Site, Year, Provenance, Clone) %>%
    rename(sum_forcing_obs = sum_forcing)

# prepare data for stan

input <- tidybayes::compose_data(phenfs)


fit <- rstan::stan(file='meanfitrealsiteyearprovclone_censor.stan', chains=5, data=input, iter=1e4, control = list(adapt_delta=0.99, max_treedepth=11), cores=20, pars=c("z_clone_offset"), include=FALSE)

saveRDS(fit, file = paste(Sys.Date(), "sypc_censor", sex, ".rds", sep=''))

pprint(fit)

# library(shinystan)
# launch_shinystan(fit)

# plot modeled and true data

get_variables(fit)

withtypes <- fit %>%
    recover_types(phenfs) 

site <- gather_draws(withtypes, site_offset[Site]) %>% 
    dplyr::rename(.label = Site)
prov <- gather_draws(withtypes, prov_offset[Provenance]) %>%
    dplyr::rename(.label = Provenance)
year <- gather_draws(withtypes, prov_offset[Year])

siteprovmodoffsets <- bind_rows(site, prov)

ggplot(siteprovmodoffsets, aes(x=.value, colour=.variable, group=.label)) +
    geom_density()
    
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

