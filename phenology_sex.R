# Fit a thermal time model to my phenology data

library(dplyr)
library(flowers)
library(rstan)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


phen <- flowers::phenology %>% # phenology data
    filter(Phenophase_Derived==2 & Year < 2012) %>% # need to remake all_clim_PCIC.csv to include 2012
    rename(state = Phenophase_Derived) # only consider days when trees are flowering

forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
    filter(forcing_type=="ristos") # only consider forcing units calculated based on work of Sarvas 1972

spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard) # provenance information for each orchard in phen

# filter phenology dataset to only include start and end phenology
phenbe <- dplyr::filter(phen, DoY == First_RF | DoY == Last_RF) %>%
    dplyr::left_join(forcing) %>%
    dplyr::left_join(spus) %>%
    dplyr::mutate(Year = as.character(Year), Clone = as.character(Clone)) %>%
    dplyr::rename(Provenance = SPU_Name)


# Fit a model in Stan to phenology data, return the model fit object and save the model fit object to a file. Choose whether the model is for "MALE" or "FEMALE" strobili and whether the event is the "begin" or "end" of flowering. data is a dataframe of flowering data. id is an optional identifier appended to the file name.
fit_model <- function(data, model = "phenology.stan", maxtreedepth=10) {
    
    # receptivity start
    phensub <- data %>% 
        dplyr::rename(Begin = First_RF, End = Last_RF) %>%
        dplyr::select(sum_forcing, Sex, Site, Year, Provenance, Clone, Begin, End) %>%
        tidyr::pivot_longer(cols=c(Begin, End), names_to = "Event") %>%
        select(-value)
    
    # prepare data for stan
    
    input <- tidybayes::compose_data(phensub)
    
    fit <- rstan::stan(file= model, chains=8, data=input, iter=3500, cores=9, 
                       pars=c("z_clone_alpha", "z_year_alpha"), include=FALSE, 
                       init = rep(list(list(mu = abs(rnorm(1,100,50)), 
                                            sigma = rexp(1,1), 
                                            sigma_site = rexp(1,1), 
                                            sigma_year = rexp(1,1), 
                                            sigma_prov = rexp(1,1), 
                                            sigma_clone = rexp(1,1))), 8),
                       control = list(max_treedepth = maxtreedepth, adapt_delta=0.9))
    
    gc()
    
    saveRDS(fit, file = paste(Sys.Date(), "_", ".rds", sep=''))
    
    return(fit)
}

all <- fit_model(data = phenbe, model = "phenology_sex.stan")
# female_end <- fit_model(data = phenbe, sex = "FEMALE", event = "end")
# 
# male_begin <- fit_model(data=phenbe, sex="MALE", event = "begin")
# male_end <- fit_model(data = phenbe, sex="MALE", event = "end")


# plot modeled and true data

# tidybayes::get_variables(fit)
# 
# withtypes <- fit %>%
#     recover_types(phenfs) 
# 
# site <- gather_draws(withtypes, site_offset[Site]) %>% 
#     dplyr::rename(.label = Site)
# prov <- gather_draws(withtypes, prov_offset[Provenance]) %>%
#     dplyr::rename(.label = Provenance)
# year <- gather_draws(withtypes, prov_offset[Year])
# 
# siteprovmodoffsets <- bind_rows(site, prov)
# 
# ggplot(siteprovmodoffsets, aes(x=.value, colour=.variable, group=.label)) +
#     geom_density()
#     
# ypred <- fit %>%
#     recover_types(phenfs) %>%
#     gather_draws(y_ppc[Site])
# # ypred <- tidybayes::gather_draws(model = fit, `y_ppc*`[i], regex=TRUE)
# # ypred <- tidybayes::gather_draws(model = fit, y_ppc[Site])
# 
# ggplot(ypred, aes(x=.value, colour=Site)) +
#     geom_density() +
#     geom_density(data=phenfs, aes(x=sum_forcing, fill=Site), inherit.aes = FALSE, alpha=0.5) +
#     ggtitle("observed data and model output with site and provenance") +
#     facet_wrap("Site")

