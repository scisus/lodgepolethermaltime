# Fit a thermal time model to my phenology data

library(dplyr)
library(flowers)
library(rstan)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source('phenology_functions.R')

# choose only phenology data that is the start or end date
phenbe <- filter_start_end() 

# male_begin <- phen_by_sex_and_side(phenbe, "MALE", "begin")
# male_end <- phen_by_sex_and_side(phenbe, "MALE", "end")
# female_begin <- phen_by_sex_and_side(phenbe, "FEMALE", "begin")
# female_end <- phen_by_sex_and_side(phenbe, "FEMALE", "end")
# 
# phenbe <- rbind(male_begin, male_end, female_begin, female_end)


# set thresholds
# - Site threshold: 250
# - Provenance threshold: 150
# - Clone threshold: 10




# fit models
female_begin <- fit_model(phendat = phenbe, sex = "FEMALE", event = "begin")
female_end <- fit_model(phendat = phenbe, sex = "FEMALE", event = "end")

male_begin <- fit_model(phendat=phenbe, sex="MALE", event = "begin")
male_end <- fit_model(phendat = phenbe, sex="MALE", event = "end")


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

