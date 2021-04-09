# Fit a thermal time model to lodgepole pine flowering phenology data

library(dplyr)
library(flowers)
library(rstan)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#source('calculate_forcingunits.R') #check if this is necessary here
source('phenology_functions.R')

# pull only phenology data that is the start or end date from the flowers package
phenbe <- filter_start_end() %>%
  dplyr::mutate(SiteYear = paste0(Site, Year)) %>%
  filter(Year > 2006 & Year != 2012)

# compile model
phenologymodel <- rstan::stan_model("phenology_interaction.stan")

# thresholds
ggplot(phenbe, aes(x=Site, fill=Sex)) +
  geom_histogram(stat="count", position="dodge")

ggplot(phenbe, aes(x=Provenance, fill=Sex)) +
  geom_histogram(stat="count", position="dodge")

ggplot(phenbe, aes(x=Year, fill=Sex)) +
  geom_histogram(stat="count", position="dodge")

ggplot(phenbe, aes(x=SiteYear, fill=Sex)) +
  geom_histogram(stat="count", position="dodge")


# fit models
female_begin <- munge_and_fit(phendat = phenbe, sex = "FEMALE", event = "begin", compiledmodel = phenologymodel, factors = c("Site", "Year", "Provenance", "Clone", "SiteYear"), factor_threshold_list = list(Site = 250, Provenance = 150, SiteYear = 400), appendname = "balancedyearsinteraction_sycentered")
    
female_end <- munge_and_fit(phendat = phenbe, sex = "FEMALE", event = "end", compiledmodel = phenologymodel)

male_begin <- munge_and_fit(phendat = phenbe, sex = "MALE", event = "begin", compiledmodel = phenologymodel)
male_end <- munge_and_fit(phendat = phenbe, sex = "MALE", event = "end", compiledmodel = phenologymodel)


