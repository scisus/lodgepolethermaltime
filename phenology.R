# Fit a thermal time model to lodgepole pine flowering phenology data

library(dplyr)
library(flowers)
library(rstan)
library(tidybayes)

#rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#source('calculate_forcingunits.R')
source('phenology_functions.R')

# data
censorbegin <- add_censor_indicator()

phenbe <- filter_start_end() #%>%
#   filter(Site != "KettleRiver") %>%
#   mutate(event = case_when(DoY == First_RF ~ "begin",
#                            DoY == Last_RF ~ "end"))

# visualize

censorsummary <- censorbegin %>%
  group_by(Site, Sex) %>%
  summarise(percent_censored = sum(100*censored)/n())

ggplot(censorsummary, aes(x = Site, y = percent_censored)) +
  geom_bar(stat="identity") +
  facet_wrap("Sex")


ggplot(phenbe, aes(x=sum_forcing, colour = Site)) +
  stat_ecdf() +
  facet_grid(Sex ~ event)

ggplot(phenbe, aes(x = Site, y=sum_forcing)) +
  geom_quasirandom(pch = 1, alpha = 0.5) +
  facet_grid(Sex ~ event)

ggplot(phenbe, aes(x = Provenance, y=sum_forcing)) +
  geom_quasirandom(pch = 1, alpha = 0.5) +
  facet_grid(Sex ~ event)

ggplot(phenbe, aes(x = Year, y=sum_forcing)) +
  geom_quasirandom(pch = 1, alpha = 0.5) +
  facet_grid(Sex ~ event)


# compile model
phenologymodel <- rstan::stan_model("phenology_nofactors.stan", auto_write = FALSE)


# set options for models
factors <- c("Site", "Provenance", "Year", "Genotype")
factor_threshold_list <- list(Site = 250, Provenance = 150, Year = 181)
expars <- c("delta_ncp_site", "delta_cp_site",
           "delta_ncp_prov", "delta_cp_prov",
           "delta_ncp_year", "delta_cp_year",
           "z_delta_genotype")
init <- rep(list(list(mu = abs(rnorm(1,100,50)),
                     sigma = rexp(1,1),
                     sigma_site = rexp(1,1),
                     sigma_year = rexp(1,1),
                     sigma_prov = rexp(1,1),
                     sigma_genotype = rexp(1,1))), 6)

# options for censored data models
# warmup = 1500, iter = 4500, control = list(max_treedepth = 12),
#


# incremental model testing
fbdat <- select_data(phenbe, censordat = censorbegin, factors = factors, sex = "FEMALE", event = "begin")
# mbdat <- select_data(phenbe, censordat = censorbegin, factors = factors, sex = "MALE", event = "begin")
# dat <- select_data(phenbe, factors = factors, sex = "FEMALE", event = "begin")
fbinput <- prepare_data_for_stan(fbdat, factor_threshold_list = factor_threshold_list, event = "begin")
fbfit <- sample_stan_model(phenologymodel, input = fbinput, sex = "FEMALE", event = "begin", expars=expars, appendname = "_nofactors")#, warmup = 3000, iter = 4500, control = list(adapt_delta = 0.9))

female_begin <- munge_and_fit(phendat = phenbe, censordat = censorbegin, sex = "FEMALE", event = "begin", appendname = "_censored", compiledmodel = phenologymodel,
                              factors = factors, factor_threshold_list = factor_threshold_list,
                              expars = expars, init = init, control = list(adapt_delta = 0.9))

#female_begin <- fit_model(phendat = phenbe, sex = "FEMALE", censorship = censorbegin, event = "begin", maxtreedepth = 12, iter = 4500, warmup = 1500)
#male_begin <- fit_model(phendat = phenbe, sex = "MALE", censorship = censorbegin, event = "begin", maxtreedepth = 12, iter = 4500, warmup = 1500)


female_end <- munge_and_fit(phendat = phenbe, sex = "FEMALE", event = "end", compiledmodel = phenologymodel,
                            factors = factors, factor_threshold_list = factor_threshold_list,
                            expars = expars, init = init)

male_begin <- munge_and_fit(phendat = phenbe, censordat = censorbegin, sex = "MALE", event = "begin", compiledmodel = phenologymodel, appendname = "_censored",
                            factors = factors, factor_threshold_list = factor_threshold_list,
                            expars = expars, init = init, control = list(adapt_delta = 0.95))

male_end <- munge_and_fit(phendat = phenbe, sex = "MALE", event = "end", compiledmodel = phenologymodel,
                          factors = factors, factor_threshold_list = factor_threshold_list,
                          expars = expars, init = init)


