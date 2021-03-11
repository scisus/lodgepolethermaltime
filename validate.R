# validate and compare models with efficient approximate leave one out cross validation using pareto smoothed importance sampling

library(rstan)
library(loo)
library(dplyr)

source('phenology_functions.R')

# phenology data

phenbe <- filter_start_end()
ristodat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 
ristodat$i <- 1:nrow(dat)

# add gdd data

#dat$sum_forcing_gdd <-# 
dat <- filter_start_end(forcingname = "gdd") %>%
  select_data("FEMALE", "begin", keep_day = TRUE) %>%
  mutate(i = 1:n()) %>%
  rename(gdd = sum_forcing) %>%
  full_join(ristodat) %>%
  rename(ristos = sum_forcing)
  
  

fbfitcombined <- readRDS('2021-03-03FEMALE_beginSY.rds')
fbfitseparate <- readRDS('2021-03-09FEMALE_begin.rds')
fbfitgdd <- readRDS('2021-03-09FEMALE_begin_gdd.rds')

llcombined <- loo::extract_log_lik(fbfitcombined, merge_chains = FALSE) # pull log likelihoods for waic & loo calculations
llseparate <- loo::extract_log_lik(fbfitseparate, merge_chains = FALSE)
llgdd <- loo::extract_log_lik(fbfitgdd, merge_chains = FALSE)

# use loo instead, too many p_waic estimates > 0.5
# loo::waic(llcombined) 
# loo::waic(llseparate)
# loo::waic(loo::extract_log_lik(fbfitgdd))

loocombined <- loo::loo(llcombined, r_eff = relative_eff(exp(llcombined)), save_psis = TRUE, cores = 10)
looseparate <- loo::loo(llseparate, r_eff = relative_eff(exp(llseparate)), save_psis = TRUE, cores = 10)
loogdd <- loo::loo(llgdd, r_eff = relative_eff(exp(llgdd)), save_psis = TRUE, cores = 10)

loo_compare(loocombined, looseparate, loogdd)

dat$paretokcombined <-loocombined$diagnostics$pareto_k
dat$paretoseparate <- looseparate$diagnostics$pareto_k
dat$paretogdd <- loogdd$diagnostics$pareto_k

loodat <- pivot_longer(dat, starts_with("pareto"), names_to = "model", values_to = "pareto_k") %>%
  pivot_longer(cols = c("gdd", "ristos"), names_to = "forcingtype", values_to = "sum_forcing")

ggplot(loodat, aes(x=i, y=pareto_k, colour = Site)) +
  geom_point(shape = 3) +
  geom_hline(yintercept = c(0.5, 0.7), lty=2, colour = "darkgray") +
  facet_wrap("model")



