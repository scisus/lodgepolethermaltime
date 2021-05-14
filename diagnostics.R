# diagnostics for stan models
#
library(dplyr)
library(purrr)
library(rstan)

fbfit <- readRDS('2021-05-11FEMALE_begin_nofactors.rds')
fefit <- readRDS('2021-01-07FEMALE_end.rds')
mbfit <- readRDS('2021-04-29MALE_begin_censored.rds')
mefit <- readRDS('2021-01-07MALE_end.rds')

#fbpars <- data.frame(rstan::extract(fbfit))


pairs(fbfit, pars = c("mu", "sigma", "sigma_cens", "mu_site", "sigma_site", "mu_year", "sigma_year", "mu_clone", "sigma_clone", "mu_prov", "sigma_prov"))
pairs(fbfit, pars = c("mu", "sigma", "sigma_site", "sigma_year", "sigma_clone", "sigma_prov"))
pairs(fbfit, pars = c("mu", "sigma", "mu_site", "mu_year", "mu_clone", "mu_prov"))
nuts <- bayesplot::nuts_params(fbfit)
draws <- as.array(fbfit)
bayesplot::mcmc_parcoord(draws, pars = vars("mu", "sigma", starts_with("mu_"), starts_with("sigma_"), contains("alpha_site")), np=nuts, transform = function(x) {(x - mean(x)) / sd(x)})
bayesplot::mcmc_intervals(draws, pars = vars(starts_with("mu_"), starts_with("sigma_"), contains("delta_site"), "sigma"))
bayesplot::mcmc_intervals(draws, pars = vars( contains("year")))
bayesplot::mcmc_areas(draws, pars = vars(contains("sigma_")))
bayesplot::mcmc_areas(draws, pars = vars(contains("mu_")))
#female_begin_censored <- fit_model(phendat = censored, sex = "FEMALE", event = "begin")

library(shinystan)
launch_shinystan(fbfit)


 # format for Rhat and ESS calculations
#sims <- list(fbsims = as.array(fbfit), fesims = as.array(fefit), mbsims = as.array(mbfit), mesims = as.array(mefit))
#
sims <- list(fbsims = as.array(fb))

# calculate minimum effective sample size (bulk and tail) for a two-dimensional array whose rows are equal to the number of iterations of the Markov Chain(s) and whose columns are equal to the number of Markov Chains (preferably more than one).
miness <- function(sims) {
  bulk_ess <- apply(sims, MARGIN = 3, FUN = rstan::ess_bulk)
  tail_ess <- apply(sims, MARGIN = 3, FUN = rstan::ess_tail)

  bulk <- min(bulk_ess)
  tail <- min(tail_ess)

  return(data.frame(bulk_ess = bulk, tail_ess = tail))
}


minesses <- purrr::map(sims, miness) %>%
  dplyr::bind_rows(.id = "id")
# Goal is for ESS to be at least 100 for each chain. e.g. if 6 chains, you're in trouble if min ESS < 600.
print(minesses)

maxrhat <- function(sims) {
  rhats <- apply(sims, MARGIN = 3, FUN = rstan::Rhat)
  maxhat <- max(rhats)
  return(maxhat)
}

rhats <- purrr::map(sims, maxrhat)
print(rhats)

