# diagnostics for stan models
# 
library(dplyr)
library(purrr)
library(rstan)

fbfit <- readRDS('2020-09-03FEMALE_begin.rds')
fefit <- readRDS('2020-09-03FEMALE_end.rds')
mbfit <- readRDS('2020-09-03MALE_begin.rds')
mefit <- readRDS('2020-09-03MALE_end.rds')

fbpars <- data.frame(rstan::extract(fbfit))


library(shinystan)
shinystan::launch_shinystan(fefit)


# format for Rhat and ESS calculations
sims <- list(fbsims = as.array(fbfit), fesims = as.array(fefit), mbsims = as.array(mbfit), mesims = as.array(mefit))

# calculate minimum effective sample size (bulk and tail) for a two-dimensional array whose rows are equal to the number of iterations of the Markov Chain(s) and whose columns are equal to the number of Markov Chains (preferably more than one). 
miness <- function(sims) {
  bulk_ess <- apply(sims, MARGIN = 3, FUN = rstan::ess_bulk)
  tail_ess <- apply(sims, MARGIN = 3, FUN = rstan::ess_tail)
  
  bulk <- min(bulk_ess)
  tail <- min(tail_ess)
  
  return(data.frame(bulk_ess = bulk, tail_ess = tail))
}


minesses <- purrr::map(sims, miness) %>%
  dplyr::bind_rows()

maxrhat <- function(sims) {
  rhats <- apply(sims, MARGIN = 3, FUN = rstan::Rhat)
  maxhat <- max(rhats)
  return(maxhat)
}

rhats <- purrr::map(sims, maxrhat)
