# diagnostics for stan models
# 
library(dplyr)
library(purrr)
library(rstan)

fbfit <- readRDS('2020-08-24FEMALE_begin.rds')
fefit <- readRDS('2020-08-24FEMALE_end.rds')
mbfit <- readRDS('2020-08-24MALE_begin.rds')
mefit <- readRDS('2020-08-24MALE_end.rds')

library(shinystan)
shinystan::launch_shinystan(mbfit)

sims <- list(fbsims = as.array(fbfit), fesims = as.array(fefit), mbsims = as.array(mbfit), mesims = as.array(mefit))

# calculate minimum effective sample size (bulk and tail) for a two-dimensional array whose rows are equal to the number of iterations of the Markov Chain(s) and whose columns are equal to the number of Markov Chains (preferably more than one). 
miness <- function(sims) {
  bulk_ess <- apply(sims, MARGIN = 3, FUN = rstan::ess_bulk)
  tail_ess <- apply(sims, MARGIN = 3, FUN = rstan::ess_tail)
  
  bulk <- min(bulk_ess)
  tail <- min(tail_ess)
  
  return(data.frame(bulk_ess = bulk, tail_ess = tail))
}

miness(mbfit)

minesses <- purrr::map(sims, miness)

rhats <- purrr::map(sims, Rhat)
