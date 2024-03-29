# kfoldcv validation ####

# I tried to use k-fold cross validation to understand how my model performed relative to a simpler model and
# also to see how predictive ability varied for new levels of factors. However, I was unable to get it to work.
# Leaving data out to create the training set makes it impossible to fit the model. Honestly, this isn't a good sign. Suggests my model is quite fragile and likely mis-specified.

library(dplyr)
library(rstan)
library(loo)

source('phenology_functions.R')

#### functions ######
# add fold to dataframe phensub using fold_type "stratified" or "grouped" and grouping variables ...
# if K < number of groups, kfold will fail. TODO: add check and automatic k recommendation
add_folds <- function(phensub, fold_type, k = 10, ...) {
  group <- group_by(phensub, ...) %>%
    group_indices()

  if (fold_type == "stratified") {
    fold <- loo::kfold_split_stratified(K=k, x = group)
  }

  if (fold_type == "grouped") {
    fold <- loo::kfold_split_grouped(K=k, x = group)
  }
  return(fold)
}

# create a log likelihood matrix to feed to `loo::elpd`.
#
# for a set of observations in phenology `dat`, loop over all folds `k`
# fit a model `fitmodel` to all phenology observations except ones in the left out fold
# then
# compute log pointwise predictive densities for the left out fold using `kfoldmodel`
# store and return the predictive density for observations of left out fold in a matrix of iter/2 * chains (total samples after warmup) x nrow(dat) (# of datapoints)
# fitmodel and kfoldmodel should be the same, but rstan::gqs fumbles when there are transformed parameters in the model
# kfoldmodel allows you to re-specify your model sans extraneous parameter calculations and transformations for feeding into gqs.
# kfoldmodel *must* have a generated quantities block that calculates a vector log_lik
# event is a string indicating whether the phenological event is the "begin" or "end" of flowering.
# feeds the correct priors to stan
# loop can/should be parallelized
loop_kfold <- function(dat, censordat = NULL, datfold, event, fitmodel, kfoldmodel, iter = 4000, chains = 6, control=NULL, test = FALSE) {

  k <- length(unique(datfold))

  # Prepare a matrix with the total number of post-warmup iterations (rows) by number of observations (cols):
  #log_pd_kfold_specific <- matrix(nrow = iter * chains, ncol = nrow(dat))
  log_pd_kfold_general <- matrix(nrow = iter/2 * chains, ncol = nrow(dat))

  # Loop over the folds

  #uncensored data
  for (i in 1:k) {
  paste("LOOP", i)
    data_train <- dat[datfold != i,] %>%
      prepare_data_for_stan(event = event, factor_threshold_list = list(Site = 250, Provenance = 150, Year = 181))
    data_test <- dat[datfold == i,] %>%
      prepare_data_for_stan(event = event, factor_threshold_list = list(Site = 250, Provenance = 150, Year = 181))
    fit <- sample_stan_model(fitmodel, data_train, iter = iter, control = control, kfold = TRUE, test = test)
    gen_test <- rstan::gqs(kfoldmodel, draws = as.matrix(fit), data = data_test)
    #log_pd_kfold_specific[, datfold == i] <- loo::extract_log_lik(gen_test, parameter_name = "log_lik_specific")
    log_pd_kfold_general[, datfold == i] <- loo::extract_log_lik(gen_test, parameter_name = "log_lik_general")

    gc()
  }


  #return(list(specific = log_pd_kfold_specific, general = log_pd_kfold_general))
  return(log_pd_kfold_general)
}

# data #####
# phenology data
phenbe <- filter_start_end()
censorind <- add_censor_indicator()

dat <- select_data(phenbe, factors = c("Site", "Year", "Provenance", "Clone"), sex = "MALE", event ="begin")
cdat <- select_data(phenbe, censordat = censorind, factors = c("Site", "Year", "Provenance", "Clone"), sex = "MALE", event ="begin")

# script ######

# identify folds
 dat$fold <- loo::kfold_split_random(K = 30, N = nrow(dat))
 cdat$fold <- loo::kfold_split_random(K = 30, N = nrow(cdat))
 dat$fold_strat <- add_folds(phensub=dat, fold_type = "stratified", k=10, Site, Provenance, Year)
 dat$fold_clone <- add_folds(phensub = dat, fold_type = "grouped", k=20, Clone)
# dat$fold_group <- add_folds(phensub=dat, fold_type = "grouped", k = 10, Year, Clone)


# model prep

# full model
stanmodel_full <- rstan::stan_model('phenology.stan') # model to be fit in stan with training data
kfoldmodel_full <- rstan::stan_model('kfoldcv/phenology_kfold.stan') # because of limitations in gqs around [transformed parameters](https://github.com/stan-dev/rstan/issues/714), pare down the stan model to only generate the log likelihood and declare only parameters necessary for that calculation

# censorship model
censored_full <- rstan::stan_model('phenology_censored.stan')
kfold_censored_full <- rstan::stan_model('phenology_censored_kfold.stan')
# base model
#stanmodel_base <- rstan::stan_model('kfoldcv/phenology_mean.stan')

# # base + factor
# stanmodel_site <- rstan::stan_model('kfoldcv/phenology_site.stan')
# #stanmodel_prov <- rstan::stan_model('kfoldcv/phenology_prov.stan')
# stanmodel_clone <- rstan::stan_model('kfoldcv/phenology_clone.stan')



#############
# random

fullrand <- loop_kfold(dat, datfold = dat$fold, event="begin", fitmodel = censored_full, kfoldmodel = kfold_censored_full, test=TRUE)
# stratified
saveRDS(fullrand, "testkfold.rds")
foo <- readRDS("testkfold.rds")
fullstrat <- loop_kfold(dat, datfold=dat$fold_strat, event = "begin", fitmodel = stanmodel_full, kfoldmodel = kfoldmodel_full, iter=2000)

#pggroup <- loop_kfold(dat, datfold=dat$fold_pg, event = "begin", fitmodel = stanmodel_full, kfoldmodel = kfoldmodel_full)
clonegroup <- loop_kfold(dat, datfold = dat$fold_clone, event="begin", fitmodel = stanmodel_full, kfoldmodel = kfoldmodel_full, iter=2000)
loo::elpd(clonegroup)
basestrat <- loop_kfold(dat, datfold=dat$fold_strat, event = "begin", fitmodel = stanmodel_base, kfoldmodel = stanmodel_base)

elpd_kfold_fullstrat_specific <- loo::elpd(fullstrat[[1]])
elpd_kfold_fullstrat <- loo::elpd(fullstrat)

loo_compare(elpd_kfold_fullstrat_specific, elpd_kfold_fullstrat_general)

loo_compare(elpd_kfold_fullstrat_specific, elpd_kfold_fullstrat_general)

# grouped
fullgroup <- loop_kfold(dat, datfold=dat$fold_grouped, event = "begin", fitmodel = stanmodel_full, kfoldmodel = kfoldmodel_full, k=10)
basegroup <- loop_kfold(dat, datfold=dat$fold_grouped, event = "begin", fitmodel = stanmodel_base, kfoldmodel = stanmodel_base, k=10)


elpd_kfold_fullgroup_specific <- loo::elpd(fullgroup[[1]])
elpd_kfold_fullgroup_general <- loo::elpd(fullgroup[[2]])




loo_compare(elpd_kfold_fullstrat, elpd_kfold_basestrat, elpd_kfold_sitestrat)
loo_compare(elpd_kfold_basegroup, elpd_kfold_fullgroup)
loo_compare(elpd_kfold_fullstrat, elpd_kfold_fullgroup) #because folds are different, don't think i can compare

