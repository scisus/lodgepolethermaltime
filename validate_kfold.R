# kfold validation ####

library(dplyr)
library(rstan)
library(loo)

source('phenology_functions.R')




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

# phenology data
phenbe <- filter_start_end()

dat <- select_data(phenbe, "FEMALE", "begin", factors = c("Site", "Year", "Provenance", "Clone")) 

# identify folds
dat$fold_strat <- add_folds(phensub=dat, fold_type = "stratified", k=10, Site, Year, Provenance)
dat$fold_grouped <- add_folds(dat, "grouped", k = 10, Site, Year, Provenance, Clone)



# model prep

# full model
stanmodel_full <- rstan::stan_model('phenology.stan') # model to be fit in stan with training data
kfoldmodel_full <- rstan::stan_model('phenology_kfold.stan') # because of limitations in gqs around [transformed parameters](https://github.com/stan-dev/rstan/issues/714), pare down the stan model to only generate the log likelihood and declare only parameters necessary for that calculation

# base model
stanmodel_base <- rstan::stan_model('phenology_mean.stan')
kfoldmodel_base <- rstan::stan_model('phenology_mean_kfold.stan')

# create a log likelihood matrix to feed to `loo::elpd`.
# 
# for a set of observations in phenology `dat`, loop over all folds `k`
# fit a model `fitmodel` to all phenology observations except ones in the left out fold
# then
# compute log pointwise predictive densities for the left out fold using `kfoldmodel`
# store and return the predictive density for observations of left out fold
# setting iterations and chains does not change them 
# fitmodel and kfoldmodel should be the same, but rstan::gqs fumbles when there are transformed parameters in the model
# kfoldmodel allows you to re-specify your model sans extraneous parameter calculations and transformations for feeding into gqs.
# kfoldmodel *must* have a generated quantities block that calculates a vector log_lik
# event is a string indicating whether the phenological event is the "begin" or "end" of flowering.
# feeds the correct priors to stan
# loop can/should be parallelized
loop_kfold <- function(dat, datfold, event, fitmodel, kfoldmodel, k, iter = 1750, chains =6) {
  
  # Prepare a matrix with the total number of post-warmup iterations (rows) by number of observations (cols):
  log_pd_kfold <- matrix(nrow = iter * chains, ncol = nrow(dat)) 
  
  # Loop over the folds
  
  
  for (i in 1:k) {
    data_train <- dat[datfold != i,] %>%
      prepare_data_for_stan(event = event, factor_threshold_list = list(Site = 250, Provenance = 150))
    data_test <- dat[datfold == i,] %>%
      prepare_data_for_stan(event = event, factor_threshold_list = list(Site = 250, Provenance = 150))
    fit <- sample_stan_model(fitmodel, data_train, kfold = TRUE, test = FALSE) 
    gen_test <- rstan::gqs(kfoldmodel, draws = as.matrix(fit), data = data_test)
    log_pd_kfold[, datfold == i] <- loo::extract_log_lik(gen_test)
    gc()
  }
  
  return(log_pd_kfold)
}

# rewrite loop_kfold as a ||izable function ########
one_loop <- function(fold) {
    
    data_train <- dat[datfold != fold,] %>%
      prepare_data_for_stan(event = "begin", factor_threshold_list = list(Site = 250, Provenance = 150))
    data_test <- dat[datfold == fold,] %>%
      prepare_data_for_stan(event = "begin", factor_threshold_list = list(Site = 250, Provenance = 150))
    fit <- sample_stan_model(fitmodel, data_train, kfold = TRUE, test = FALSE) 
    gen_test <- rstan::gqs(kfoldmodel, draws = as.matrix(fit), data = data_test)
    log_pd_kfold[, datfold == fold] <- loo::extract_log_lik(gen_test)
    gc()
  }

loop_kfold_parallel <- function(dat, datfold, event, fitmodel, kfoldmodel, k, cores, iter = 1750, chains = 6) {
  folds <- seq(1,k)
  # Prepare a matrix with the number of post-warmup iterations by number of observations:
  log_pd_kfold <- matrix(nrow = iter * chains, ncol = nrow(dat)) #iteration x chains
  
  # Loop over the folds
  
  
  for (i in 1:k) {
    data_train <- dat[datfold != i,] %>%
      prepare_data_for_stan(event = event, factor_threshold_list = list(Site = 250, Provenance = 150))
    data_test <- dat[datfold == i,] %>%
      prepare_data_for_stan(event = event, factor_threshold_list = list(Site = 250, Provenance = 150))
    fit <- sample_stan_model(fitmodel, data_train, kfold = TRUE, test = FALSE) 
    gen_test <- rstan::gqs(kfoldmodel, draws = as.matrix(fit), data = data_test)
    log_pd_kfold[, datfold == i] <- loo::extract_log_lik(gen_test)
    gc()
  }
  
  return(log_pd_kfold)
}
#############

fullstrat <- loop_kfold(dat, datfold=dat$fold_strat, event = "begin", fitmodel = stanmodel_full, kfoldmodel = kfoldmodel_full, k=10)

elpd_kfold <- loo::elpd(log_pd_kfold)
(elpd_kfold)
