# kfold validation ####

library(dplyr)
library(rstan)
library(loo)

source('phenology_functions.R')

# phenology data
phenbe <- filter_start_end()

# sex and event characters
prepare_data_for_kfold <- function(phensub, sex, event, group_type, groups) {
  dat <- select_data(phensub, sex, event) 
  
  # add fold markers and stratify by Site+Year+Provenance
  group <- group_by(dat, Site, Year, Provenance) %>% group_indices()
  dat$fold <- loo::kfold_split_stratified(K = 10, x = group)
}

# add fold to dataframe
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

#modify add folds function for k = length(levels) or 10, whichever is smaller

dat$fold_strat <- add_folds(phensub=dat, fold_type = "stratified", k=10, Site, Year, Provenance)
dat$fold_site <- add_folds(dat, "grouped", k = length(unique(dat$Site)), Site)
dat$fold_


dat <- select_data(phenbe, "FEMALE", "begin") 

# add fold markers and stratify by Site+Year+Provenance
group <- group_by(dat, Site, Year, Provenance) %>% group_indices()
dat$fold <- loo::kfold_split_stratified(K = 10, x = group)

# Prepare a matrix with the number of post-warmup iterations by number of observations:
log_pd_kfold <- matrix(nrow = 3500*6, ncol = nrow(dat)) #iteration x chains

# model prep

stanmodel <- rstan::stan_model('phenology.stan') # model to be fit in stan with training data
kfoldmodel <- rstan::stan_model('phenology_kfold.stan') # because of limitations in gqs around [transformed parameters](https://github.com/stan-dev/rstan/issues/714), pare down the stan model to only generate the log likelihood and declare only parameters necessary for that calculation

# Loop over the folds


for (i in 1:10) {
  data_train <- dat[dat$fold != i,] %>%
    prepare_data_for_stan(event = "begin")
  data_test <- dat[dat$fold == i,] %>%
    prepare_data_for_stan(event = "begin")
  fit <- sample_stan_model(stanmodel, data_train, kfold = TRUE, test = FALSE) 
  gen_test <- rstan::gqs(kfoldmodel, draws = as.matrix(fit), data = data_test)
  log_pd_kfold[, dat$fold == i] <- loo::extract_log_lik(gen_test)
  gc()
}

elpd_kfold <- loo::elpd(log_pd_kfold)
(elpd_kfold)
