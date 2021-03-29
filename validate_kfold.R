# kfold validation ####

library(dplyr)
library(rstan)
library(loo)

source('phenology_functions.R')

# phenology data
phenbe <- filter_start_end()
dat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 

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
}

elpd_kfold <- loo::elpd(log_pd_kfold)
(elpd_kfold)
