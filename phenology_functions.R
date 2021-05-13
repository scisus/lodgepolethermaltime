# functions
#
# calculate censorship codes. 0 for uncensored, 1 for left censored, 2 for right censored, and 3 for no flowering record. Returns a dataframe with identifying information for each observation and a censorship code. only for start (left censoring) rn.
add_censor_indicator <- function() {
  phen <-  flowers::phenology

  # Wagner censorship
  wagnerbegin <- phen %>%
    filter(Source == "Rita Wagner") %>%
    group_by(Source, Index, Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
    summarise(censored = case_when(unique(First_RF) == min(DoY) ~ -1,
                                   unique(First_RF) > min(DoY) ~ 0,
                                   is.na(unique(First_RF)) ~ 3))

  nawag <- wagnerbegin[(is.na(wagnerbegin$censored)),] #test no nas



  walshbegin <- phen %>%
    filter(Source == "Chris Walsh") %>%
    distinct() %>%
    group_by(Source, Sex, Year, Site, Orchard) %>%
    mutate(first_group_obs = min(DoY)) %>%
    ungroup() %>%
    group_by(Source, Index, Sex, Year, Site, Orchard, Clone, Tree, X, Y) %>%
    summarise(censored = case_when(unique(First_RF) == min(first_group_obs) ~ 1,
                                   unique(First_RF) > min(first_group_obs) ~ 0,
                                   is.na(unique(First_RF)) ~ 3))


  nawal <- walshbegin[(is.na(walshbegin$censored)),] #test no nas

  censorbegin <- full_join(wagnerbegin, walshbegin) %>%
    ungroup() %>%
    mutate(Year = as.character(Year), Clone = as.character(Clone))

  naall <- censorbegin[(is.na(censorbegin$censored)),] #test no nas

  return(censorbegin)
}

# format data for model. Include only start and end dates and match them with forcing units
filter_start_end <- function(forcingname = "ristos", clim = "data/all_clim_PCIC.csv") {

  phen <-  flowers::phenology %>% # phenology data
    filter(Phenophase_Derived==2) %>% # only include flowering days
    rename(state = Phenophase_Derived)

  forcing <- read.csv(clim, header=TRUE, stringsAsFactors = FALSE) %>%
      filter(forcing_type==forcingname) # ristos consider forcing units calculated based on work of Sarvas 1972

  spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard) # provenance information for each orchard in phen

  phenbe <- dplyr::filter(phen, DoY == First_RF | DoY == Last_RF) %>%
    dplyr::left_join(forcing) %>%
    dplyr::left_join(spus) %>%
    dplyr::mutate(Year = as.character(Year), Clone = as.character(Clone)) %>%
    dplyr::rename(Provenance = SPU_Name) %>%
    distinct()

  return(phenbe)
}

# select data for stan models - separate by sex and event. can keep day of year if you want, but dropped by default. factors is a vector string of factors you'll be passing to the model and keep_day is an option for if you want to keep Day of Year in the dataframe
select_data <- function(phendat, censordat = NULL, factors, sex, event, keep_day = FALSE) {
  # checks
  assertthat::assert_that(sex %in% c("MALE", "FEMALE"), msg = "Sex should be MALE or FEMALE")
  assertthat::assert_that(event %in% c("begin", "end"), msg = "event should be begin or end")

  # filter for event and sex
  phensub <- phendat %>%
    dplyr::filter(if (event == "begin") Sex == sex & DoY == First_RF else Sex == sex & DoY == Last_RF)

  if (is.null(censordat)) {
    phensub <- phensub

  } else {
    factors <- c(factors, "censored")
    censorship <- censordat %>%
      dplyr::filter(Sex == sex) %>%
      dplyr::filter(censored != 3) # this will need to be updated when I'm considering things that aren't just left censored

    phensub <- dplyr::left_join(phensub, censorship)
  }

  if (keep_day == TRUE) {
    phensub <- phensub %>%
      dplyr::select(sum_forcing, DoY, all_of(factors))
  } else {
    phensub <- phensub %>%
      dplyr::select(sum_forcing, all_of(factors))
  }

  return(phensub)
}



# create an index to assign each level of a factor to be centered or non-centered based on a frequency threshold - levels that have more occurrences than the threshold are centered and those at or below are non-centered
create_centering_index <- function(phensub, fac, threshold) {

  # create an index for a factor for levels that should be modeled as centered or non-centered
  ncp_idx <- which(table(phensub[[fac]]) <= threshold)
  cp_idx <- which(table(phensub[[fac]]) > threshold)

  # data for stan
  k_ncp <- length(ncp_idx) # number of non centered sites
  ncp_idx <- array(ncp_idx) # non-centered sites

  k_cp <- length(cp_idx)
  cp_idx <- array(cp_idx)

  standat <- list(k_ncp, ncp_idx, k_cp, cp_idx)
  names(standat) <- paste0(c("k_ncp_", "ncp_idx_", "k_cp_", "cp_idx_"), fac)

  return(standat)
}

# Build centering vs noncentering indexes for all factors that are partially decentered. Uses create_index. dat is a phenology dataset as a dataframe, factors are a list of factors from dat that should be centered or decentered based on a threshold, e.g. list(Site = 20, Provenance = 50). Thresholds are determined in the conceptualanalysis.
build_factor_centering_indexes <- function(phensub, factor_threshold_list) {

  assertthat::assert_that(is.list(factor_threshold_list), msg = "factor_thresholds must be a list with entries like factor name = threshold, e.g. list(Site = 20, Provenance = 50)")

  nfac <- length(factor_threshold_list)

#  phensub <- select_data(phendat, censorship, sex, event)
  centering_indexes <- list()
  for (i in 1:nfac) {
    indexes <- create_centering_index(phensub = phensub, fac = names(factor_threshold_list)[i], threshold = factor_threshold_list[[i]])
    centering_indexes <- append(centering_indexes, indexes)
  }

  assertthat::assert_that(length(centering_indexes) == 4*nfac, msg = "You should have 4 entries in the list for each factor, but something has gone wrong. Check build_factor_centering_indexes")

  return(centering_indexes)
}



# Fit a model in Stan to phenology data, return the model fit object and save the model fit object to a file. Choose whether the model is for "MALE" or "FEMALE" strobili and whether the event is the "begin" or "end" of flowering. data is a dataframe of flowering data. id is an optional identifier appended to the file name.


prepare_data_for_stan <- function(phensub, factor_threshold_list, event) {

  base_data <- tidybayes::compose_data(phensub, .n_name=tidybayes::n_prefix(prefix="k")) # format data for stan

  indexes_for_partially_centered_factors <- build_factor_centering_indexes(phensub, factor_threshold_list = factor_threshold_list) # create indexes for factors with some levels centered and others noncentered

  input <- append(base_data, indexes_for_partially_centered_factors) # combine centering indices with the rest of the data for stan

  # add event-specific prior. These are determined in the conceptual analysis
  if (event == "begin") {
    input <- c(input, mu_mean=335, mu_sigma = 50)
  }

  if (event == "end") {
    input <- c(input, mu_mean=555, mu_sigma = 90)
  }

  return(input)
}

## set iterations
sample_stan_model <- function(compiledmodel, input, sex, event, appendname = NULL,
                              expars = c("alpha_ncp_site", "alpha_cp_site",
                                         "alpha_ncp_prov", "alpha_cp_prov",
                                         "z_alpha_clone"),
                              init = rep(list(list(mu = abs(rnorm(1,100,50)),
                                                   sigma = rexp(1,1),
                                                   sigma_site = rexp(1,1),
                                                   sigma_year = rexp(1,1),
                                                   sigma_prov = rexp(1,1),
                                                   sigma_clone = rexp(1,1))), 6),
                              iter = 3500, warmup = floor(iter/2), control = NULL, kfold = FALSE, test = FALSE) {

  # if the model is for kfold cross validation, then don't change the seed between runs.
  # if (kfold == FALSE) {
  #   seed = sample.int(.Machine$integer.max, 1) } else {
  #     seed = 1330 }

  if (test == TRUE) { # if you're testing the model, run just a few iterations.
    iter = 100
  } else {
    iter = iter
  }

  fit <- rstan::sampling(object = compiledmodel, chains=6, data=input, iter=iter, cores=7,
                         pars=expars, include=FALSE,
                         init = init, # stop stan from sampling impossible negative numbers
                         #seed = seed,
                         warmup = warmup,
                         control = control)


  gc() # don't eat all the RAM

  if (kfold == FALSE & test == FALSE) { # save the model fit unless you're doing kfold
    saveRDS(fit, file = paste(Sys.Date(), sex, "_", event, appendname, ".rds", sep=''))
  }

  return(fit)
}


#choose data, prepare it, and fit a model.
#phendat is a dataframe with a columns Sex (containing MALE or FEMALE), FirstRF, and LastRF (Day of year recorded flowering) and factor level for the model. Sex is "MALE" or "FEMALE". compiled model is a stan model compiled with rstan::stan_model. appendname is a string to append to the end of the file name. factors is a vector of columns in phendat that serve as factors in your model. The factor_threshold_list is a list of key-value pairs matching a factor to a threshold used for centering or non-centering. Expars is a list of parameters calculated in the model that should not be returned. Init chooses initial values that aren't impossible so stan doesn't complain.
munge_and_fit <- function(phendat, censordat = NULL, sex, event,
                                  compiledmodel, appendname = NULL,
                          factors = c("Site", "Provenance", "Year", "Clone"),
                                  factor_threshold_list = list(Site = 250, Provenance = 150),
                                  expars = c("delta_ncp_site", "delta_cp_site",
                                             "delta_ncp_prov", "delta_cp_prov",
                                             "z_delta_clone"),
                                  init = rep(list(list(mu = abs(rnorm(1,100,50)),
                                                       sigma = rexp(1,1),
                                                       sigma_site = rexp(1,1),
                                                       sigma_year = rexp(1,1),
                                                       sigma_prov = rexp(1,1),
                                                       sigma_clone = rexp(1,1))), 6),
                                  control = NULL, test = FALSE) {
  # subset data by sex and event
  phensub <- select_data(phendat, censordat = censordat, factors = factors, sex = sex, event = event)

  # add indexes and turn data into a list for stan
  input <- prepare_data_for_stan(phensub = phensub, factor_threshold_list = factor_threshold_list, event = event)


  # fit stan model

  fit <- sample_stan_model(compiledmodel = compiledmodel, input = input, sex=sex, event = event, appendname = appendname, expars = expars, init = init, control = control, test = test, kfold = FALSE)

}
