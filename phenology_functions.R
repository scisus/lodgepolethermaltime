# functions
#
# calculate censoring status of each observation. left, right, and interval. Assumes no uncensored data - uncensored data will be treated as left or right censored - and that all trees flowered. Returns a dataframe with identifying information for each observation and a censorship code. takes wide form of data
add_censor_indicator <- function(phenevent) {

  #calculate indexes of trees with before flowering or past flowering observations

  have1 <- unique(phenevent[which(phenevent$State == 1), "Index"]) # trees with an event 1/state 1 obs
  have4 <- unique(phenevent[which(phenevent$State == 3), "Index"]) # trees with an event 4/state 3 obs

  # add censoring labels
  censorind <- phenevent %>%
    mutate(censored = case_when(State %in% c(1,3) ~ "interval", # because all trees have a flowering observation, all state 1 and 3 observations are interval censored
                                # if no event/state 1 was observed, then event 2 obs are left censored, otherwise they are interval censored
                                Event_Obs == 2 & !Index %in% have1 ~ "left",
                                Event_Obs == 2 & Index %in% have1 ~ "interval",
                                # if no event 4/state 3 was observed, then event 3 obs are left censored, otherwise they are interval censored
                                Event_Obs == 3 & !Index %in% have4 ~ "right",
                                Event_Obs == 3 & Index %in% have4 ~ "interval"))

  assertthat::are_equal(which(is.na(censorind$censored)), integer(0))
  assertthat::are_equal(nrow(censorind), nrow(phenevent))

  return(censorind)
}

# add_forcing_data <-

# format data for model. Include only start and end dates and match them with forcing units
# filter_start_end <- function(forcingname = "ristos", clim = "data/all_clim_PCIC.csv") {
#
#   phen <-  flowers::phenology %>% # phenology data
#     filter(Phenophase_Derived==2) %>% # only include flowering days
#     rename(state = Phenophase_Derived)
#
#   forcing <- read.csv(clim, header=TRUE, stringsAsFactors = FALSE) %>%
#       filter(forcing_type==forcingname) # ristos consider forcing units calculated based on work of Sarvas 1972
#
#   spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
#     select(SPU_Name, Orchard) # provenance information for each orchard in phen
#
#   phenbe <- dplyr::filter(phen, DoY == First_RF | DoY == Last_RF) %>%
#     dplyr::left_join(forcing) %>%
#     dplyr::left_join(spus) %>%
#     dplyr::mutate(Year = as.character(Year), Clone = as.character(Clone)) %>%
#     dplyr::rename(Provenance = SPU_Name) %>%
#     distinct()
#
#   return(phenbe)
# }

# select data for stan models - separate by sex and event. can keep day of year if you want, but dropped by default. factors is a vector string of factors you'll be passing to the model and keep_day is an option for if you want to keep Day of Year in the dataframe
# select_data <- function(phendat, censordat = NULL, factors, sex, event, keep_day = FALSE) {
#   # checks
#   assertthat::assert_that(sex %in% c("MALE", "FEMALE"), msg = "Sex should be MALE or FEMALE")
#   assertthat::assert_that(event %in% c("begin", "end"), msg = "event should be begin or end")
#
#   # filter for event and sex
#   phensub <- phendat %>%
#     dplyr::filter(if (event == "begin") Sex == sex & DoY == First_RF else Sex == sex & DoY == Last_RF)
#
#   if (is.null(censordat)) {
#     phensub <- phensub
#
#   } else {
#     factors <- c(factors, "censored")
#     censorship <- censordat %>%
#       dplyr::filter(Sex == sex) %>%
#       dplyr::filter(censored != 3) # this will need to be updated when I'm considering things that aren't just left censored
#
#     phensub <- dplyr::left_join(phensub, censorship)
#   }
#
#   if (keep_day == TRUE) {
#     phensub <- phensub %>%
#       dplyr::select(sum_forcing, DoY, all_of(factors))
#   } else {
#     phensub <- phensub %>%
#       dplyr::select(sum_forcing, all_of(factors))
#   }
#
#   return(phensub)
# }



# create an index to assign each level of a factor to be centered or non-centered based on a frequency threshold - levels that have more occurrences than the threshold are centered and those at or below are non-centered
# create_centering_index <- function(phensub, fac, threshold) {
#
#   # create an index for a factor for levels that should be modeled as centered or non-centered
#   ncp_idx <- which(table(phensub[[fac]]) <= threshold)
#   cp_idx <- which(table(phensub[[fac]]) > threshold)
#
#   # data for stan
#   k_ncp <- length(ncp_idx) # number of non centered sites
#   ncp_idx <- array(ncp_idx) # non-centered sites
#
#   k_cp <- length(cp_idx)
#   cp_idx <- array(cp_idx)
#
#   standat <- list(k_ncp, ncp_idx, k_cp, cp_idx)
#   names(standat) <- paste0(c("k_ncp_", "ncp_idx_", "k_cp_", "cp_idx_"), fac)
#
#   return(standat)
# }

# Build centering vs noncentering indexes for all factors that are partially decentered. Uses create_index. dat is a phenology dataset as a dataframe, factors are a list of factors from dat that should be centered or decentered based on a threshold, e.g. list(Site = 20, Provenance = 50). Thresholds are determined in the conceptualanalysis.
# build_factor_centering_indexes <- function(phensub, factor_threshold_list) {
#
#   assertthat::assert_that(is.list(factor_threshold_list), msg = "factor_thresholds must be a list with entries like factor name = threshold, e.g. list(Site = 20, Provenance = 50)")
#
#   nfac <- length(factor_threshold_list)
#
# #  phensub <- select_data(phendat, censorship, sex, event)
#   centering_indexes <- list()
#   for (i in 1:nfac) {
#     indexes <- create_centering_index(phensub = phensub, fac = names(factor_threshold_list)[i], threshold = factor_threshold_list[[i]])
#     centering_indexes <- append(centering_indexes, indexes)
#   }
#
#   assertthat::assert_that(length(centering_indexes) == 4*nfac, msg = "You should have 4 entries in the list for each factor, but something has gone wrong. Check build_factor_centering_indexes")
#
#   return(centering_indexes)
# }

# munge phenology data. Remove duplicate observations, add censoring information, combine with forcing data, and standardize sum_forcing. phendat should be flowers::lodgepole_phenology_event or structured similarly. clim is a dataframe of daily climate with Site + Year + DoY columns that can be matched against the ones in phendat. spu is a dataframe of information on spus with a provenance column SPU_Name and Orchard column that can be matched against the one in phendat
prepare_data <- function(phendat, clim, spu) {
  # 4 trees were observed by both Wagner and Walsh at PGTIS in 2006 - drop 1 copy of them (16 duplicate observations).
  rmidx <- phendat %>%
    group_by(Index) %>%
    summarize(count = n()) %>%
    filter(count > 4)

  spu <- select(spu, SPU_Name, Orchard) # drop unnecessary columns

  #add information about censoring
  phen <- phendat %>%
    filter(! (Index %in% rmidx$Index & Source == "Rita Wagner")) %>% # Drop duplicates
    add_censor_indicator() %>% # add censoring type
    mutate(censored_lronly = case_when(censored == "interval" ~ "none",
                                       censored %in% c("left", "right") ~ censored)) %>% # exclude interval censoring
    # add bound labels for interval censoring models
    mutate(bound = case_when(Event_Obs == 1 | Event_Obs == 2 & censored == "left" ~ "lower",
                             Event_Obs == 2 & censored == "interval" ~ "upper",
                             Event_Obs == 3 ~ "lower",
                             Event_Obs == 4 ~ "upper"))

  phenf <- phen %>%
    dplyr::left_join(clim) %>%
    dplyr::left_join(spu) %>%
    dplyr::mutate(Year = as.character(Year), Clone = as.character(Clone)) %>%
    dplyr::rename(Provenance = SPU_Name) %>%
    distinct() %>%

  return(phenf)
}

# filter phenology data by sex and event in order to build models for male and female, begin and end. sex is one of "FEMALE" or "MALE" event is one of "begin" or "end, and dat is output from `prepare_data` or similarly structured
filter_sex_event <- function(sex, event, dat = phenf) {
  assertthat::assert_that(sex %in% c("MALE", "FEMALE"))
  assertthat::assert_that(event %in% c("begin", "end"))

  if (event == "begin") {
    event_obs <- c(1,2)
  } else {
    event_obs <- c(3,4)
  }

  dat_prepped <- dat %>%
    dplyr::filter(Sex == sex & Event_Obs %in% event_obs) %>%
    #dplyr::select(-DoY, -Date, -State, -contains("Event"), -mean_temp, -forcing, -sum_forcing) %>%
    dplyr::select(-DoY, -Date, -State, -contains("Event"), -mean_temp, -forcing) %>%
   # tidyr::pivot_wider(names_from = bound, values_from = sum_forcing_centered, values_fill = 0) %>%
   tidyr::pivot_wider(names_from = bound, values_from = sum_forcing, values_fill = 0) %>%
    dplyr::rename(sum_forcing = lower) %>%
    dplyr::mutate(event = event)

  return(dat_prepped)
}

# fit an intercepts-only model to phenology data in brms. model accounts for both interval and end censoring and includes the effects of Site, Provenance, Clone, and Year.
fit_model <- function(dat, init_sigma = lapply(1:4, function(id) list(sigma = 30 ))) {

  fit <- brm(sum_forcing | cens(censored, upper) ~ 0 + Intercept + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = dat,
             prior = c(prior("normal(0,20)", class = "b"),
                       prior("normal(0,10)", class = "sigma"),
                       prior("normal(0,5)", class = "sd")),
             cores = 5, inits = init_sigma, iter = 3000, control = list(adapt_delta = 0.9),
             save_pars = save_pars(all = TRUE))

  return(fit)
}

# pulls together above functions - most useful for running individual models as || jobs.
model_phenology <- function(event, sex, inits = lapply(1:4, function(id) list(sigma = 30 )), phendat = phendat) {

  phenf <- prepare_data(phendat)

  dat <- filter_sex_event(sex, event, phenf)

  fit <- fit_model(dat=dat, init_sigma = inits )

  saveRDS(fit, paste0(sex, event, ".rds"))

  return(list(dat = dat, fit = fit))
}

# convenience function for gathering population mean draws from the model `mod`
gather_means_draws <- function(mod) {
  draws <- mod %>% tidybayes::gather_draws(b_Intercept, ndraws = nsamp, seed = seed)
  return(draws)
}

# convenience function for gathering sd (population and offset) draws from the model `mod`
gather_var_draws <- function(mod) {
  draws <- mod %>% gather_draws(`sd_.*`, `sigma`, regex = TRUE, ndraws = nsamp, seed = seed)
  return(draws)
}

# convenience function for gathering offset delta draws from the model `mod`
gather_offset_draws <- function(mod) {
  draws <- mod %>% gather_draws(`r_.*`, regex = TRUE, ndraws = nsamp, seed = seed)
  return(draws)
}

# simulate new data from the model for n_lct new levels of each factor of the model. n_lct is a numeric vector of length 3 c(number of new sites/provenances/years, number of clones per provenance, number of trees per clone) Draws for factor effects are from N(0, sigma_cluster). Using nsamples draws from the posterior.

simulate_from_model <- function(data, model, n_lct = c(5,10,2), nsamples = nsamp, seed = seed, cores = 6) {

  # retrodict "true"
  yrep <- add_predicted_draws(newdata = data, object = model, ndraws = nsamples, seed = seed, cores = cores, value = ".prediction") %>%
    mutate(prediction_type = "retrodiction - uncensored")

  # censor yrep based on censoring points in the raw data
  yrep_censored <- yrep %>%
    mutate(prediction_temp = case_when(censored == "interval" & .prediction < sum_forcing ~ sum_forcing,
                                       censored == "interval" & .prediction > upper ~ upper,
                                       censored == "interval" & (.prediction >= sum_forcing) & (.prediction <= upper) ~ .prediction,

                                       censored == "right" & .prediction >= sum_forcing ~ sum_forcing,
                                       censored == "right" & .prediction < sum_forcing ~ .prediction,

                                       censored == "left" & .prediction < sum_forcing ~ .prediction,
                                       censored == "left" & .prediction >= sum_forcing ~ sum_forcing),
           prediction_type = "retrodiction - censored") %>%
    select(-.prediction) %>%
    rename(.prediction = prediction_temp)


  # simulate data for fully crossed version of real dataset. This is 82,000+ observations and my computer can't handle that unless I use *very* few posterior samples

  crossdat <- data %>% tidyr::complete(Sex, event, Year, Site, tidyr::nesting(Provenance, Clone, Tree)) %>%
    tidyr::complete(Year, nesting(Site, Tree)) %>%
    select(Year, Site, Tree, Provenance, Clone) %>%
    distinct()

  ypred_fullcross <- tidybayes::add_predicted_draws(newdata = crossdat, object = model, ndraws = 30, seed = seed, cores = cores, value = ".prediction") %>%
    mutate(prediction_type = "prediction - full cross")

  # simulate data from new levels (out-of-sample predictions). The newdata simulation code took an embarrassingly long time to figure out
  nlevels <- n_lct[1] # how many sites, provenances, and years
  lv <- as.character(1:nlevels)
  nc <- n_lct[2] # clones per prov
  nt <- n_lct[3] # trees per clone

  Year <- data.frame(Year = lv)
  newdata <- data.frame(Site = rep(lv, nc*nt),
                        Provenance = rep(lv, nc*nt),
                        Clone = as.character(rep(1:(nlevels*nc), nt))) %>%
    tidyr::complete(Site, tidyr::nesting(Provenance, Clone)) %>%
    arrange(Site, Provenance, Clone) %>%
    mutate(Tree = as.character(1:n())) %>%
    merge(Year) %>%
    complete(Site, Provenance, Year)

  ypred_newlevels <- tidybayes::add_predicted_draws(newdata = newdata, object = model, allow_new_levels = TRUE, sample_new_levels = "gaussian", ndraws = nsamples, seed = seed, cores = cores, value = ".prediction") %>%
    mutate(prediction_type = "prediction - new levels")

  preds <- rbind(yrep, yrep_censored, ypred_fullcross, ypred_newlevels) %>%
    mutate(Sex = unique(data$Sex), event = unique(data$event))

  return(preds)
}


# prepare dataframes a and b for interval finding by splitting into lists and ensuring that the climate list (a) and the phenology list (b) contain information for the same sites and years. a and b must both have Site and Year columns
split_df_to_lists <- function(a, b) {
  lista <- split(a, f = list(a$Site, a$Year), drop = TRUE)
  listb <- split(b, f = list(b$Site, b$Year), drop = TRUE)

  # check that all sites and years in the phenology frame are in the climate frame
  assertthat::assert_that(all(names(listb) %in% names(lista))) # all entries in B must be in A

  # subset lista so it only contains Site x Year that also occur in B
  ainb <- lista[names(listb)]

  assertthat::are_equal(names(ainb), names(listb))

  return(list(listainb = ainb, listb = listb))
}

forcing_to_doy <- function(a, b, aforce, bforce, newdoycolname) {
  # prepare dataframes for interval finding by splitting into lists
  splitdfs <- split_df_to_lists(a, b)

  df <- purrr::map2(splitdfs$listainb, splitdfs$listb, find_day_of_forcing, aforce = aforce, bforce = bforce) %>% # find DoY in A corresponding to each sum_forcing in B
    purrr::map_dfr(bind_rows) # combine into a single dataframe

  names(df)[which(names(df) == "newdoycol")] <- newdoycolname

  return(df)
}

# given dataframes adf (climate) and bdf (phenology) identify the day of year corresponding to reaching each sum_forcing bcol in df. adf must have a sum_forcing column identified by name aforce and a DoY column and b must have a sum_forcing column identified with name bforce.
find_day_of_forcing <- function(adf, bdf, aforce, bforce) {

  # what row in a contains the interval for entries in b. Add 1 to the index because phenological events require the threshold to be reached. this introduces error, but is unavoidable in some direction.
  a_index <- findInterval(bdf[[bforce]], adf[[aforce]]) + 1


  # add a column to b for Day of Year and extract the correct Day of year from a using the index
  bdf$newdoycol <- adf$DoY[a_index]

  # when sum_forcing in b is exactly identical to sum_forcing in b, a_index will be +1 day. Re-write those
  identical_forcing_index <- which(bdf[[bforce]] %in% adf[[aforce]])
  bdf$newdoycol[identical_forcing_index] <- bdf$newdoycol[identical_forcing_index] - 1

  # (indirectly) test whether the correct day of year is being assigned to the correct forcing unit - for any site x year, sorting by sum_forcing_rep or newdoycol should produce the same ordering of newdoycol in bdf

  # order_by_sumforcing <- arrange(bdf, bforce, newdoycol)$newdoycol
  # order_by_newdoycol <- arrange(bdf, newdoycol)$newdoycol
  #
  # assertthat::are_equal(order_by_sumforcing, order_by_newdoycol)

  return(bdf)
}

# match forcing to doy for future climates and add identifying info about the future climates
match_force_future <- function(adf, bdf, aforce, bforce) {
  matched <- find_day_of_forcing(adf, bdf, aforce, bforce)

  idf <- select(adf, Year, Site, name, SSP, climate_forcing, normal_period) %>%
    distinct()

  df <- merge(matched, idf)

  return(df)
}
