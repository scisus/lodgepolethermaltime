# functions
# 

# format data for model - only start and end dates
filter_start_end <- function() {
  
  phen <-  flowers::phenology %>% # phenology data
    filter(Phenophase_Derived==2) %>% # only include flowering days
    rename(state = Phenophase_Derived) 

  forcing <- read.csv("data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
      filter(forcing_type=="ristos") # only consider forcing units calculated based on work of Sarvas 1972

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

# select data for stan models - separate by sex and event
select_data <- function(phendat, sex, event) {
  phensub <- phendat %>%
    dplyr::filter(if (event == "begin") Sex == sex & DoY == First_RF else Sex == sex & DoY == Last_RF) %>%
    dplyr::select(sum_forcing, Site, Year, Provenance, Clone) 
  
  return(phensub)
}


# set thresholds
# - Site threshold: 250
# - Provenance threshold: 150

build_centering_index <- function(phensub, fac, threshold) {
  
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


# Fit a model in Stan to phenology data, return the model fit object and save the model fit object to a file. Choose whether the model is for "MALE" or "FEMALE" strobili and whether the event is the "begin" or "end" of flowering. data is a dataframe of flowering data. id is an optional identifier appended to the file name.
fit_model <- function(phendat, sex, event, model = "phenology.stan", maxtreedepth=10) {
  
  
  phensub <- select_data(phendat, sex, event)
  #centering_indexes <- build_centering_indexes(phensub) 
  # factor levels are very unbalanced, so I'm non-centering some levels
  
  siteidx <- build_centering_index(phensub, "Site", 250)
  providx <- build_centering_index(phensub, "Provenance", 150)
  yearidx <- build_centering_index(phensub, "Year", 150)
 # cloneidx <- build_centering_index(phensub, "Clone", 10)
  
  centering_indexes <- append(siteidx, providx) %>%
    append(yearidx)
  
  # prepare data for stan
  
  base_data <- tidybayes::compose_data(phensub, .n_name=tidybayes::n_prefix(prefix="k"))

  input <- append(base_data, centering_indexes)
  
  # add event-specific prior
  if (event == "begin") {
    input <- c(input, mu_mean=335, mu_sigma = 50)
  } 
  
  if (event == "end") {
    input <- c(input, mu_mean=555, mu_sigma = 90)
  } 
  
  
  fit <- rstan::stan(file= model, chains=6, data=input, iter=3500, cores=7,
                     pars=c("alpha_ncp_site", "alpha_cp_site", "alpha_ncp_prov", "alpha_cp_prov", "z_alpha_clone"), include=FALSE,
                     init = rep(list(list(mu = abs(rnorm(1,100,50)), # stop stan from sampling impossible negative numbers
                                          sigma = rexp(1,1),
                                          sigma_site = rexp(1,1),
                                          sigma_year = rexp(1,1),
                                          sigma_prov = rexp(1,1),
                                          sigma_clone = rexp(1,1))), 6),
                     control = list(max_treedepth = maxtreedepth, adapt_delta=0.8))

  # fit <- rstan::stan(file= model, chains=6, data=input, cores=7, 
  #                    pars=c("alpha_ncp_site", "alpha_cp_site", "alpha_ncp_prov", "alpha_cp_prov", "z_alpha_clone"), include=FALSE, 
  #                    control = list(max_treedepth = maxtreedepth))
  gc()
  
  saveRDS(fit, file = paste(Sys.Date(), sex, "_", event, ".rds", sep=''))
  
  return(fit)
}