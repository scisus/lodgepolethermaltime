# functions
# 

# format data for model - only start and end dates
filter_start_end <- function() {
  
  phen <- flowers::phenology %>% # phenology data
      filter(Phenophase_Derived==2 & Year < 2012) %>% # need to remake all_clim_PCIC.csv to include 2012
      rename(state = Phenophase_Derived)# %>% # only consider days when trees are flowering
      # dplyr::left_join(drop) %>% # drop years and locations where observations were only made on 1 day
      # filter(is.na(keep)) %>%
      # select(-keep)

  forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
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


# Fit a model in Stan to phenology data, return the model fit object and save the model fit object to a file. Choose whether the model is for "MALE" or "FEMALE" strobili and whether the event is the "begin" or "end" of flowering. data is a dataframe of flowering data. id is an optional identifier appended to the file name.
fit_model <- function(phendat, sex, event, model = "phenology.stan", maxtreedepth=10) {
  
  
  phensub <- select_data(phendat, sex, event)
  
  # prepare data for stan
  
  input <- tidybayes::compose_data(phensub)
  
  # add event-specific prior
  if (event == "begin") {
    input <- c(input, mu_mean=335, mu_sigma = 50)
  } 
  
  if (event == "end") {
    input <- c(input, mu_mean=555, mu_sigma = 90)
  } 
  
  
  fit <- rstan::stan(file= model, chains=8, data=input, iter=3500, cores=9, 
                     pars=c("z_alpha_clone", "z_alpha_ramet"), include=FALSE, 
                     init = rep(list(list(mu = abs(rnorm(1,100,50)), 
                                          sigma = rexp(1,1), 
                                          sigma_site = rexp(1,1), 
                                          sigma_year = rexp(1,1), 
                                          sigma_prov = rexp(1,1), 
                                          sigma_clone = rexp(1,1))), 8),
                     control = list(max_treedepth = maxtreedepth, adapt_delta=0.9))
  
  gc()
  
  saveRDS(fit, file = paste(Sys.Date(), sex, "_", event, ".rds", sep=''))
  
  return(fit)
}