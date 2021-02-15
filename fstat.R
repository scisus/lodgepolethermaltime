# calculate f statistics for data and model
# 
library(dplyr)

# functions ########
# calculate f statistics for factors in a dataset - requires observations ("y") and a factor ("factor_id") that are columns in a dataframe "df"
calc_f_stat <- function(df, y, factor_id) {
  
  y <- df[[y]]
  factor_id <- df[[factor_id]]
  
  assertthat::are_equal(length(y), length(factor_id))
  
  groupmeans <- tapply(y, factor_id, mean)
  assertthat::assert_that(!any(is.na(groupmeans)), msg = "One of the group means is NA - something is wrong")
  
  groupsds <- tapply(y, factor_id, sd)
  if (any(is.na(groupsds))) { print("One or more of the group sds is NA - check your sample sizes") } # no means are NA
  
  grandmean <- mean(y)
  n_group <- tapply(y, factor_id, length)
  k_group <- length(n_group)
  n_tot <- length(y)
  
  between <- sum(n_group * (groupmeans - grandmean)^2) / (k_group - 1)
  within <- sum(groupsds^2 * (n_group - 1)) / (n_tot - k_group)
  
  fstat <- between/within
  return(fstat)
}

# calculate site, provenance, and year f_statistic for a Site, Provenance, Year, and Clone factors in dataset (df) with observations (y)
calc_f_facs <- function(df, y) {
  fsite <- calc_f_stat(df, y, "Site")
  fprov <- calc_f_stat(df, y, "Provenance")
  fyear <- calc_f_stat(df, y, "Year")
  # fclone <- calc_f_stat(df, y, "Clone") #sd is infinite where clone only observed once and fstat cannot be calculated for this factor
  
  fstat <- data.frame(Site = fsite, Provenance = fprov, Year = fyear)
  return(fstat)
}

# MCMC draws from stan produce estimates for the entire dataset at each iteration. For each draw of a dataset within a model, calculate the fstatistic factors Site, Provenance, and Year

calc_f_draws <- function(splitlist, y) {
  
  fstat_frame <-  splitlist %>% 
    purrr::map(.f = calc_f_facs, y = y) %>%
    dplyr::bind_rows(.id = ".draw")
  
  return(fstat_frame)
}


# data #########

# read in retrodictions
retro.fb <- read.csv("retrodictions/retrofb.csv", header=TRUE)

# filter for observations only
obs <- dplyr::select(retro.fb, i, sum_forcing, DoY, Site, Year, Provenance, Clone) %>% 
    distinct()

facs <- c("Site", "Provenance", "Year", "Clone")


# fstats for observations ########
# 

fstat_obs_forcing <- calc_f_facs(obs, "sum_forcing")
fstat_obs_day <- calc_f_facs(obs, "DoY")

fstat_obs <- rbind(fstat_obs_forcing, fstat_obs_day)
fstat_obs$y <- c("Sum Forcing", "Day of Year")

fstat_obs <- tidyr::pivot_longer(fstat_obs, cols = any_of(facs), names_to = "factors", values_to = "F_statistic") 


# fstats for models #########
# 
# split retrodictions df into a list by draws - fstatistic should be calculated for the full "dataset" at each draw of the model
retrosplit <- retro.fb %>% #this object is needed only temporarily
  split(.$.draw) 

fstat_mod_forcing <- calc_f_draws(retrosplit, "sum_forcing_rep")

fstat_mod_doy <- calc_f_draws(retrosplit, "doy_rep")

# figures ########

fstat_forcing_long <- fstat_mod_forcing %>%
  tidyr::pivot_longer(cols = any_of(facs), names_to = "factors", values_to = "F_statistic") 
fstat_forcing_long$y <- "Sum Forcing"

fstat_doy_long <- fstat_mod_doy %>%
  tidyr::pivot_longer(cols = any_of(facs), names_to = "factors", values_to = "F_statistic") 
fstat_doy_long$y <- "Day of Year"

fstat_mod <- rbind(fstat_forcing_long, fstat_doy_long)

ggplot(fstat_mod, aes(x= F_statistic)) +
  geom_histogram(bins = 50) +
  facet_grid(factors ~ y, scales = "free_x") +
  geom_vline(data = fstat_obs, aes(xintercept = F_statistic))

