# calculate f statistics
# 
library(dplyr)

# read in retrodictions
retro.fb <- read.csv("retrodictions/retrofb.csv", header=TRUE)

# filter out obs
obs <- dplyr::select(retro.fb, i, sum_forcing, DoY, Site, Year, Provenance, Clone) %>% 
    distinct()


# calculate f statistics for factors in a dataset
calc_f_obs <- function(df, sum_forcing, factor_id) {
  
  sum_forcing <- df[[sum_forcing]]
  factor_id <- df[[factor_id]]
  
  assertthat::are_equal(length(sum_forcing), length(factor_id))
  
  groupmeans <- tapply(sum_forcing, factor_id, mean)
  assertthat::assert_that(!any(is.na(groupmeans)), msg = "One of the group means is NA") # no means are NA
  
  groupsds <- tapply(sum_forcing, factor_id, sd)
  assertthat::assert_that(!any(is.na(groupsds)), msg = "One of the group sds is NA - check your sample sizes")
                          
  grandmean <- mean(sum_forcing)
  n_group <- tapply(sum_forcing, factor_id, length)
  k_group <- length(n_group)
  n_tot <- length(sum_forcing)
  
  between <- sum(n_group * (groupmeans - grandmean)^2) / (k_group - 1)
  within <- sum(groupsds^2 * (n_group - 1)) / (n_tot - k_group)
  
  fobs <- between/within
  return(fobs)
}

# calculate f statistic for factors for each iteration of a dataset within a model
calc_f_mod <- function(splitlist, factor_id, y) {
  purrr::map(splitlist, .f = calc_f_obs, sum_forcing = y, factor_id = factor_id) %>%
    dplyr::bind_cols() %>%
    tidyr::pivot_longer(cols = everything(), names_to = ".draw", values_to = factor_id)
}
####

# fstat forcing obs ########
fsite <- calc_f_obs(obs, "sum_forcing", "Site")
fprov <- calc_f_obs(obs, "sum_forcing", "Provenance")
fyear <- calc_f_obs(obs, "sum_forcing", "Year")
fclone <- calc_f_obs(obs, "sum_forcing", "Clone") #sd is infinite where clone only observed once and fstat cannot be calculated for this factor

fobs <- list(Site = fsite, Provenance = fprov, Year = fyear, Clone = fclone)
fobs


# fstat forcing model #########

# split retrodictions df into a list by draws - fstatistic should be calculated for the full "dataset" at each draw of the model
retrosplit <- retro.fb %>%
  split(.$.draw) 

fmods <- list()
fmods$Site <- calc_f_mod(retrosplit, "Site", y = "sum_forcing_rep")
fmods$Provenance <- calc_f_mod(retrosplit, "Provenance", y = "sum_forcing_rep")
fmods$Year <- calc_f_mod(retrosplit, "Year", y = "sum_forcing_rep")

fmods <- cbind(fmods$Site, Provenance = fmods$Provenance$Provenance, Year = fmods$Year$Year)

ggplot(fmods, aes(x = Site)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = fobs$Site)

ggplot(fmods, aes(x = Provenance)) + 
  geom_histogram(bins = 50) +
  geom_vline(xintercept = fobs$Provenance)

ggplot(fmods, aes(x = Year)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = fobs$Year)

# fstat day obs ########
fsite <- calc_f_obs(obs, "DoY", "Site")
fprov <- calc_f_obs(obs, "DoY", "Provenance")
fyear <- calc_f_obs(obs, "DoY", "Year")
fclone <- calc_f_obs(obs, "DoY", "Clone") #sd is infinite where clone only observed once and fstat cannot be calculated for this factor

fobs <- list(Site = fsite, Provenance = fprov, Year = fyear, Clone = fclone)
fobs

# fstat day mod ##########
fmods <- list()
fmods$Site <- calc_f_mod(retrosplit, "Site", y = "doy_rep")
fmods$Provenance <- calc_f_mod(retrosplit, "Provenance", y="doy_rep")
fmods$Year <- calc_f_mod(retrosplit, "Year", y = "doy_rep")

fmods <- cbind(fmods$Site, Provenance = fmods$Provenance$Provenance, Year = fmods$Year$Year)

ggplot(fmods, aes(x = Site)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = fobs$Site)

ggplot(fmods, aes(x = Provenance)) + 
  geom_histogram(bins = 50) +
  geom_vline(xintercept = fobs$Provenance)

ggplot(fmods, aes(x = Year)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = fobs$Year)
