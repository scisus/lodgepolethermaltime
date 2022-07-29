# predict flowering events
# posterior predictive includes individual observation uncertainty
# grand means ignore group specific effects
# conditional effects include group specific effects as well as the uncertainty of fixed coefficients and the uncertainty of variance parameters for groups

# libraries
library(purrr)
library(dplyr)
library(tidybayes)

# data

modells <- readRDS("objects/modells.rds")
alldatls <- readRDS("objects/datlist.rds")

n <- 1000 # when downsampling required


# predict the global grand means: average predicted outcome ignoring group-specific deviations in intercept or slope

# grand mean ####
# build a dataframe with one entry for each dataset - just Sex, event, and Generation no groups, and calculate the average predicted outcome ignoring group specific deviations and individual level variation.

## expectation ####
fepred <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event) %>% distinct(), object = y, re_formula = NA)}) %>%
  bind_rows()
saveRDS(fepred, file = "objects/fepred.rds")

fepred %>%
  group_by(Sex, event) %>%
  tidybayes::median_hdci(.epred)


## posterior predictive ####
fpred <- purrr::map2(alldatls, modells, function(x,y) {
  add_predicted_draws(newdata = select(x, Sex, event) %>% distinct(), object = y, re_formula = NA)}) %>%
    bind_rows()
saveRDS(fpred, file = "objects/fpred.rds")

# conditional effects, new group ####
# average predicted outcome for a new group based on random draws from the model (sample new levels from the (multivariate) normal distribution implied by the group-level standard deviations and correlations.). (That is, sampling for the new group from the "prior" estimated by the model)

newfactors <- data.frame(Site = "new_Site", Year = "new_Year", Clone = "new_Clone", Tree = "new_Tree")

## expectation ####
fepred_cenew <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event) %>% distinct %>% merge(newfactors),
                  object = y,
                 re_formula = NULL,
  allow_new_levels = TRUE, sample_new_levels = "gaussian")}) %>%
  bind_rows()
saveRDS(fepred_cenew, file = "objects/fepred_cenew.rds")

## posterior prediction ####
fpred_cenew <- purrr::map2(alldatls, modells, function(x,y) {
  add_predicted_draws(newdata = select(x, Sex, event) %>% distinct %>% merge(newfactors),
                  object = y,
                  re_formula = NULL,
                  allow_new_levels = TRUE, sample_new_levels = "gaussian")}) %>%
  bind_rows()
saveRDS(fpred_cenew, file = "objects/fpred_cenew.rds")

# conditional effects, existing groups ####

# average predicted outcomes for existing groups incorporating group specific deviations in intercept/slope

## expectation ####
fepred_ceold <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event, Site, Year, Clone, Tree) %>% distinct(), object = y, re_formula = NULL, ndraws = n)}) %>%
  bind_rows()
saveRDS(fepred_ceold, file = "objects/fepred_ceold.rds")

## posterior prediction ####
fpred_ceold <- purrr::map2(alldatls, modells, function(x,y) {
  add_predicted_draws(newdata = select(x, Sex, event, Site, Year, Clone, Tree) %>% distinct(), object = y, re_formula = NULL, ndraws = n)}) %>%
  bind_rows()
saveRDS(fpred_ceold, file = "objects/fpred_ceold.rds")

# predictions doy ####

