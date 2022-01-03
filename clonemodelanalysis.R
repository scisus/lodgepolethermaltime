# provenance model analysis

library(dplyr)
library(brms)
library(purrr)
library(tidybayes)
library(shinystan)
library(ggplot2)



clonemodells <- list(fb = readRDS("female_begin_clone.rds"),
                     mb = readRDS("male_begin_clone.rds"),
                     fe = readRDS("female_end_clone.rds"),
                     me = readRDS("male_end_clone.rds"))
saveRDS(clonemodells, "objects/clonemodells.rds")
clonedat <- readRDS("objects/clonedat.rds") %>%
  split(f = list(.$Sex, .$event))

labdf <- data.frame(Sex = c("FEMALE", "FEMALE", "MALE", "MALE"), event = c('begin', 'end', 'begin', 'end'), model = c('fb', 'fe', 'mb', 'me'))


vars <- purrr::map(clonemodells, function(x) {gather_draws(x, b_Intercept, b_MAT, sigma)}) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) # label the models for plotting

# add draws from the expectation of the posterior predictive distribution (expected value/mean of the posterior). only incorporates uncertainty in the mean while ignoring residual error

clonepred <- purrr::map2(clonedat, clonemodells, function(x,y) {add_predicted_draws(x, y)}) %>%
  bind_rows() %>%
  select(-.chain, -.iteration) %>%
  filter(!is.na(MAT)) %>%
  ungroup()
saveRDS(clonepred, "objects/clonepred.rds")



