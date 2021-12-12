# provenance model analysis

library(dplyr)
library(brms)
library(purrr)
library(tidybayes)
library(shinystan)
library(ggplot2)
library(sjPlot)

launch_shinystan(clonemodells$fb)
clonemodells <- list(fb = readRDS("female_begin_clone.rds"),
                     mb = readRDS("male_begin_clone.rds"),
                     fe = readRDS("female_end_clone.rds"),
                     me = readRDS("male_end_clone.rds"))

labdf <- data.frame(Sex = c("FEMALE", "FEMALE", "MALE", "MALE"), event = c('begin', 'end', 'begin', 'end'), model = c('fb', 'fe', 'mb', 'me'))


vars <- purrr::map(clonemodells, function(x) {gather_draws(x, b_Intercept, b_MAT, sigma)}) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) # label the models for plotting

sumvars <- vars %>%
  group_by(.variable, Sex, event) %>%
  median_hdci(.value)

tab_model(clonemodells) #next: figure out how to save and then relabel
mbtab <- tab_model(clonemodells$mb, title = "Male begin")



ggplot(beta, aes(x = .value)) +
  stat_slabinterval()

  vcov(fbfitclone)
summary(fbfitclone)
as_draws_df(fbfitclone) %>%
  select(-lp__, -starts_with(".")) %>%
  cov() %>%
  round(digits = 2)
pairs(fbfitclone)
