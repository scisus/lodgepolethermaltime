# adjust predictions for thermal time model based on results from provenance model

library(tidybayes)
library(dplyr)
library(purrr)

clonemodells <- readRDS("objects/clonemodells.rds")
siteMAT <- read.csv("../lodgepole_climate/data/climateBC/climatebc_locs_Normal_1961_1990Y.csv") %>%
  filter(id == "site") %>%
  select(Site, MAT) %>%
  mutate(sdoffset = 0)



labdf <- data.frame(Sex = c("FEMALE", "FEMALE", "MALE", "MALE"), event = c('begin', 'end', 'begin', 'end'), model = c('fb', 'fe', 'mb', 'me'))

siteMATl <- merge(siteMAT, labdf) %>%
  split(f = list(.$model))

cepred <- purrr::map2(siteMATl, clonemodells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event, MAT, sdoffset, Site) %>% distinct(), object = y, re_formula = NA)}) %>%
  bind_rows()


# vars <- purrr::map(clonemodells, function(x) {spread_draws(x, b_Intercept, b_MAT, sigma)}) %>%
#   bind_rows(.id = "model") %>%
#   left_join(labdf) # label the models for plotting


library(ggplot2)

ggplot(cepred, aes(x = .epred, y = Site)) +
  stat_pointinterval() +
  facet_grid(Sex ~ event)
