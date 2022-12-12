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

# pull in predictions & calculate mean and variance of forcing
fepred_summary <- readRDS("objects/fepred.rds") %>%
  group_by(Sex, event) %>%
  summarise(mean_forcing = mean(.epred), variance_forcing = var(.epred))

# calculate mean and variance for adjustment at each site
cepred_summary <- cepred %>%
  group_by(Sex, event, MAT, Site) %>%
  summarise(mean = mean(.epred), variance = var(.epred))

# adjust forcing based on clinal model
clinal_forcing_adjustment <- full_join(fepred_summary, cepred_summary) %>%
  mutate(adjusted_forcing_mean = mean_forcing + mean, adjusted_forcing_variance = variance_forcing + variance, sd = sqrt(adjusted_forcing_variance),
         diff = mean_forcing - adjusted_forcing_mean)
saveRDS(clinal_forcing_adjustment, "objects/clinal_forcing_adjustment.rds")

ggplot(clinal_forcing_adjustment, aes(x = MAT, y = adjusted_forcing_mean, color = "adjusted")) +
  geom_line() +
  geom_point(aes(x= MAT, y = mean_forcing, color = "original")) +
  facet_grid(Sex ~ event)

forcing_on_flowering_day <- readRDS("objects/phenf.rds") %>%
  filter(Event_Obs %in% c(2,3)) %>%
  left_join(select(cepred_summary, Site, MAT) %>% distinct())

ggplot(clinal_forcing_adjustment, aes(x = MAT, y = diff, color = Sex, shape = event)) +
  geom_line() +
  geom_jitter(data = forcing_on_flowering_day, aes(x = MAT, y = forcing)) +
  ggtitle("difference between predictions with and without cline", subtitle = "points actual forcing day of event")



