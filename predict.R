# predict flowering events

# libraries
library(purrr)
library(dplyr)

# data

modells <- readRDS("objects/modells.rds")
alldatls <- readRDS("objects/datlist.rds")


# predict the global grand means: average predicted outcome ignoring group-specific deviations in intercept or slope

# grand mean ####
# build a dataframe with one entry for each dataset - just Sex, event, and Generation no groups, and calculate the average predicted outcome ignoring group specific deviations and individual level variation.
fepred <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event, Generation) %>% distinct(), object = y, re_formula = NA)}) %>%
  bind_rows()
saveRDS(fepred, file = "objects/fepred.rds")


# conditional effects, new group random ####
# average predicted outcome for a new group based on random draws from the model (sample new levels from the (multivariate) normal distribution implied by the group-level standard deviations and correlations. )
# build a dataframe with new factors

newfactors <- expand_grid(Site = c("SiteA", "SiteB", "SiteC"), Year = c("Year1", "Year2", "Year3"), Clone = c("Clone1", "Clone2", "Clone3")) #leaving out tree for now
fepred_cenew <- purrr::map2(alldatls, modells, function(x,y) {
  add_predicted_draws(newdata = select(x, Sex, event, Generation) %>% distinct %>% merge(newfactors),
                  object = y,
                 re_formula = ~ 1 + (1|Site) + (1|Clone) + (1|Year) + mo(Generation),
  allow_new_levels = TRUE, sample_new_levels = "gaussian")}) %>%
  bind_rows()

fepred_cenew %>% group_by(Site) %>% summarise(mean = mean(.prediction), sd = sd(.prediction))
fepred_cenew %>% group_by(Year) %>% summarise(mean = mean(.prediction), sd = sd(.prediction))
fepred_cenew %>% group_by(Clone) %>% summarise(mean = mean(.prediction), sd = sd(.prediction))



ggplot(fepred_cenew,
       aes(x = .prediction, y = Site, fill = Sex)) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_okabe_ito() +
  labs(title = "new levels",
       x = "Predicted forcing", y = "Sex",
       subtitle = "Posterior predictions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_grid(event ~ Generation)

# conditional effects, existing groups ####

fepred_ceold <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event, Generation, Site, Year, Clone, Tree) %>% distinct(), object = y, re_formula = NULL, ndraws = 200)}) %>%
  bind_rows()

ggplot(fepred_ceold,
       aes(x = .epred, y = Site, fill = Sex)) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_okabe_ito() +
  labs(title = "old levels",
       x = "Predicted forcing", y = "Sex",
       subtitle = "Posterior expectation") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_grid(event ~ Generation)
