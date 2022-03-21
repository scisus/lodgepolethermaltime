# predict flowering events

# libraries

library(ggokabeito)   # Neat accessible color palette
library(ggthemes)    # Nice themes

# data

modells <- readRDS("objects/modells.rds")
alldatls <- readRDS("objects/datlist.rds")


# predict the global grand means: average predicted outcome ignoring group-specific deviations in intercept or slope

# grand mean ####
# build a dataframe with one entry for each dataset - just Sex and event, no groups, and calculate the average predicted outcome ignoring group specific deviations and individual level variation. Theoretically you could have as many entries as you'd like in each dataset but in this case it is redundant (all entries produce the same distribution) and seems to run afoul of a bug in the plotting where the distribution becomes very lumpy. anyway, keeps the resulting dataframe small enough for easy plotting while letting me include all the draws.
fepred <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event) %>% distinct(), object = y, re_formula = NA)}) %>%
  bind_rows()

ggplot(fepred,
       aes(x = .epred, y = Sex, fill = event)) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_okabe_ito() +
  labs(title = "Grand mean",
       x = "Predicted forcing", y = "Sex",
       subtitle = "Posterior predictions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_clean() +
  theme(legend.position = "bottom")

# conditional effects, new group random ####
# average predicted outcome for a new group based on random draws from the model (sample new levels from the (multivariate) normal distribution implied by the group-level standard deviations and correlations. )
# build a dataframe with new factors

newfactors <- expand_grid(Site = c("SiteA", "SiteB", "SiteC"), Year = c("Year1", "Year2", "Year3"), Clone = c("Clone1", "Clone2", "Clone3")) #leaving out tree for now
fepred_cenew <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event) %>% merge(newfactors), object = y, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "gaussian")}) %>%
  bind_rows()

ggplot(fepred_cenew,
       aes(x = .epred, y = Sex, fill = event)) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_okabe_ito() +
  labs(title = "new levels",
       x = "Predicted forcing", y = "Sex",
       subtitle = "Posterior predictions") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_clean() +
  theme(legend.position = "bottom") +
  facet_grid(Site ~ Year)
