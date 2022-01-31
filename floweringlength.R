# calculate length of flowering period
#
# depends

library(dplyr)
library(tidyr)


# observations
phenf <- readRDS("objects/phenf.rds")
alldat <- readRDS("objects/alldat.rds")

# calculate the length of the flowering period for each clone

length_dat <- phenf %>%
  filter(Event_Obs %in% c(2,3)) %>%
  mutate(event = case_when(Event_Obs == 2 ~ "begin",
                           Event_Obs == 3 ~ "end")) %>%
  select(-contains("censored"), -Source, -X, -Y, -bound, -mean_temp, -contains("forcing"), -Date, -contains("Event_"), -State) %>%
  group_by(Index, Year, Sex, Site, Orchard, Clone, Tree) %>%
  pivot_wider(names_from = event, values_from = DoY) %>%
  mutate(length = end - begin)

## using data that's only interval censored


## using last day observed not flowering and first day observed past flowering instead of flowering period

length_dat_long <- phenf %>%
  filter(Event_Obs %in% c(1,4)) %>%
  mutate(event = case_when(Event_Obs == 1 ~ "begin",
                           Event_Obs == 4 ~ "end")) %>%
  select(-contains("censored"), -Source, -X, -Y, -bound, -mean_temp, -contains("forcing"), -Date, -contains("Event_"), -State) %>%
  group_by(Index, Year, Sex, Site, Orchard, Clone, Tree) %>%
  pivot_wider(names_from = event, values_from = DoY) %>%
  mutate(length = end - begin)

# for the retrodictions, use censored simulation data

censor_doy_retro <- readRDS("objects/censor_doy_retro.rds")

meanstartend_censored <- censor_doy_retro %>%
  group_by(Clone, Site, Year, Sex, event) %>%
  summarise(meandoy = mean(newdoycol), sddoy = sd(newdoycol))

length_censored <- meanstartend_censored %>%
  pivot_wider(names_from = event, values_from = c("meandoy", "sddoy")) %>%
  mutate(length_mean = meandoy_end - meandoy_begin, length_sd = sqrt(sddoy_begin^2 + sddoy_end^2))

# compare retrodicted length and real observed length

length_comp <- left_join(length_dat, length_censored)

library(ggplot2)
ggplot(length_comp, aes(x = length_mean, y = length)) +
  geom_point() +
  facet_wrap("Sex") +
  geom_abline(slope = 1, intercept = 0)


# model strongly biased to longer flowering length than observed.


## compare retrodicted length using the full possible flowering period

length_comp_full <- left_join(length_dat_long, length_censored)

ggplot(length_comp_full, aes(x = length_mean, y = length)) +
  geom_point() +
  facet_wrap("Sex") +
  geom_abline(slope = 1, intercept = 0)

# YES! Now the length estimates have a bias to be too short. Model is doing great!

# even more obvious bias. maybe that's the expectation though? the type of censoring in my data *always* underestimates length. though these graphs suggest more extreme censoring than I'd expect for interval censoring fo just a day or two

specific_doy_preds <- readRDS("objects/specific_doy_preds.rds")
doypredmatchfut_medians <- readRDS("objects/doypredmatchfut_medians.rds")
factororder <- readRDS("objects/factororder.rds")

# historical ####
#individual lengths
specific_doy_preds_length <- specific_doy_preds %>%
  ungroup() %>%
  select(-.prediction, -sum_forcing, -upper, -contains("censored")) %>%
  pivot_wider(names_from = event, values_from = newdoycol) %>%
  mutate(length = end - begin)
saveRDS(specific_doy_preds_length, file = "objects/specific_doy_preds_length.rds")

# population lengths
specific_doy_preds_length_ts <- specific_doy_preds %>%
  group_by(Sex, event, Site, Year, prediction_type) %>%
  median_hdci(newdoycol, .width = c(0.5, 0.89)) %>%
  # drop wider uncertainty for now
  filter(.width == 0.5) %>%
  select(-starts_with(".")) %>% distinct() %>%
  pivot_wider(names_from = event, values_from = newdoycol) %>%
  mutate(length = end - begin) %>%
  ungroup() %>%
  mutate(Site = forcats::fct_relevel(as.factor(Site), factororder$site))
saveRDS(specific_doy_preds_length_ts, file = "objects/specific_doy_preds_length_ts.rds")

# length of future phenological periods

futlen <- doypredmatchfut_medians %>%
  select(-contains("Date"), -contains(".lower"), -contains(".upper"), -.width) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = event, values_from = newdoycol) %>%
  mutate(period_length = end - begin)
saveRDS(futlen, file = "objects/futlen.rds")


