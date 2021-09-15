# calculate length of flowering period
#
# depends

library(dplyr)
library(tidyr)

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


