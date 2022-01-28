# calculate length of flowering period
#
# depends

library(dplyr)
library(tidyr)


# observations
phenf <- readRDS("objects/phenf.rds")
alldat <- readRDS("objects/alldat.rds")

length_dat <- phenf %>%
  filter(Event_Obs %in% c(2,3)) %>%
  mutate(event = case_when( )) # start here and change event label to events
  pivot_wider(names_from = "event", values_from = c("sum_"))

# calculate the length of the flowering period for each clone

# for the retrodictions, use censored simulation data

censor_doy_retro <- readRDS("objects/censor_doy_retro.rds")

meanstartend_censored <- censor_doy_retro %>%
  group_by(Clone, Site, Year, Sex, event) %>%
  summarise(meandoy = mean(newdoycol), sddoy = sd(newdoycol))

length_censored <- meanstartend_censored %>%
  pivot_wider(names_from = event, values_from = c("meandoy", "sddoy")) %>%
  mutate(mean_length = meandoy_end - meandoy_begin, length_sd = sqrt(sddoy_begin^2 + sddoy_end^2))



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


