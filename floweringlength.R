# calculate length of flowering period
#
# depends

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# The exact flowering period is never observed because of censoring. Using the first and last observed flowering days we construct a minimum flowering period length for the data. Using the last observed before flowering day and the first observed after flowering day, we construct a maximum flowering period length for the data. We expect model estimates to be between the min and max ranges for the observations.

# observations
phenf <- readRDS("objects/phenf.rds")
alldat <- readRDS("objects/alldat.rds")

# calculate the minimum length of the flowering period for each clone

length_dat_min <- phenf %>%
  filter(Event_Obs %in% c(2,3)) %>%
  mutate(event = case_when(Event_Obs == 2 ~ "begin",
                           Event_Obs == 3 ~ "end")) %>%
  select(-contains("censored"), -Source, -X, -Y, -bound, -mean_temp, -contains("forcing"), -Date, -contains("Event_"), -State) %>%
  group_by(Index, Year, Sex, Site, Orchard, Clone, Tree) %>%
  pivot_wider(names_from = event, values_from = DoY) %>%
  mutate(length_min = end - begin)


## calculate the maximum length of the flowering period for each clone using last day observed not flowering and first day observed past flowering instead of flowering period

length_dat_max <- phenf %>%
  filter(Event_Obs %in% c(1,4)) %>%
  mutate(event = case_when(Event_Obs == 1 ~ "begin",
                           Event_Obs == 4 ~ "end")) %>%
  select(-contains("censored"), -Source, -X, -Y, -bound, -mean_temp, -contains("forcing"), -Date, -contains("Event_"), -State) %>%
  group_by(Index, Year, Sex, Site, Orchard, Clone, Tree) %>%
  pivot_wider(names_from = event, values_from = DoY) %>%
  mutate(length_max = end - begin)

# try using noncensored retrodictions

retro_doy <- readRDS("objects/specific_doy_preds.rds") %>% filter(prediction_type == "retrodiction - uncensored") # slow step, consider object that's only the uncensored obs

meanstartend_retro <- retro_doy %>%
  group_by(Tree, Clone, Site, Year, Sex, event) %>%
  summarise(meandoy = mean(newdoycol), sddoy = sd(newdoycol))

length_retro <- meanstartend_retro %>%
  pivot_wider(names_from = event, values_from = c("meandoy", "sddoy")) %>%
  mutate(length_mean = meandoy_end - meandoy_begin, length_sd = sqrt(sddoy_begin^2 + sddoy_end^2))

# compare retrodicted length and min/max observed flowering

length_comp <- full_join(select(length_dat_min, -begin, -end), select(length_dat_max, -begin, -end) ) %>%
  left_join(length_retro) %>%
  mutate(estimate_in_interval = case_when(!is.na(length_min) & !is.na(length_max) ~ length_mean >= length_min & length_mean <= length_max,
                                          is.na(length_max) ~ length_mean > length_min,
                                          is.na(length_min) ~ length_mean < length_max)) %>%
  mutate(Clone = fct_reorder(Clone, length_mean))
# how many trees have length out of expected interval?
length(which(length_comp$estimate_in_interval))/nrow(length_comp) # The mean estimate of clone flowering period length is within the expected interval 90.4 % of the time.

ggplot(length_comp, aes(x = Clone, y = length_mean, colour = estimate_in_interval)) +
  geom_point(alpha = 0.5) +
  scale_colour_viridis_d(option = "cividis") +
  theme_dark() +
  facet_grid(Sex ~ Site, scales = "free_x")


#


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


