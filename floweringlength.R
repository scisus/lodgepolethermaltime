# calculate length of flowering period
#
# depends

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# observations

alldat <- readRDS("objects/alldat.rds")


## calculate the maximum length of the flowering period for each genotype using last day observed not flowering and first day observed past flowering instead of flowering period



# try using noncensored retrodictions

retro_doy_summary <- readRDS("objects/retro_doy_summary.rds")

# compare retrodicted length and min/max observed flowering

length_comp <- full_join(select(length_dat_min, -begin, -end), select(length_dat_max, -begin, -end) ) %>%
  left_join(retro_doy_summary) %>%
  mutate(estimate_in_interval = case_when(!is.na(length_min) & !is.na(length_max) ~ length_mean >= length_min & length_mean <= length_max,
                                          is.na(length_max) ~ length_mean > length_min,
                                          is.na(length_min) ~ length_mean < length_max)) %>%
  mutate(Genotype = fct_reorder(Genotype, length_mean))
# how many trees have length out of expected interval?
length(which(length_comp$estimate_in_interval))/nrow(length_comp) # The mean estimate of genotype flowering period length is within the expected interval 90.4 % of the time.

ggplot(length_comp, aes(x = Genotype, y = length_mean, colour = estimate_in_interval)) +
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


## calculate length of phenological period
doy_annual_pp_sum <- readRDS("objects/doy_annual_pp_sum.rds")

wide_data <- doy_annual_pp_sum %>%
  pivot_wider(
    names_from = event,           # Use the event column to create new column names
    values_from = c(DoY, .lower, .upper),  # Columns to spread into wider format
    names_sep = "_"              # Separator for new column names
  )

diff_data <- wide_data %>%
  mutate(
    diff_DoY = DoY_end - DoY_begin,
    diff_lower = .upper_end - .lower_begin,
    diff_upper = .lower_end - .upper_begin
  )

length_annual <- doy_annual_pp %>%
  pivot_wider(
    names_from = event,
    values_from = DoY
  ) %>%
  mutate(length = end - begin) %>%
  group_by(MAT, Site, Sex, Year) %>%
  median_hdci(length) %>%
  ungroup() %>%
  mutate(Site = forcats::fct_relevel(Site, shortsites))

# is length related to length of observation period at a site or frequency of observations?
