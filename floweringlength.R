# length of flowering period

# historical ####
#individual lengths
specific_doy_preds_length <- specific_doy_preds %>%
  ungroup() %>%
  select(-prediction, -sum_forcing, -upper, -contains("censored")) %>%
  pivot_wider(names_from = event, values_from = newdoycol) %>%
  mutate(length = end - begin)

ggplot(specific_doy_preds_length, aes(x = length, y = prediction_type, colour = Sex)) +
  stat_histinterval(position = "dodge", .width = c(0.5, 0.89)) +
  scale_colour_viridis_d() +
  labs(title = "Length of phenological period for individuals")

# population lengths
specific_doy_preds_length_ts <- specific_doy_preds %>%
  group_by(Sex, event, Site, Year, prediction_type) %>%
  median_hdci(newdoycol, .width = c(0.5, 0.89)) %>%
  # drop uncertainty for now
  filter(.width == 0.5) %>%
  select(-starts_with(".")) %>% distinct() %>%
  pivot_wider(names_from = event, values_from = newdoycol) %>%
  mutate(length = end - begin) %>%
  ungroup() %>%
  mutate(Site = forcats::fct_relevel(as.factor(Site), sitefactororder))


ggplot(filter(specific_doy_preds_length_ts, prediction_type == "prediction - full cross"), aes(x = Site, y = length, colour = Sex)) +
  geom_beeswarm(dodge.width =0.75) +
  scale_colour_viridis_d() +
  labs(title = "Median flowering period length at a site", subtitle = "Each point represents one year at one site 1997-2012", caption = "fully crossed predictions") +
  theme(legend.position = "bottom") +
  ylab("Length of flowering period (days)")

futlen <- doypredmatchfut_medians %>%
  select(-contains("Date"), -contains(".lower"), -contains(".upper"), -.width) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = event, values_from = newdoycol) %>%
  mutate(period_length = end - begin)

ggplot(filter(futlen, climate_forcing %in% c(4.5, 8.5)), aes(y = period_length, x = normal_period, colour = Sex)) +
  geom_point()  +
  facet_grid(climate_forcing ~ Site) +
  #scale_colour_viridis_d(end = 0.9) +
  theme_dark(base_size = 18) +
  #theme(legend.position = "none") +
  scale_colour_viridis_d() +
  theme(legend.position = "bottom") +
  ylab("Length of flowering period (days)") +
  labs(title = "Future flowering period median length", caption = "fully crossed - only 5 posterior samples!") +
  theme(axis.text.x = element_text(angle = 30, hjust=1), legend.position = "top") +
  xlab("Normal period")
