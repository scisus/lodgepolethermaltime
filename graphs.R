# visualizations
library(dplyr)
library(ggplot2)
library(forcats)
library(ggbeeswarm)

# raw data plot using phenf from modelmethods.R
ggplot(phenf, aes(x = sum_forcing, color = Event_Label, linetype = Sex)) +
  stat_ecdf() +
  labs(title = "Cumulative distribution of accumulated forcing for flowering events", caption = "raw data") +
  scale_colour_viridis_d() +
  theme_dark(base_size = 18) +
  ylab("") +
  xlab("GDD")

# plot sd parameters using variation from modelparameters.R
ggplot(variation, aes(y = fct_rev(.variable), x = .value, colour = .variable, linetype = Sex)) +
  stat_pointinterval(position = "dodge") +
  scale_colour_viridis_d() +
  labs(title = "Standard deviation of \npop mean & offsets", caption = "2000 draws from the posterior") +
  ylab("") +
  xlab("GDD") +
  facet_grid(event ~ .) +
  guides(color = "none", size = "none") +
  theme_dark(base_size = 18) +
  theme(legend.position = "top")

# plot medians of offset parameters in point clouds (like beeswarm)
offsets_summary %>%
  select(model, Sex, event, factor, level, .value, .point) %>% distinct() %>%
  ggplot(aes(y=.value, x = model, colour = Sex, shape = event)) +
  geom_quasirandom(alpha = 0.5) +
  facet_wrap("factor") +
  labs(title = "Offset medians", caption = "2000 draws from posterior") +
  geom_hline(yintercept = 0, linetype =3, colour = "darkgray") +
  theme_dark(base_size = 18) +
  ylab("GDD") +
  scale_colour_viridis_d() +
  theme(legend.position = "top") +
  geom_hline(yintercept = 0, linetype = 3)

# interval plot for site level offsets using siter from modelparameters.R
ggplot(siter, aes(y=level, x = .value, colour = Sex)) +
  stat_pointinterval() +
  facet_grid(event ~ Sex) +
  ggtitle("Site offsets", subtitle = "Ordered warmest to coldest MAT") +
  theme_dark(base_size = 18) +
  ylab("Site") +
  xlab("GDD") +
  geom_vline(xintercept = 0, linetype =3) +
  xlim(c(-90,90)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top")

# interval plot for provenance level offsets using provr from modelparameters.R
ggplot(provr, aes(y=level, x = .value, colour = Sex)) +
  stat_pointinterval() +
  facet_grid(event ~ Sex) +
  ggtitle("Provenance offsets", subtitle = "Ordered warmest to coldest MAT") +
  theme_dark(base_size = 18) +
  ylab("Provenance") +
  xlab("GDD") +
  geom_vline(xintercept = 0, linetype =3)  +
  xlim(c(-90,90)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top")

# interval plot for year level offsets using yearr from modelparameters.R
ggplot(yearr, aes(y=level, x = .value, colour = Sex)) +
  stat_pointinterval() +
  facet_grid(event ~ Sex) +
  ggtitle("Year offsets", subtitle = "Ordered warmest to coldest MAT") +
  theme_dark(base_size = 18) +
  xlab("GDD") +
  ylab("Year") +
  geom_vline(xintercept = 0, linetype =3, colour = "darkgray")  +
  xlim(c(-90,90)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top")
