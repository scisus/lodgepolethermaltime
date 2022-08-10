
library(purrr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidybayes)
library(tidyr)
library(patchwork)

source('phenology_functions.R')

datetodoy <- data.frame(Scale = seq(ymd('2022-01-01'), ymd('2022-12-31'), by = "1 day")) %>%
  mutate(DoY = yday(Scale))

fepred <- readRDS("objects/fepred.rds")
factororder <- readRDS("objects/factororder.rds")
histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
typicalclim <- histclim %>%
  group_by(Site, DoY) %>%
  summarise(mean_mean_temp = mean(mean_temp)) %>%
  mutate(temporary = case_when(mean_mean_temp < 5 ~ 0,
                                      mean_mean_temp >=5 ~ mean_mean_temp),
         mean_sum_forcing = cumsum(temporary)) %>%
  select(-temporary) %>%
  left_join(datetodoy)


# plot typical climate
ggplot(filter(typicalclim, DoY < 180), aes(x = Scale, y = mean_sum_forcing, color = Site)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")


# in a typical year at all my sites (mean temp 1945-2012 to create sum_forcing), calculate average predicted DoY for flowering (excluding site effects)
doy_typical <- map_dfr(split(typicalclim, f = list(typicalclim$Site), drop = TRUE),
    find_day_of_forcing, .id = ".id",
    bdf = fepred, aforce = "mean_sum_forcing", bforce = ".epred") %>%
  rename(Site = .id, DoY = newdoycol) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  left_join(datetodoy)

# this contrast does not compare site effects - uses grand means to describe how different sites are on average. forcing the same for each site
expdoy <- ggplot(doy_typical, aes(x = DoY, y = forcats::fct_rev(Sex), color = Sex, shape = event)) +
  stat_pointinterval() +
  facet_grid(forcats::fct_rev(Site) ~ .) +
  labs(caption = "typical year based on mean daily heat sum accumulation at 7 sites between 1945 and 2012") +
  xlab("Day of Year") +
  theme(strip.text.y.right = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# put side-by-side with forcing, which is the same for each site!
expforc <- ggplot(fepred, aes(x=.epred, y = forcats::fct_rev(Sex), color = Sex, shape = event)) +
  stat_halfeye() +
  xlab("Accumulated Growing Degree Days") +
  theme(legend.position = "none", axis.title.y = element_blank())
  #labs(title = "Forcing requirements", subtitle = "in any year or site")

expected <- expforc + expdoy
expected + plot_annotation(title = "Event expectations",
                           subtitle = "from thermal time model",
                           tag_levels = "A")
# how much do the max sites differ from one another
sitediff <- doy_typical %>%
  group_by(Site, Sex, event) %>%
  summarise(med_doy = mean(DoY)) %>%
  pivot_wider(names_from = "Site", values_from = "med_doy") %>%
  mutate(contrast_PGTIS = PGTIS - Kalamalka)




