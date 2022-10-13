# convert forcing to day of year

library(purrr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidybayes)
library(tidyr)


source('phenology_functions.R')

datetodoy <- data.frame(Scale = seq(ymd('2022-01-01'), ymd('2022-12-31'), by = "1 day")) %>%
  mutate(DoY = yday(Scale))

fepred <- readRDS("objects/fepred.rds")
factororder <- readRDS("objects/factororder.rds")
dailyforc <- read.csv("data/dailyforc_1945_2012.csv") # site clim with forcing

# for each site, generate a timeseries of mean temperatures and associated accumulated forcing that reflects the general pattern of temperatures throughout the year. Do this by averaging temperatures on each day between 1945 and 2011 at each site.
# alternate source for this data would be typical_ts.csv in lodgepole_climate project
typical_ts <- dailyforc %>%
  group_by(Site, DoY) %>%
  summarise(mean_mean_temp = mean(mean_temp)) %>%
  mutate(temporary = case_when(mean_mean_temp < 5 ~ 0,
                                      mean_mean_temp >=5 ~ mean_mean_temp),
         mean_sum_forcing = cumsum(temporary)) %>%
  select(-temporary) %>%
  left_join(datetodoy)


# plot typical year temperature timeseries for each site ~ spring
ggplot(filter(typical_ts, DoY < 180 & DoY > 100), aes(x = Scale, y = mean_sum_forcing, color = Site)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")


# avg predicted DoY for flowering at seed orchard sites ####
# in a typical year at all my sites (mean temp 1945-2012 to create sum_forcing), calculate average predicted DoY for flowering (excluding site effects)
doy_typical <- map_dfr(split(typical_ts, f = list(typical_ts$Site), drop = TRUE),
    find_day_of_forcing, .id = ".id",
    bdf = fepred, aforce = "mean_sum_forcing", bforce = ".epred") %>%
  rename(Site = .id, DoY = newdoycol) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  left_join(datetodoy)
saveRDS(doy_typical, "objects/doy_typical.rds")

# contrast ####
# this contrast does not compare site effects - uses grand means to describe how different sites are on average. forcing the same for each site

# how much do the max sites differ from one another
sitediff <- doy_typical %>%
  group_by(Site, Sex, event) %>%
  summarise(med_doy = mean(DoY)) %>%
  pivot_wider(names_from = "Site", values_from = "med_doy") %>%
  mutate(contrast_PGTIS = PGTIS - Kalamalka)






