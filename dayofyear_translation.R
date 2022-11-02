# convert forcing to day of year

library(purrr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidybayes)
library(tidyr)


source('phenology_functions.R')

# daily forcing data

dailyforc <- read.csv("data/dailyforc_1945_2012.csv") %>% # daily "real" forcing
  group_by(Site, Year) %>%
  mutate(index = cur_group_id()) %>% ungroup()
typical_year_forc <- read.csv("data/typical_year_forc.csv") %>% # from temp mean at each site across 1945-2012
  mutate(Date = as.Date(Date_scale)) %>% select(-Date_scale)
normal_forc <- read.csv("data/normalforc_1901-2100.csv") %>% # averaged over 30 year periods
  group_by(Site, period, scenario) %>%  # index
  mutate(index = cur_group_id()) %>% ungroup()

datetodoy <- data.frame(Scale = seq(ymd('2022-01-01'), ymd('2022-12-31'), by = "1 day")) %>%
  mutate(DoY = yday(Scale))

fepred <- readRDS("objects/fepred.rds")
factororder <- readRDS("objects/factororder.rds")

# for each site, generate a timeseries of mean temperatures and associated accumulated forcing that reflects the general pattern of temperatures throughout the year. Do this by averaging temperatures on each day between 1945 and 2011 at each site.
# alternate source for this data would be typical_ts.csv in lodgepole_climate project


# plot typical year temperature timeseries for each site ~ spring
ggplot(filter(typical_year_forc, DoY < 180 & DoY > 100), aes(x = Date, y = sum_forcing, color = Site)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")
ggplot(typical_year_forc, aes(x = Date, y = sum_forcing, color = Site)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b")

typical_year_forc %>%
  group_by(Site) %>%
  summarise(meantemp = mean(mean_temp)) %>%
  arrange(meantemp)

# avg predicted DoY for flowering at seed orchard sites ####
# in a typical year at all my sites (mean temp 1945-2012 to create sum_forcing), calculate average predicted DoY for flowering (excluding site effects)
# 9000 draws per site
doy_typical <- map_dfr(split(typical_year_forc, f = list(typical_year_forc$Site), drop = TRUE),
                       find_day_of_forcing, .id = ".id",
                       bdf = fepred, aforce = "sum_forcing", bforce = ".epred") %>%
  rename(Site = .id, DoY = newdoycol) %>%
  ungroup() %>%
  select(-.row, -.chain, -.iteration, -.draw) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  left_join(select(typical_year_forc, Date, DoY) %>% distinct())
saveRDS(doy_typical, "objects/doy_typical.rds")


# in various climate normal periods ####
# 9000 draws per normal period

doy_normal <- map_dfr(split(normal_forc, f = list(normal_forc$index), drop = TRUE),
                      find_day_of_forcing, .id = ".id",
                      bdf = fepred, aforce = "sum_forcing", bforce = ".epred") %>%
  rename(index = .id, DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  select(-.row, -.chain, -.iteration, -.draw) %>%
  left_join(select(normal_forc, index, Site, period, scenario) %>% distinct()) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))



# year to year variation ####
# 9000 draws per year 1945-2011 per site

doy_annual <- map_dfr(split(dailyforc, f = list(dailyforc$index), drop = TRUE),
                      find_day_of_forcing, .id = "index",
                      bdf = fepred, aforce = "sum_forcing", bforce = ".epred") %>%
  rename(DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  select(-.row, -.chain, -.iteration, -.draw) %>%
  left_join(select(dailyforc, index, Site, Year) %>% distinct()) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))

# graph year to year variation ####

doy_annual_plotting <- doy_annual %>%
  filter(Site %in% c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border")) %>%
  group_by(Site, Year, Sex, event) %>%
  median_hdci(DoY) #slow

dplot2 <- doy_annual_plotting %>%
  pivot_wider(values_from = c(DoY, .lower, .upper), names_from = event)

ggplot(dplot2, aes(x = Year, ymin = .lower_begin, ymax = .upper_begin, fill = Sex)) +
  geom_ribbon(alpha = 0.5) +
  geom_ribbon(aes(x = Year, ymin = .lower_end, ymax = .upper_end, fill = Sex), alpha = 0.5) +
  geom_line(data=doy_annual_plotting, aes(x = Year, y = DoY, color = Sex, linetype = event), inherit.aes = FALSE) +
  facet_grid(Sex ~ Site) +
  labs(title = "Predicted flowering periods", subtitle = "posterior expectation, ribbons = uncertainty, lines = medians") +
  ylab("Day of Year") +
  theme(legend.position = "bottom")
ggsave("..flowering-cline/figures/yearly_phenology.png", width = 14, height = 7, units = "in")

# variation
summary_doy_annual <- doy_annual %>%
  mutate(normal_period = case_when(Year >= 1951 & Year <= 1980 ~ "1951-1980",
                                   Year >= 1981 & Year <= 2010 ~ "1981-2010")) %>%
  filter(!is.na(normal_period)) %>%
  group_by(normal_period, Sex, event, Site) %>%
  summarise(median_forcing = median(.epred), median_DoY = median(DoY), sd_forcing = sd(.epred), sd_DoY = sd(DoY))

ggplot(summary_doy_annual, aes(x = Site, y = sd_DoY, color = normal_period)) +
  geom_point() +
  facet_grid(Sex ~ event) +
  labs(title = "Year-to-year variation in flowering phenology at 9 Sites", subtitle = "over two 30-year climate normal periods") +
  theme(legend.position = "bottom") +
  theme_bw()
ggsave("../flowering-cline/figures/year2yearvar.png", width = 10, height = 6, units = "in")


# graph normals ####

doy_normal_plotting <- doy_normal %>%
  filter(#! scenario %in% c( "ssp370"),
         period %in% c("1951-1980", "1981-2010", "2011-2040", "2041-2070", "2071-2100"),
         Site %in% c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border"),
         scenario %in% c("historical", "ssp245", "ssp585")) %>%
  left_join(datetodoy)
ggplot(filter(doy_normal_plotting, event == "begin"), aes(x = scenario, y = DoY, colour = Site, shape = Sex)) +
  stat_pointinterval(position = "dodge") +
  stat_pointinterval(data = filter(doy_normal_plotting, event == "end"), position = "dodge") +
  #scale_y_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap("period", scales = "free_x", nrow = 1) +
  theme_bw() +
  theme(legend.position = "bottom")  +
  labs(title = "Flowering period expectation", subtitle = "1951-2100 for 4 Shared Socioeconomic Pathways") +
  xlab("Shared Socioeconomic Pathway") +
  ylab("Day of Year")
ggsave("../flowering-cline/figures/normal_predictions.png", width = 13, height = 5, units = "in")




# contrast ####
# this contrast does not compare site effects - uses grand means to describe how different sites are on average. forcing the same for each site

# how much do the max sites differ from one another
sitediff <- doy_typical %>%
  group_by(Site, Sex, event) %>%
  summarise(med_doy = mean(DoY)) %>%
  pivot_wider(names_from = "Site", values_from = "med_doy") %>%
  mutate(contrast_PGTIS = PGTIS - Kalamalka)






