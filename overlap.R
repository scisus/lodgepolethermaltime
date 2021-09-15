# calculate historical and future overlap of phenological periods

# depends
library(dplyr)
library(lubridate)
library(forcats)

general_doy_preds_med_siteyearsex <- readRDS("objects/general_doy_preds_med_siteyearsex.rds")
doypredmatchfut_medians <- readRDS("objects/doypredmatchfut_medians.rds")
factororder <- readRDS("objects/factororder.rds")

# historical ####
#
med_sys_begin <- filter(general_doy_preds_med_siteyearsex, event == "begin") %>%
rename(b.newdoycol = newdoycol, b.lower = .lower, b.upper = .upper, b.date = Date, b.lowerdate = .lowerdate, b.upperdate = .upperdate) %>%
  select(-event)
med_sys_end <- filter(general_doy_preds_med_siteyearsex, event == "end") %>%
  rename(e.newdoycol = newdoycol, e.lower = .lower, e.upper = .upper, e.date = Date, e.lowerdate = .lowerdate, e.upperdate = .upperdate) %>%
  select(-event)

med_interval_sys <- full_join(med_sys_begin, med_sys_end) %>%
  mutate(med_interval = lubridate::interval(b.date, e.date, tzone = "UTC")) %>% # calculate interval for phenology
  select(Sex, Site, Year, .width, .point, .interval, med_interval)

female_intervals <- filter(med_interval_sys, Sex == "FEMALE") %>%
  rename(female_med_interval = med_interval) %>%
  select(-Sex)

male_intervals <- filter(med_interval_sys, Sex == "MALE") %>%
  rename(male_med_interval = med_interval, male_Site = Site) %>%
  select(-Sex)

med_overlap_sys <- full_join(female_intervals, male_intervals) %>%
  mutate(overlap = day(as.period(intersect(female_med_interval, male_med_interval), "days")) + 1, Site = forcats::fct_relevel(Site, factororder$site), male_Site = forcats::fct_relevel(male_Site, factororder$site))
med_overlap_sys[is.na(med_overlap_sys)] <- 0
saveRDS(med_overlap_sys, file = "objects/med_overlap_sys.rds")

# future ####

# this could be functionalised - see historical overlap
med_fut_begin <- filter(doypredmatchfut_medians, event == "begin") %>%
  rename(b.newdoycol = newdoycol, b.lower = .lower, b.upper = .upper, b.date = Date, b.lowerdate = .lowerdate, b.upperdate = .upperdate) %>%
  select(-event)
med_fut_end <- filter(doypredmatchfut_medians, event == "end") %>%
  rename(e.newdoycol = newdoycol, e.lower = .lower, e.upper = .upper, e.date = Date, e.lowerdate = .lowerdate, e.upperdate = .upperdate) %>%
  select(-event)

med_interval_fut_sys <- full_join(med_fut_begin, med_fut_end) %>%
  mutate(med_interval = lubridate::interval(b.date, e.date, tzone = "UTC")) %>% # calculate interval for phenology
  select(Sex, Site, .width, .point, .interval, med_interval, SSP, normal_period, climate_forcing)

female_fut_intervals <- filter(med_interval_fut_sys, Sex == "FEMALE") %>%
  rename(female_med_interval = med_interval) %>%
  select(-Sex)

male_fut_intervals <- filter(med_interval_fut_sys, Sex == "MALE") %>%
  rename(male_med_interval = med_interval, male_Site = Site) %>%
  select(-Sex)

fut_overlap_sys <- full_join(female_fut_intervals, male_fut_intervals) %>%
  mutate(overlap = day(as.period(intersect(female_med_interval, male_med_interval), "days")) + 1, Site = forcats::fct_relevel(Site, factororder$site), male_Site = forcats::fct_relevel(male_Site, factororder$site))
fut_overlap_sys[is.na(fut_overlap_sys)] <- 0
saveRDS(fut_overlap_sys, file = "objects/fut_overlap_sys.rds")



# this might work better if calculated as a change in overlap
