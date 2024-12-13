# calculate historical and future overlap of phenological periods

# depends
library(dplyr)
library(lubridate)
library(forcats)
library(tidyr)

doy_typical_allsites <- readRDS("objects/doy_typical_allsites.rds") # all provenances at all sites
factororder <- readRDS("objects/factororder.rds")
doy_normal <- readRDS('objects/doy_normal.rds')

# typical #######

## provenance at home #########

typical_intervals <- doy_typical_allsites %>%
  filter(Site == Source) %>%
  select(Site, MAT, DoY, Sex, event) %>%
  group_by(Site, MAT, Sex, event) %>%
  median_hdci(DoY) %>%
  select(Site, MAT, Sex, event, DoY) %>%
  distinct() %>%
  mutate(Date = ymd("2023-12-31") + DoY) %>%
  select(-DoY) %>%
  pivot_wider(values_from = Date, names_from = event) %>%
  mutate(flowering_period = interval(begin,end)) %>%
  select(-begin, -end, MAT) %>%
  pivot_wider(values_from = flowering_period, names_from = Sex)

# thank you Brian on [Stack Overflow](https://stackoverflow.com/questions/58517015/lubridate-find-overlap-time-between-interval-and-a-date)
int_overlaps_numeric <- function (int1, int2) {
  stopifnot(c(is.interval(int1), is.interval(int2)))

  x <- intersect(int1, int2)@.Data
  x[is.na(x)] <- 0
  days_of_overlap = as.numeric(as.duration(x), "days")
}

male_typical <- select(typical_intervals, SiteM = Site, MALE, MATM = MAT)
female_typical <- select(typical_intervals, SiteF = Site, FEMALE, MATF = MAT)

typical_overlap <- merge(male_typical, female_typical) %>%
  mutate(overlap = int_overlaps_numeric(FEMALE, MALE))
saveRDS(typical_overlap, "objects/typical_overlap.rds")

library(viridis)
ggplot(typical_overlap, aes(x = SiteM, y=SiteF, fill = overlap)) +
  geom_tile() +
  scale_fill_viridis() +
  ggtitle("Days of overlap in a typical year") +
  geom_text(data = filter(typical_overlap, SiteM == SiteF), aes(x = SiteM, y = SiteF, label = MATM))

## with only PGTIS provenance ###
typical_intervals_only_PGTIS <- doy_typical_allsites %>%
  #filter(Source == "Border") %>%
  select(Site, MAT, DoY, Sex, event) %>%
  group_by(Site, MAT, Sex, event) %>%
  median_hdci(DoY) %>%
  select(Site, MAT, Sex, event, DoY) %>%
  distinct() %>%
  mutate(Date = ymd("2023-12-31") + DoY) %>%
  select(-DoY) %>%
  pivot_wider(values_from = Date, names_from = event) %>%
  mutate(flowering_period = interval(begin,end)) %>%
  select(-begin, -end) %>%
  pivot_wider(values_from = flowering_period, names_from = Sex)

male_typical_only_PGTIS <- select(typical_intervals_only_PGTIS, SiteM = Site, MALE, MATM = MAT)
female_typical_only_PGTIS <- select(typical_intervals_only_PGTIS, SiteF = Site, FEMALE, MATF = MAT)

typical_overlap_only_PGTIS <- merge(male_typical_only_PGTIS, female_typical_only_PGTIS) %>%
  mutate(overlap = int_overlaps_numeric(FEMALE, MALE))
saveRDS(typical_overlap_only_PGTIS, "objects/typical_overlap_only_PGTIS.rds")

ggplot(typical_overlap_only_PGTIS, aes(x = SiteM, y=SiteF, fill = overlap)) +
  geom_tile() +
  scale_fill_viridis() +
  ggtitle("Days of overlap in a typical year - no MAT effect") +
  geom_text(data = filter(typical_overlap_only_PGTIS, SiteM == SiteF), aes(x = SiteM, y = SiteF, label = MATM))

# overlap - normal periods ###############
#
normal_intervals <- doy_normal %>%
  filter(period %in% c("1951-1980", "1981-2010", "2011-2040", "2041-2070", "2071-2100"),
         Site %in% c(focalsites),
         scenario %in% c("historical", "ssp245", "ssp585")) %>%
  select(Site, MAT, DoY, Sex, event, period, scenario) %>%
  group_by(Site, MAT, Sex, event, period, scenario) %>%
  median_hdci(DoY) %>%
  select(Site, MAT, Sex, event, DoY, period, scenario) %>%
  distinct() %>%
  mutate(Date = ymd("2023-12-31") + DoY) %>%
  select(-DoY) %>%
  pivot_wider(values_from = Date, names_from = event) %>%
  mutate(flowering_period = interval(begin,end)) %>%
  select(-begin, -end, MAT) %>%
  pivot_wider(values_from = flowering_period, names_from = Sex)

male_normal <- select(normal_intervals, SiteM = Site, MALE, MATM = MAT, period, scenario)
female_normal <- select(normal_intervals, SiteF = Site, FEMALE, MATF = MAT, period, scenario)

normal_overlap <- merge(male_normal, female_normal) %>%
  mutate(overlap = int_overlaps_numeric(FEMALE, MALE))
saveRDS(normal_overlap, "objects/normal_overlap.rds")

library(viridis)
ggplot(normal_overlap, aes(x = SiteM, y=SiteF, fill = overlap)) +
  geom_tile() +
  scale_fill_viridis() +
  ggtitle("Days of overlap in a typical year") +
  geom_text(data = filter(normal_overlap, SiteM == SiteF), aes(x = SiteM, y = SiteF, label = MATM)) +
  facet_grid(period ~ scenario)

