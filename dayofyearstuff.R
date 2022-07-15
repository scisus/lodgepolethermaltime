
library(purrr)

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
  select(-temporary)

# plot typical climate
ggplot(filter(typicalclim, DoY < 180), aes(x = DoY, y = mean_sum_forcing, color = Site)) +
  geom_line()

# in a typical year at all my sites (mean temp 1945-2012 to create sum_forcing), calculate average predicted DoY for flowering (excluding site effects)
doy_typical <- map_dfr(split(typicalclim, f = list(typicalclim$Site), drop = TRUE),
    find_day_of_forcing, .id = ".id",
    bdf = fepred, aforce = "mean_sum_forcing", bforce = ".epred") %>%
  rename(Site = .id, DoY = newdoycol) %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site))

ggplot(filter(doy_typical, Sex == "FEMALE"), aes(x = DoY, y = Generation, color = event)) +
  stat_pointinterval() +
  facet_wrap("Site") +
  labs(title = "Female expected flowering day", subtite = "in a typical year", caption = "typical year based on mean daily heat sum accumulation at 7 sites between 1945 and 2012")

ggplot(filter(doy_typical, Sex == "MALE"), aes(x = DoY, y = Generation, color = event)) +
  stat_pointinterval() +
  facet_wrap("Site") +
  labs(title = "Female expected flowering day", subtitLe = "in a typical year", caption = "typical year based on mean daily heat sum accumulation at 7 sites between 1945 and 2012")

gendiff <- doy_typical %>%
  filter(Generation %in% c("1", "1.75", "Advanced")) %>%
  group_by(Site, Sex, event, Generation) %>%
  summarise(med_doy = mean(DoY)) %>%
  pivot_wider(names_from = "Generation", values_from = "med_doy") %>%
  mutate(contrast_1.75 = `1.75` - `1`, contrast_Advanced = Advanced - `1`)

ggplot(gendiff, aes(x = Site, y = contrast_1.75, color = Sex)) +
  geom_point() +
  facet_grid(Sex ~ event)

ggplot(gendiff, aes(x = Site, y = contrast_Advanced, color = Sex)) +
  geom_point() +
  facet_grid(Sex ~ event)

# In a typical year at each of my 7 sites, the change from 1st generation to 1.75 generation delays flowering by less than a day on average and hastens the end of flowering by less than a day on average. The change from 1st generation to Advanced generation orchard delays the start of flowering by about a day and hastens the end of flowering by about 4 days.
# No need to do multiple sites - difference isn't that meaningful.
# This was with means - i should calculate hpds



