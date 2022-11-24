
mhistclim <- histclim %>% select(Site, Year, DoY, forcing) %>%
  group_by(Site, DoY) %>%
  summarise(forcing = mean(forcing)) %>%
  mutate(sum_forcing = cumsum(forcing))

kalonly <- histclim %>% filter(Site == "Kalamalka", DoY < 180, DoY > 75)

ggplot(data = kalonly, aes(x = DoY, y = sum_forcing, color = Year, group = Year) )+
  geom_line() +
  geom_line(data=filter(mhistclim, Site == "Kalamalka", DoY < 180, DoY > 75), aes(x = DoY, y = sum_forcing), inherit.aes = FALSE, size = 2)

naclim <- read.csv("data/all_clim_PCIC.csv") %>%
  filter(forcing_type == "gdd") %>%
  rename(namt = mean_temp) %>%
  select(Site, Year, DoY, namt)
bcclim <- read.csv("data/dailyforc_1945_2012.csv") %>%
  select(-contains("forcing")) %>%
  rename(bcmt = mean_temp) %>%
  filter(Site %in% unique(naclim$Site))

clim <- full_join(naclim, bcclim)

ggplot(clim, aes(x = namt, y = bcmt)) +
  geom_point() +
  facet_wrap("Site")
