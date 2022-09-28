# Add forcing to climate data

library(dplyr)
library(lubridate)

# set threshold for GDD calculation
threshold <- 5

pnwnamet_adj <- read.csv("../lodgepole_climate/processed/PNWNAmet_adjusted.csv") %>%
  select(Date, Site, mean_temp_corrected) %>%
  rename(mean_temp = mean_temp_corrected) %>%
  mutate(DoY = yday(Date))

dailyforc_1945_2012 <- pnwnamet_adj %>%
  mutate(Year = lubridate::year(Date)) %>%
  arrange(Site, Date) %>%
  mutate(forcing = case_when(mean_temp <= threshold ~ 0,
                             mean_temp > threshold ~ mean_temp - threshold)) %>% # calculate forcing
  group_by(Site, Year) %>%
  mutate(sum_forcing = cumsum(forcing)) #calculate accumulated forcing

allforc <- full_join(old_forc_man, dailyforc_1945_2012) %>%
  select(-forcing, -sum_forcing, -old_sf, -old_f) %>%
  pivot_longer(cols = c(mean_temp, old_mt)) %>%
  rename(source = name) %>%
  group_by(source, Site, DoY) %>%
  summarise(mintemp = min(value), maxtemp = max(value), meantemp = mean(value))

library(ggplot2)
ggplot(allforc, aes(x = DoY, y = mintemp, linetype = source)) +
  geom_line() +
  facet_wrap("Site")

write.csv(dailyforc_1945_2012, "data/dailyforc_1945_2012.csv", row.names = FALSE)

# test that forcing always > 0
all(dailyforc_1945_2012$forcing >= 0)



