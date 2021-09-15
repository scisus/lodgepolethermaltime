# what observations are end vs interval censoring

# depends
library(dplyr)
library(tidyr)

phenf <- readRDS("objects/phenf.rds")

# what proportion of data is censored end vs. interval?
censdf <- filter(phenf, Event_Obs %in% c(2,3)) %>%
  group_by(Event_Label, Sex) %>%
  mutate(ng = n()) %>%
  group_by(Event_Label, Sex, censored) %>%
  summarise(prop_cens = n()/unique(ng)) %>%
  mutate(prop_cens = round(prop_cens, 2)) %>%
  arrange(Event_Label, Sex) %>%
  pivot_wider(names_from = censored, values_from = prop_cens)

saveRDS(censdf, file = "objects/censdf.rds")



