# censoring
#
# # censorship ####

# what proportion of data is censored end vs. interval?
censdf <- filter(phenf, Event_Obs %in% c(2,3)) %>%
  group_by(Event_Label, Sex) %>%
  mutate(ng = n()) %>%
  group_by(Event_Label, Sex, censored) %>%
  summarise(prop_cens = n()/unique(ng)) %>%
  arrange(Event_Label, Sex)

ggplot(censdf, aes(x = Sex, y = prop_cens, fill = censored)) +
  geom_bar(stat = "identity") +
  facet_wrap("Event_Label")

