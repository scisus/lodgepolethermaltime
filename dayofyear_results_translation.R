# Translate GDD results into typical year days

# translate the difference between coldest and warmest provenance into day of year in a typical year at each site
typical_year_forc <- read.csv("data/typical_year_forc.csv") %>% # from temp mean at each site across 1945-2012
  mutate(Date = as.Date(Date_scale)) %>% select(-Date_scale)

fpred_allsites <- readRDS("objects/fepred_allsites.rds")


# Filter rows where MAT is the maximum or minimum, and add a new column
minmaxMAT_fepred <- fepred_allsites %>%
  # keep only coldest and warmest provenances and label
  filter(Site %in% c("Kalamalka", "Border")) %>%
  # order model samples
  group_by(model, Sex, event, Site) %>%
  arrange(Sex, event, Site, .epred) %>%
  mutate(order = 1:n()) %>%
  select(-.row, -.draw, -MAT) %>%
  pivot_wider(names_from = Site, values_from = .epred) %>%
  # get differences
  mutate(differences = Kalamalka - Border) %>%
  group_by(Sex, event)

minmaxMAT_fepred_summary <- minmaxMAT_fepred %>%
  mean_hdci(differences)

# now match minmaxMAT_fepred$differences with typical_year_forc$forcing

typical_year_forc_BK <- filter(typical_year_forc, Site %in% c("Border", "Kalamalka"))

fepred_Kal <- filter(minmaxMAT_fepred, Site == "Kalamalka")

ggplot(filter(typical_year_forc_BK, DoY > 121 & forcing > 0), aes(x = forcing, fill = Site)) +
  geom_histogram() +
  facet_wrap("Site")
