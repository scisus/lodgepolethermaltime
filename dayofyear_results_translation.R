# Translate GDD results into typical year days

# translate the difference between coldest and warmest provenance into day of year in a typical year at each site
typical_year_forc <- read.csv("data/typical_year_forc.csv") %>% # from temp mean at each site across 1945-2012
  mutate(Date = as.Date(Date_scale)) %>% select(-Date_scale)

fpred_allprovs <- readRDS("objects/fepred_allprovs.rds")

# Calculate the maximum and minimum of MAT
max_MAT <- max(fepred_allprovs$MAT, na.rm = TRUE)
min_MAT <- min(fepred_allprovs$MAT, na.rm = TRUE)

# Filter rows where MAT is the maximum or minimum, and add a new column
minmaxMAT_fepred <- fepred_allprovs %>%
  # keep only coldest and warmest provenances and label
  filter(MAT == max_MAT | MAT == min_MAT) %>%
  mutate(size = case_when(
    MAT == max_MAT ~ "max",
    MAT == min_MAT ~ "min"
  )) %>%
  # order model samples
  group_by(model, Sex, event, size) %>%
  arrange(Sex, event, size, .epred) %>%
  group_by(model, size) %>%
  mutate(order = 1:n()) %>%
  select(-.row, -.draw, -MAT) %>%
  pivot_wider(names_from = size, values_from = .epred) %>%
  # get differences
  mutate(differences = max - min) %>%
  group_by(Sex, event) %>%
  mean_hdci(differences)
minmaxMAT_fepred
