# Special points
# Identify clones represented across multiple years
# Clones represented in multiple orchards
# Clones represented by multiple trees in the same orchard

library(dplyr)

modeldata <- readRDS("objects/datlist.rds") %>%
  bind_rows()

simpledat <- modeldata %>%
  select(Tree, Genotype, Year, Site) %>% distinct()

# Clones represented across multiple years

mys <- modeldata %>%
  select(Genotype, Year) %>%
  distinct() %>%
  group_by(Genotype) %>%
  summarise(nyears = n()) %>%
  mutate(yearstf = case_when(nyears == 1 ~ FALSE,
                             nyears > 1 ~ TRUE))

# Clones represented in multiple orchards (sites)

mss <- modeldata %>%
  select(Genotype, Site) %>%
  distinct() %>%
  group_by(Genotype) %>%
  summarise(nsites = n()) %>%
  mutate(sitestf = case_when(nsites == 1 ~ FALSE,
                             nsites > 1 ~ TRUE))

# Clones represented by multiple trees in the same orchard

mtss <- modeldata %>%
  select(Tree, Genotype, Site) %>%
  distinct() %>%
  group_by(Genotype, Site) %>%
  summarise(ntrees = n()) %>%
  mutate(treestf = case_when(ntrees == 1 ~ FALSE,
                             ntrees > 1 ~ TRUE))


# join and identify no replication
replication_points <- mys %>%
  full_join(mss) %>%
  full_join(mtss) %>%
  group_by(Genotype, Site) %>%
  mutate(replicated = base::any(yearstf, sitestf, treestf))

replication_points %>% filter(replicated == TRUE) %>% nrow()
# save all replication info
saveRDS(replication_points, "objects/replication_points.rds")
