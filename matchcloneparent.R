# Match clones to parent locations
#
# Clones are assigned same number as their parents

library(dplyr)
library(tidyr)

parentdat <- read.csv('../phd/data/OrchardInfo/ParentTrees/ParentTreeExtractReport_ParentTrees_2014_05_15_13_38_17.csv')  %>%
  mutate(Latitude = Latitude.Degrees + Latitude.Minutes/60 + Latitude.Seconds/60^2,
         Longitude = -(Longitude.Degrees + Longitude.Minutes/60 + Longitude.Seconds/60^2))

write.csv(parentdat, "../phd/data/OrchardInfo/ParentTrees/parents.csv", row.names = FALSE)

phenf <- readRDS("objects/phenf.rds")

clones <- phenf %>%
  select(Clone, Provenance) %>% distinct() %>%
  mutate(Clone = as.numeric(Clone))


parentdat <- parentdat %>% filter(Parent.Tree.Number %in% clones$Clone) %>%
  left_join(clones, by = c("Parent.Tree.Number" = "Clone"))


# map stuff
library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
library('rnaturalearthhires')
library("ggspatial")

world <- ne_states(returnclass = "sf", country = c("United States of America", "Canada"))

class(world)

parents <- st_as_sf(parentdat, coords = c("Longitude", "Latitude"),
                  crs = 4326, agr = "constant")

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = parents, aes(colour = Provenance), alpha = 0.75) +
  #annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-131, -112), ylim = c(48, 60), expand = FALSE) +
  ggtitle("Parent trees")

# Are parent trees from the provenances they're assigned to in the seed orchard
library(terra)
spu <- vect("../phd/maps/SPUmaps/SPU_Tongli/Pli_SPU.shp", layer = "Pli_SPU")
#spu <- readOGR("..phd/maps/SPUmaps/SPU_Tongli", layer="Pli_SPU")
#levels(spu$SPU) <- c("Bulkley Valley High", "Bulkley Valley Low", "Bulkley Valley-Central Plateau Transition High", "Bulkley Valley-Central Plateau Transition Low", "Bulkley Valley-Central Plateau Transition Mid",  "Bulkley Valley-Prince George Transition High", "Bulkley Valley-Prince George Transition Low", "Central Plateau High", "Central Plateau Low", "Central Plateau-Prince George Transition High", "Central Plateau-Prince George Transition Low", "Central Plateau-Prince George Transition Mid", "East Kootenay High", "East Kootenay Low", "Nelson High", "Nelson Low", "Nass Skeena High", "Nass Skeena Low", "Prince George High", "Prince George Low", "Prince George-Nelson Transition High", "Prince George-Nelson Transition Low", "Prince George-Nelson Transition Mid", "Thompson Okanagan High", "Thompson Okanagan Low", "Thompson Okanagan Mid", "Thompson Okanagan-Nelson Transition High", "Thompson Okanagan-Nelson Transition Low", "Thompson Okanagan-Nelson Transition Mid")

par_pnts <- vect(cbind(parentdat$Longitude, parentdat$Latitude), crs="+proj=longlat") %>% project(crs(spu))

par_pnts$id <- parentdat$Parent.Tree.Number
intersect_vec <- terra::intersect(par_pnts, spu)


shortnames <- clones %>% select(Provenance) %>% distinct() %>%
  mutate(Prov_shortname = case_when(Provenance == "Bulkley Valley Low" ~ "PLI BV LOW",
                                    Provenance == "Nelson Low" ~ "PLI NE LOW",
                                    Provenance == "Prince George Low" ~ "PLI PG LOW",
                                    Provenance == "Central Plateau Low" ~ "PLI CP LOW",
                                    Provenance == "Thompson Okanagan Low" ~ "PLI TO LOW",
                                    Provenance == "Thompson Okanagan High" ~ "PLI TO HIGH"))

parinspu <- terra::relate(spu, par_pnts, "contains") %>%
  data.frame() %>%
  mutate(SPU_polygon = spu$SPU)

par_spus <- data.frame(Clone = intersect_vec$id, SPU_Number = intersect_vec$SPU_ID, Parent_Provenance = intersect_vec$SPU) %>%
  # add back clones that aren't in any SPU zone
  full_join(clones) %>%
  left_join(shortnames) %>%
  mutate(Parent_in_Orch_Prov = Parent_Provenance == Prov_shortname)

# clones not in spu
nclone <- length(unique(par_spus$Clone)) # total number of clones

clones_with_no_spu <- par_spus %>% filter(is.na(Parent_Provenance)) %>%
  select(Clone) %>% distinct() %>% nrow()

# percent of clones not from spu
clones_with_no_spu/nclone  # 15%

# how many clones are in multiple orchards
par_spus %>%
  group_by(Clone) %>%
  summarise(Orchard_membership = length(Provenance)) %>%
  filter(Orchard_membership > 1)

46/nclone
