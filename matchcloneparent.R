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

climatena <- parentdat %>%
  select(ID1 = Parent.Tree.Number, ID2 = Provenance, lat = Latitude, long = Longitude, el = Elevation)

write.csv(climatena, file = "../phd/data/OrchardInfo/ParentTrees/locations_for_climatena.csv", eol = "\r\n", row.names = FALSE)

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
  ggtitle("Parent trees & sites")

# add sites and maybe provenances to this map

