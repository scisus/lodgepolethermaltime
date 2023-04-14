library(ggplot2)
theme_set(theme_bw())
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(viridis)
library(dplyr)
library(flowers)
library(ggspatial)
# devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

# NAD83 / Canada Atlas Lambert 3978
# WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS 4326

#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

base <- ne_states(country = c("canada", "united states of america"), returnclass = "sf") %>%
    st_transform(crs = 3978)

clones <- select(flowers::lodgepole_phenology_event, Clone) %>% distinct()

parentdat <- read.csv('data/OrchardInfo/ParentTrees/parents.csv') %>%
    filter(Parent.Tree.Number %in% clones$Clone) %>%
    select(Clone = Parent.Tree.Number, Latitude, Longitude)

orchloc <- read.csv('../lodgepole_climate/locations/site_coord_elev.csv', stringsAsFactors = FALSE, header = TRUE)

pcontorta <- st_read("maps/SpeciesDistribution/pinucont.shp") %>%
    st_make_valid() %>%
    st_set_crs(4326) %>%
    mutate(Distribution = case_when(CODE == 0 ~ FALSE,
                            CODE == 1 ~ TRUE))

bboxdist <- st_bbox(pcontorta)

baset <- st_transform(base, crs = 4326)

parents <- st_as_sf(parentdat, coords = c("Longitude", "Latitude"),
                    crs = 4326, agr = "constant")
bboxparents <- st_bbox(parents)

sites <- st_as_sf(orchloc, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# https://docs.ropensci.org/rnaturalearth/ populated_places
citydat <- ne_download(scale = 'medium', type = 'populated_places', category = 'cultural', returnclass = "sf")
cities <- citydat %>% filter(NAME_EN== "Vancouver", ADM0NAME == "Canada") %>%
       st_transform(crs = 4326)


# if I give the parent lat long the same crs as map (the spu data), then they show up on the map way out in the pacific ocean. I have no idea why giving them a different crs seems to put them in the right place
#parents <- st_as_sf(parentdat, coords = c("Longitude", "Latitude"),
                    #crs = st_crs(map), agr = "constant")
bboxsites <- sites %>% filter(!id1 %in% c("Prince George Tree Improvement Station", "Kettle River Seed Orchards", "Sorrento Seed Orchard")) %>%
    st_bbox()

# base map
basemap <- ggplot(data = baset) +
    geom_sf() +
    #geom_sf(data = map, aes(fill = SPU, colour = SPU)) +
    #scale_fill_viridis_d(option = "cividis", alpha = 0.5) +
    #scale_colour_viridis_d(option = "cividis", alpha = 0.5) +
    #geom_sf(data = pcontorta, alpha = 0.5, fill = "darkolivegreen3") +
    geom_sf(data = pcontorta, alpha = 0.25, aes(fill = Distribution), show.legend = FALSE) +
    scale_fill_discrete(type = c("grey67", "darkolivegreen3")) +
    geom_sf(data = parents, shape = 3, colour = "black") +
    geom_sf(data = sites, shape = 23, size = 2, fill = "gold3", alpha = 0.7)

# plot the spus and parent tree locations with full range
basemap +
    #geom_sf(data = st_as_sfc(bboxparents), alpha = 0, colour = "grey20") +
    coord_sf(xlim = c(bboxdist$xmin, bboxdist$xmax),
             ylim = c(bboxdist$ymin, bboxdist$ymax)) +
   # ggtitle("Genotype sources and seed orchard sites", subtitle = "Extent of lodgepole pine range in light green") +
    #annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering)

ggsave("../flowering-cline/figures/datamap.png")
# zoom in on parents
basemap +
    geom_sf(data = st_as_sfc(bboxsites), alpha = 0, colour = "firebrick4") +
    ggtitle("Genotype sources and seed orchard sites", subtitle = "Extent of lodgepole pine range in light green") +
    geom_sf(data = cities, pch = 16) +
    geom_sf_text_repel(data = cities, aes(label = name_en), size = 2) +
    #annotation_scale(location = "bl", width_hint = 0.5)
    #annotation_north_arrow(location = "bl", which_north = "true",
                          # pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                           #style = north_arrow_fancy_orienteering)
    coord_sf(xlim = c(bboxparents$xmin - 1, bboxparents$xmax + 1),
         ylim = c(bboxparents$ymin - 1, bboxparents$ymax + 1))

# zoom in on clustered sites
basemap +
    coord_sf(xlim = c(bboxsites$xmin-0.25, bboxsites$xmax+0.25),
             ylim = c(bboxsites$ymin-0.05, bboxsites$ymax+0.05)) +
    ggtitle("Genotype sources and seed orchard sites", subtitle = "Extent of lodgepole pine range in light green") +
    annotation_scale(location = "bl", width_hint = 0.5)
    #annotation_north_arrow(location = "bl", which_north = "true",
                           # pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                           # style = north_arrow_fancy_orienteering)

# INTERSECTIONS
# identify the SPU each parent tree was sourced from
# if I use map and parent with crs as is (map = espg:3005, parent espg:4326), then
# Error: Problem with `mutate()` column `intersection`.
# ℹ `intersection = as.integer(st_intersects(geometry, map))`.
# x st_crs(x) == st_crs(y) is not TRUE

# map2 <- st_transform(map, crs = 4326) %>%
#     st_make_valid()
#
#
# pnts <- parents %>% mutate(
#     intersection = as.integer(st_intersects(geometry, map2))
#     , area = if_else(is.na(intersection), '', map2$SPU[intersection])
# )

# 37 clones are not assigned to an SPU for some reason. I can try to find the closest feature instead for these

#pnts2 <- pnts %>% filter(is.na(intersection))

# This is too slow and crashes my computer
# closest <- list()
# for(i in seq_len(nrow(parents))){
#     closest[[i]] <- map2[which.min(
#         st_distance(map2, parents[i,])),]
# }

