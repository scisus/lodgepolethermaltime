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
# library(ggsflabel) # folded into geom_sf_label() and geom_sf_text() https://github.com/yutannihilation/ggsflabel

# NAD83 / Canada Atlas Lambert 3978
# WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS 4326

#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

base <- ne_states(country = c("canada", "united states of america"), returnclass = "sf") %>%
    st_transform(base, crs = 3979)

clones <- select(flowers::lodgepole_phenology_event, Clone) %>% distinct()

parentdat <- read.csv('data/parents.csv') %>%
    filter(Parent.Tree.Number %in% clones$Clone) %>%
    select(Clone = Parent.Tree.Number, Latitude, Longitude)

orchloc <- read.csv('../lodgepole_climate/locations/site_coord_elev.csv', stringsAsFactors = FALSE, header = TRUE)

pcontorta <- st_read("data/latifoliaDistribution/shapefiles/latifolia_distribution_prj.shp") %>%
    st_make_valid()
   # st_set_crs(4326) %>%
    #mutate(Distribution = case_when(CODE == 0 ~ FALSE,
                        #    CODE == 1 ~ TRUE))

## bounding box for distribution
bboxdist <- st_bbox(pcontorta)



# parent (genotype source) coords
parents <- st_as_sf(parentdat, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

bb_ll = bb %>%
    st_as_sfc() %>%
    st_transform(crs = 4326) %>%
    st_bbox()

# parent bounding box
bboxparents <- st_bbox(parents) %>%
    st_as_sfc() %>%
    st_transform(crs = 3978) %>%
    st_bbox()

# site coords
sites <- st_as_sf(orchloc, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# https://docs.ropensci.org/rnaturalearth/ populated_places
citydat <- ne_download(scale = "large", type = 'populated_places', category = 'cultural', returnclass = "sf")
# cities <- citydat %>% filter(NAME_EN== "Vancouver", ADM0NAME == "Canada") %>%
#        st_transform(crs = 4326)
cities <- citydat %>% filter(ADM0NAME == "Canada", ADM1NAME == "British Columbia") %>%
    st_transform(crs = 4326)


bboxsitezoom <- sites %>%
    filter(Site %in% c("Kalamalka", "Vernon", "PRT", "Tolko")) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(crs = 3978) %>%
    st_bbox()

# base map with latifolia distribution
basemap <- ggplot(data = base) +
    geom_sf() +
    geom_sf(data = pcontorta, alpha = 0.5, fill = "darkolivegreen3") +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(bboxdist$xmin - 1e6, bboxdist$xmax + 1e6),
             ylim = c(bboxdist$ymin - 1e5, bboxdist$ymax + 1e5)) +
    ggtitle("latifolia distribution")

# plot the parent tree locations, zoomed in

pointmap <- basemap +
    geom_sf(data = sites, aes(shape = orchard), color = "darkgoldenrod4", size = 3) +
    geom_sf(data = parents, shape = 3, alpha = 0.8) +
    coord_sf(xlim = c(bboxparents$xmin, bboxparents$xmax),
             ylim = c(bboxparents$ymin, bboxparents$ymax + 5e4))


# zoom in on sites around kalamalka

# add mountains
# library(raster)
# dem.raster <- getData("SRTM", lat = 46.0146, lon = 9.344197, download = TRUE)
# https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/


# zoom in on vernon area plot the site locations

pointmap +
    geom_sf(data = cities) +
    coord_sf(xlim = c(bboxsitezoom$xmin, bboxsitezoom$xmax),
             ylim = c(bboxsitezoom$ymin, bboxsitezoom$ymax))


# basemap +
#     geom_sf(data = st_as_sfc(bboxsites), alpha = 0, colour = "firebrick4") +
#     ggtitle("Genotype sources and seed orchard sites", subtitle = "Extent of lodgepole pine range in light green") +
#     geom_sf(data = cities, pch = 16) +
#     geom_sf_text_repel(data = cities, aes(label = name_en), size = 2) +
#     #annotation_scale(location = "bl", width_hint = 0.5)
#     #annotation_north_arrow(location = "bl", which_north = "true",
#                           # pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
#                            #style = north_arrow_fancy_orienteering)
#     coord_sf(xlim = c(bboxparents$xmin - 1, bboxparents$xmax + 1),
#          ylim = c(bboxparents$ymin - 1, bboxparents$ymax + 1))
#
# # zoom in on clustered sites
# basemap +
#     coord_sf(xlim = c(bboxsites$xmin-0.25, bboxsites$xmax+0.25),
#              ylim = c(bboxsites$ymin-0.05, bboxsites$ymax+0.05)) +
#     ggtitle("Genotype sources and seed orchard sites", subtitle = "Extent of lodgepole pine range in light green") +
#     annotation_scale(location = "bl", width_hint = 0.5)
#     #annotation_north_arrow(location = "bl", which_north = "true",
#                            # pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
#                            # style = north_arrow_fancy_orienteering)
#
# # INTERSECTIONS
# # identify the SPU each parent tree was sourced from
# # if I use map and parent with crs as is (map = espg:3005, parent espg:4326), then
# # Error: Problem with `mutate()` column `intersection`.
# # ℹ `intersection = as.integer(st_intersects(geometry, map))`.
# # x st_crs(x) == st_crs(y) is not TRUE
#
# # map2 <- st_transform(map, crs = 4326) %>%
# #     st_make_valid()
# #
# #
# # pnts <- parents %>% mutate(
# #     intersection = as.integer(st_intersects(geometry, map2))
# #     , area = if_else(is.na(intersection), '', map2$SPU[intersection])
# # )
#
# # 37 clones are not assigned to an SPU for some reason. I can try to find the closest feature instead for these
#
# #pnts2 <- pnts %>% filter(is.na(intersection))
#
# # This is too slow and crashes my computer
# # closest <- list()
# # for(i in seq_len(nrow(parents))){
# #     closest[[i]] <- map2[which.min(
# #         st_distance(map2, parents[i,])),]
# # }

