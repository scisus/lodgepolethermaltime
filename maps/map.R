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
library(terra)
library(elevatr)

# NAD83 / Canada Atlas Lambert 3978
# WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS 4326

# data ########
# country outlines
basedat <- ne_states(country = c("canada", "united states of america"), returnclass = "sf") %>%
    st_transform(basedat, crs = 3978)

# terrain

## Create a SpatialPointsDataFrame with the extent coordinates
extent_points <- st_as_sf(data.frame(x = c(-145, -65), y = c(45,70)),
                          coords = c("x", "y"),
                          crs = 4326)
extent_points_zoom <- st_as_sf(data.frame(x = c(-119.5, -119), y = c(50.1,50.4)),
                               coords = c("x", "y"),
                               crs = 4326)

## Download & format the DEM
# dem_proj <- get_elev_raster(extent_points, z = 3, clip = "bbox", verbose = FALSE) %>%
#     rast() %>%
#     project(st_as_text(st_crs(basedat)))

elevations_dat <- get_elev_raster(extent_points, z = 5, clip = "bbox", verbose = FALSE)
elevations_dat@data@names <- "elevation"


# Create a SpatRaster from the elevations_dat
elevations <- rast(elevations_dat) %>%
  terra::project(st_as_text(st_crs(basedat)))
elevations_df <- as.data.frame(elevations, xy = TRUE)
elevations_df$elevation[elevations_df$elevation<0] <- NA


# elevations <- elevations_df %>%
#   rast() %>%
#   project(st_as_text(st_crs(basedat)))

# dem_proj_highrez <- get_elev_raster(extent_points_zoom, z = 11, clip = "bbox", verbose = FALSE) %>%
#     rast() %>%
#     project(st_as_text(st_crs(basedat)))

# lodgepole pine distribution shapefile
pcontorta <- st_read("data/latifoliaDistribution/shapefiles/latifolia_distribution_prj.shp") %>%
    st_make_valid()

# genotype locations
clones <- select(flowers::lodgepole_phenology_event, Clone) %>% distinct()

parents <- read.csv('data/parents.csv') %>%
    filter(Parent.Tree.Number %in% clones$Clone) %>% # only include genotypes I have in my phenologydataset
    select(Clone = Parent.Tree.Number, Latitude, Longitude) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

# orchard locations
sites <- read.csv('../lodgepole_climate/locations/site_coord_elev.csv', stringsAsFactors = FALSE, header = TRUE) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

# bboxes ##########

## bounding box for distribution
bboxdist <- st_bbox(pcontorta)

# parent bounding box
bboxparents <- st_bbox(parents) %>%
    st_as_sfc() %>%
    st_transform(crs = 3978) %>%
    st_bbox()

# sites bounding boxes
bboxsites <- st_bbox(sites) %>%
    st_as_sfc() %>%
    st_transform(crs = 3978) %>%
    st_bbox()

bboxsitezoom <- sites %>%
    filter(Site %in% c("Kalamalka", "Vernon", "PRT", "Tolko")) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(crs = 3978) %>%
    st_bbox()


basemap <- ggplot(data = base) +
    geom_raster(data = elevations_df, aes(x = x, y = y, fill = elevation)) +
    scale_fill_gradientn(colours = grey.colors(10), na.value = "#FFFFFF") +
    geom_sf(data = pcontorta, alpha = 0.3, fill = "darkolivegreen3") +
    geom_sf(fill = NA) +
    annotation_north_arrow(location = "bl", which_north = "true",
                           pad_x = unit(0.05, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(bboxparents$xmin - 2e5, bboxparents$xmax + 5e5),
             ylim = c(bboxsites$ymin - 1e5, bboxsites$ymax + 3e5)) +
    theme(legend.position = "none") +
    ylab("") + xlab("")

print(basemap)

# plot the parent tree locations, zoomed in

pointmap <- basemap +
    geom_sf(data = sites, aes(color = orchard), size = 2) +
    geom_sf(data = parents, shape = 3, alpha = 0.8) +
    coord_sf(xlim = c(bboxparents$xmin, bboxparents$xmax),
             ylim = c(bboxparents$ymin, bboxparents$ymax + 5e4))

print(pointmap)
# zoom in on sites around kalamalka

# add mountains
# library(raster)
# dem.raster <- getData("SRTM", lat = 46.0146, lon = 9.344197, download = TRUE)
# https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/




pointmap +
  #  geom_raster(data = hillshade_highrez, aes(x = x, y = y, fill = hillshade_highrez), alpha = 0.5) +
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

