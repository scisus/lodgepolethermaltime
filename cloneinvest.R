# how many clones are from locations where multiple clones were sourced from?

library(dplyr)
library(flowers)

parents <- read.csv("../phd/data/OrchardInfo/ParentTrees/parents.csv") %>%
  select(Clone = Parent.Tree.Number, Geographic.Location, Elevation, BGC.Zone.Code, Latitude, Longitude)

phendat <- flowers::lodgepole_phenology_event %>%
  left_join(parents) %>%
  mutate(Tree = paste0(Orchard,Clone,X,Y))

ngeo <- phendat %>% select(Clone, Geographic.Location) %>% distinct() %>%
  group_by(Geographic.Location) %>%
  summarise(nclone = n())

nrow(filter(ngeo, nclone > 1))
bgc <- table(phendat$BGC.Zone.Code)
geo <- table(phendat$Geographic.Location)

latlong <- phendat %>% select(Clone, Latitude, Longitude, Elevation) %>%
  distinct() %>%
  group_by(Latitude, Longitude) %>%
  summarise(nclones = n())

hist(latlong$nclones)

dim(latlong) # 150
reps <- filter(latlong, nclones > 1) #41
