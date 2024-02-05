ail# how many genotypes are from locations where multiple genotypes were sourced from?

library(dplyr)
library(flowers)

parents <- read.csv("../phd/data/OrchardInfo/ParentTrees/parents.csv") %>%
  select(Genotype = Parent.Tree.Number, Geographic.Location, Elevation, BGC.Zone.Code, Latitude, Longitude)

phendat <- flowers::lodgepole_phenology_event %>%
  left_join(parents) %>%
  mutate(Tree = paste0(Orchard,Genotype,X,Y))

ngeo <- phendat %>% select(Genotype, Geographic.Location) %>% distinct() %>%
  group_by(Geographic.Location) %>%
  summarise(ngenotype = n())

nrow(filter(ngeo, ngenotype > 1))
bgc <- table(phendat$BGC.Zone.Code)
geo <- table(phendat$Geographic.Location)

latlong <- phendat %>% select(Genotype, Latitude, Longitude, Elevation) %>%
  distinct() %>%
  group_by(Latitude, Longitude) %>%
  summarise(ngenotypes = n())

hist(latlong$ngenotypes)

dim(latlong) # 150
reps <- filter(latlong, ngenotypes > 1) #41
