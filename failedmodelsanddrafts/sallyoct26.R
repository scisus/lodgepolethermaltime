fb <- readRDS("female_begin.rds")
fe <- readRDS("female_end.rds")
mb <- readRDS("male_begin.rds")
me <- readRDS("male_end.rds")

summary(fb)
summary(fe)
summary(mb)
summary(me)

plot(conditional_effects(fb))
plot(conditional_effects(fe))
plot(conditional_effects(mb))
plot(conditional_effects(me))

parentdat <- read.csv('../phd/data/OrchardInfo/ParentTrees/ParentTreeExtractReport_ParentTrees_2014_05_15_13_38_17.csv')  %>%
  mutate(Latitude = Latitude.Degrees + Latitude.Minutes/60 + Latitude.Seconds/60^2,
         Longitude = -(Longitude.Degrees + Longitude.Minutes/60 + Longitude.Seconds/60^2))

orchdat <- select(phenf, Clone, Orchard, Generation) %>% distinct() %>%
  filter(Generation == "Advanced")

orchdat[!which(orchdat$Clone %in% parentdat$Parent.Tree.Number),]

filter(phenf, Clone %in% c())
