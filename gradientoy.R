base = 50
provenanceMAT <- 1:10
novar <- base + 0*provenanceMAT
clinvar <- base + 1*provenanceMAT
countervar <- base + -1*provenanceMAT

thermaltime <- data.frame(provenanceMAT, novar, clinvar, countervar)

ltt <- thermaltime %>%
  pivot_longer(contains("var"), names_to = "type", values_to = "forcing")
ggplot(ltt, aes(x = provenanceMAT, y = forcing, colour = type)) +
  geom_line()

geograd <- data.frame(location = 1:10) %>%
  mutate(nograd = 0*location, positive = 1*location, negative = -1*location)

foo <- merge(geograd, ltt)

forcingnograd <- data.frame(day = 1:length(40:60), loc1 = c(40:60), loc2 = c(40:60), loc3 = c(40:60)) %>%
  pivot_longer(contains("loc"), names_to = "location", values_to = "forcing")
forcingpositive <- data.frame(day =1:length(40:60), loc1 = c(40:60), loc2 = c(45:65), loc3 = c(55:75)) %>%
  pivot_longer(contains("loc"), names_to = "location", values_to = "forcing")

forcingnegative <- data.frame(day = 1:length(40:60), loc1 = c(40:60), loc2 = c(50:70), loc3 = c(60:80)) %>%
  pivot_longer(contains("loc"), names_to = "location", values_to = "forcing")

nograd <- left_join(ltt, forcingnograd)
plusgrad <- left_join(ltt, forcingpositive)

ggplot(nograd, aes(x = location, y = day, colour = type)) +
  geom_point()

ggplot(plusgrad, aes(x = location, y = day, colour = type, group=location)) +
  #geom_line(alpha = 0.5) +
  geom_point() +
  facet_wrap("type")

# this assumes all provenances at all locations
