# calculate f statistics
# 
library(dplyr)

# read in retrodictions
retro.fb <- read.csv("retrodictions/retrofb.csv", header=TRUE)

# filter out obs
obs <- dplyr::select(retro.fb, i, sum_forcing, DoY, Site, Year, Provenance, Clone) %>% 
    distinct()

# calculate f statistic for observation factor
# 
apply(iris[,1:4],2,function(x) tapply(x,iris$Species,mean)) #group means
apply(obs[,2], 2, function(x) tapply(x, obs$Site, mean))

# This correctly calculates the fstatistic for the observations
groupmeans <- tapply(obs$sum_forcing, obs$Site, mean)
groupsds <- tapply(obs$sum_forcing, obs$Site, sd)
grandmean <- mean(obs$sum_forcing)
n_group <- tapply(obs$sum_forcing, obs$Site, length)
k_group <- length(n_group)
n_tot <- nrow(obs)
#####

between <- sum(n_group * (groupmeans - grandmean)^2) / (k_group - 1)
within <- sum(groupsds^2 * (n_group - 1)) / (n_tot - k_group)

fobs <- between/within
apply(iris[,1:4],2,mean) #grand means 


samplestats <- iris %>%
  group_by(Species) %>%
  summarise(sitemeans = mean(Sepal.Length), sitesd = sd(Sepal.Length), n = n())

grandmean <- mean(iris$Sepal.Length)

between_group <- sum(samplestats$n * (samplestats$sitemeans - grandmean)^2) / (nrow(samplestats) - 1) 

samplestats <- obs %>%
    group_by(Site) %>%
    summarise(sitemeans = mean(sum_forcing), sitesd = sd(sum_forcing), n = n())

grandmean <- mean(obs$sum_forcing)

between_group <- sum(samplestats$n * (samplestats$sitemeans - grandmean)^2) / (nrow(samplestats) - 1) #mean square group
within_group <- sum(samplestats$sitesd^2 * (samplestats$n - 1)) / (nrow(obs) - nrow(samplestats))

f_obs <- between_group/within_group    

# calculate f statistic for modeled factor

modelstats <- retro.fb %>%
  group_by(.draw, Site) %>%
  summarise(sitemeans = mean(sum_forcing_rep), sitesd = sd(sum_forcing_rep), n = n())

grandmeanmodel <- retro.fb %>%
  group_by(.draw) %>%
  summarize(grandmean = mean(sum_forcing_rep))

f_mod <- modelstats %>%
  left_join(grandmeanmodel) %>%
  mutate(sqdiffs = (n * (sitemeans - grandmean)^2), sqsds = sitesd^2 * (n-1)) %>%
  group_by(.draw) %>%
  summarise(between_group = sum(sqdiffs)/(n() - 1), within_group = sum(sqdiffs)/(nrow(obs) - n())) %>%
  mutate(f_mod = between_group/within_group)

ggplot(f_mod, aes(x=.draw, y=f_mod)) +
  geom_point()

ggplot(modelstats, aes(x=sitemeans)) +
  geom_histogram() + 
  facet_wrap("Site")

ggplot(modelstats, aes(x=sitesd)) +
  geom_histogram() +
  facet_wrap("Site")

ggplot(grandmeanmodel, aes(x = grandmean)) + 
  geom_histogram()
