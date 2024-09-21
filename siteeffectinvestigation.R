siter <- readRDS("objects/siter.rds")
phenf <- readRDS("objects/phenf.rds")

countyrs <- phenf %>%
  select(Site, Year) %>%
  distinct() %>%
  group_by(Site) %>%
  summarise(nYears = length(Year))

sitersum <- siter %>%
  group_by(Sex, event, level) %>%
  summarise(median_hdci(.value), sd = sd(.value)) %>%
  rename(Site = level) %>%
  left_join(countyrs)

ggplot(sitersum, aes(x = nYears, y = y)) +
  geom_point() +
  facet_grid(Sex ~ event)

ggplot(sitersum, aes(x = sd, y = y)) +
  geom_point(alpha = 0.2) +
  facet_grid(Sex ~ event)

library(dplyr)
library(broom)


# Fit a linear model for each 'model' type
model_results <- sitersum %>%
  group_by(Sex, event) %>%
  do(model_summary = tidy(lm(.value ~ sd, data = .)))

# fb_model_summary <- model_results %>%
#   filter(model == "fb") %>%
#   pull(model_summary)
#
# # View summary of model 'fb'
# print(fb_model_summary)

slopes <- sitersum %>%
  group_by(Sex, event) %>%
  do(tidy(lm(y ~ sd, data = .))) %>%
  filter(term == "sd") %>%
  select(Sex, event, term, estimate)

#compare provenance range to ?? site effect?

length(unique(phenf$Genotype))
length(unique(phenf$MAT))

metrange <- phenf %>%
  select(Site, MAT) %>%
  distinct() %>%
  group_by(Site) %>%
  summarise(matrange = max(MAT) - min(MAT)) %>%
  arrange(matrange)

sitersum2 <- sitersum %>%
  left_join(metrange)

# does site effect depend on range of mat provs grown at that site?
ggplot(sitersum2, aes(x = matrange, y = y)) +
  geom_point() +
  facet_grid(Sex ~ event)
## no

# does sd of site effect depend on range of mat provs grown at that site?
ggplot(sitersum2, aes(x = matrange, y = sd, colour= Site)) +
  geom_point(size = 3) +
  facet_grid(Sex ~ event) +
  scale_colour_discrete_c4a_div("dark2")
## yes, the higher the mat range covered at a site, the lower the sd of the site effect estimate

model_results <- sitersum2 %>%
  group_by(Sex, event) %>%
  do({
    model <- lm(sd ~ matrange, data = .)
    tidy_model <- tidy(model)
    rsq <- summary(model)$r.squared
    tidy_model$rsq <- rsq
    tidy_model
  })

model_results

#does site effect sd depend on nyears of observation

ggplot(sitersum, aes(x = nYears, y = sd, colour= Site)) +
  geom_point(size = 3) +
  facet_grid(Sex ~ event) +
  scale_colour_discrete_c4a_div("dark2")
## yes, the higher the mat range covered at a site, the lower the sd of the site effect estimate

model_results <- sitersum %>%
  group_by(Sex, event) %>%
  do({
    model <- lm(sd ~ nYears, data = .)
    tidy_model <- tidy(model)
    rsq <- summary(model)$r.squared
    tidy_model$rsq <- rsq
    tidy_model
  })

model_results

