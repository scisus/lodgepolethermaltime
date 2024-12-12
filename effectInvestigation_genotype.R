# genotype effect investigation
# check the relationship between genotype effects estimated in the model and site MATs to make sure site climate/latitude isn't getting picked up in the genotype effect par

library(broom)
library(dplyr)

genotyper <- readRDS("objects/genotyper.rds") %>%
  rename(Genotype = level) %>%
  group_by(Sex, event, Genotype) %>%
  mean_hdci(.value)
phenf <- readRDS("objects/phenf.rds") %>%
  select(Genotype, MAT) %>%
  distinct()

geneffects <- left_join(genotyper, phenf)

ggplot(geneffects, aes(x = MAT, y = .value, group = MAT)) +
  geom_boxplot() +
  facet_grid(Sex ~ event)

genotypeVsitemat_model_results <- geneffects %>%
  group_by(Sex, event) %>%
  do({
    model <- lm(.value ~ MAT, data = .)
    tidy_model <- tidy(model)
    rsq <- summary(model)$r.squared
    tidy_model$rsq <- rsq
    tidy_model
  })

genotypeVsitemat_model_results

genotyper %>%
  mutate(.value = abs(.value)) %>%
  arrange(desc(.value))
