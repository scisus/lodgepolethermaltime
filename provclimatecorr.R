# Does the effect of provenance correlate with provenance climate factors

library(dplyr)
library(broom)
library(purrr)
library(dotwhisker)

provclim <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv")
provr <- readRDS("objects/provr.rds")
phenf <- readRDS ("objects/phenf.rds")

spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)

# calculate z scores for provenance so effect sizes are comparable
varcols <- select(provclim, -SPU_Number, - Pl_SPU, -X_FREQ_) %>% colnames()
# zprovclim <- provclim %>% mutate_at(c('Latitude', 'Elevation', 'MAT', 'MCMT', 'DD_0', 'DD5', 'NFFD', 'bFFP'), ~(scale(.) %>% as.vector))
zprovclim <- provclim %>% mutate_at(varcols, ~(scale(.) %>% as.vector))

prov <- zprovclim %>%
  full_join(spudat) %>%
  filter(SPU_Name %in% unique(phenf$Provenance)) %>%
  rename(level = SPU_Name) %>%
  mutate(level = stringr::str_replace_all(level, pattern = "\\s", replacement = "\\.")) %>%
  full_join(provr) %>%
  rename(modelname = model, provenance_effect = .value) %>%
  select(-.variable) %>%
  tidyr::pivot_longer(cols = all_of(varcols), names_to = ".variable", values_to = ".value")

model_function <- function(model.abb, variable){

  model <- lm(provenance_effect ~ .value,
              data=prov %>% filter(modelname == model.abb, .variable == variable))

  output <- tidy(model, conf.int = TRUE) %>%
    select(term, estimate, std.error, starts_with("conf")) %>%
    mutate(modelname = model.abb, variable = variable)

  metaout <- glance(model) %>%
    mutate(modelname = model.abb, variable = variable)

  return(list(pars = output, meta = metaout))
}

modelplusvar <- purrr::cross_df(list(model_name = unique(prov$modelname), variable = unique(prov$.variable)))
result_df <- map2_df(.x = modelplusvar$model_name, .y=modelplusvar$variable, .f=model_function)

result_df$pars %>%
  rename(model = modelname) %>%
  filter(term == ".value") %>%
  select(-term) %>%
  rename(term = variable) %>%
  arrange(estimate) %>%
  dwplot() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Estimate") + ylab("")

result_df$meta %>%
  distinct() %>%
  rename(model = modelname) %>%
  arrange(model, r.squared) %>%
  ggplot(aes(x = r.squared, y = variable, colour = model)) +
  geom_point() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Correlation") + ylab("") +
 facet_wrap("model", dir = "v")

correlations <- result_df$meta %>% distinct() %>% arrange(desc(r.squared), modelname)
meancorr <- result_df$meta %>%
  group_by(variable) %>%
  summarise(meanrsquared = mean(r.squared)) %>%
  arrange(desc(meanrsquared))

beginmeancorr <- result_df$meta %>%
  filter(modelname %in% c("fb", "mb")) %>%
  group_by(variable) %>%
  summarise(meanrsquared = mean(r.squared)) %>%
  arrange(desc(meanrsquared))

endmeancorr <- result_df$meta %>%
  filter(modelname %in% c("fe", "me")) %>%
  group_by(variable) %>%
  summarise(meanrsquared = mean(r.squared)) %>%
  arrange(desc(meanrsquared))

effects <- result_df$pars %>% distinct() %>% arrange(desc(abs(estimate)), modelname)
meancorr <- result_df$pars %>%
  group_by(variable) %>%
  summarise(meaneffect = mean(estimate)) %>%
  arrange(desc(abs(meaneffect)))

beginmeaneff <- result_df$pars %>%
  filter(modelname %in% c("fb", "mb")) %>%
  group_by(variable) %>%
  summarise(meaneffect = mean(estimate)) %>%
  arrange(desc(abs(meaneffect)))

endmeancorr <- result_df$pars %>%
  filter(modelname %in% c("fe", "me")) %>%
  group_by(variable) %>%
  summarise(meaneffect = mean(estimate)) %>%
  arrange(desc(abs(meaneffect)))

