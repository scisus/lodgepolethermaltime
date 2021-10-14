# Exploratory analysis of phenology and provenance variables
#

#

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(dotwhisker)

spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)


# drop all monthly and seasonal vars
provclim <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv")
varcols <- select(provclim, -SPU_Number, - Pl_SPU, -X_FREQ_) %>% colnames()

parentclim <- read.csv("../phd/data/OrchardInfo/ParentTrees/locations_for_climatena_Normal_1961_1990MSY.csv") %>%
  rename(Clone = ID1, Provenance = ID2) %>%
  select(Clone, Provenance, Latitude, Longitude, Elevation, all_of(varcols)) %>%
  pivot_longer(!c(Clone, Provenance), names_to = ".variable", values_to = ".value") %>%
  mutate(Clone = as.character(Clone))

# zparentclim <- parentclim %>% mutate_at(varcols, ~(scale(.) %>% as.vector))  %>%
#   pivot_longer(!c(Clone, Provenance), names_to = ".variable", values_to = ".value") %>%
#   mutate(Clone = as.character(Clone))

histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one
#phenf <- prepare_data(phendat = phendat, clim = histclim, spu = spudat)
alldat <- readRDS("objects/alldat.rds") %>%
  unite(fullevent, Sex, event, remove = FALSE) %>%
  mutate_at("sum_forcing", ~(scale(.) %>% as.vector))

phenclim <- left_join(alldat, parentclim) %>%
  filter(! is.na(.value))

#note that not all climate variables are equally good
ns <- phenclim %>% group_by(.variable) %>%
  summarise(n=length(unique(.value)))

ns %>%
  mutate(.variable = fct_reorder(.variable, n)) %>%
ggplot(aes(y=.variable, x=n)) +
  geom_point() +
  ggtitle("Number of unique values per variable")

model_function <- function(model.abb, variable){

  model <- lm(sum_forcing ~ .value + Clone + Site + Year, na.action = na.omit,
              data=phenclim %>% filter(fullevent == model.abb, .variable == variable))

  output <- tidy(model, conf.int = TRUE) %>%
    select(term, estimate, std.error, starts_with("conf")) %>%
    mutate(event = model.abb, variable = variable)

  metaout <- glance(model) %>%
    mutate(event = model.abb, variable = variable)

  return(list(pars = output, meta = metaout))
}

# create a dataframe of events and variables
modelplusvar <- purrr::cross_df(list(model_name = unique(phenclim$fullevent), variable = unique(phenclim$.variable)))
result_df <- map2_df(.x = modelplusvar$model_name, .y=modelplusvar$variable, .f=model_function)

result_df$pars %>%
  rename(model = event) %>%
  filter(term == ".value") %>%
  select(-term) %>%
  rename(term = variable) %>%
  filter(term %in% colnames(parentclim)) %>%
  arrange(estimate) %>%
  filter(term == "DD18_at") %>%
  dwplot() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Estimate") + ylab("")

result_df$meta %>%
  distinct() %>%
  rename(model = event) %>%
  arrange(r.squared) %>%
  ggplot(aes(x = r.squared, y = variable, colour = model)) +
  geom_point() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Correlation") + ylab("") +
  facet_wrap("model", dir = "v")

correlations <- result_df$meta %>% distinct() %>% arrange(desc(r.squared), event)
aic <- result_df$meta %>% distinct() %>% arrange(AIC)
meancorr <- result_df$meta %>%
  group_by(variable) %>%
  summarise(meanrsquared = mean(r.squared)) %>%
  arrange(desc(meanrsquared))

effects <- result_df$pars %>% distinct() %>% arrange(desc(abs(estimate)), event)
meaneff <- result_df$pars %>%
  group_by(variable) %>%
  summarise(meaneffect = mean(estimate)) %>%
  arrange(desc(abs(meaneffect)))

beginmeaneff <- result_df$pars %>%
  filter(event %in% c("FEMALE_begin", "MALE_begin")) %>%
  group_by(variable) %>%
  summarise(meaneffect = mean(estimate)) %>%
  arrange(desc(abs(meaneffect)))

beginmeancorr <- result_df$meta %>%
  filter(event %in% c("FEMALE_begin", "MALE_begin")) %>%
  group_by(variable) %>%
  summarise(meanrsquared = mean(r.squared)) %>%
  arrange(desc(meanrsquared)) %>%
  filter(meanrsquared > 1.5e-2)

ggplot(filter(phenclim, .variable == "MAT"), aes(x=.value, y=sum_forcing)) +
  geom_point()

parentclim[, c(246:ncol(parentclim))] %>%
  pairs()
