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
  select(Clone, Provenance, Latitude, Longitude, Elevation, all_of(varcols))# %>%
  pivot_longer(!c(Clone, Provenance), names_to = ".variable", values_to = ".value") %>%
  mutate(Clone = as.character(Clone))

#spring climate variables
spclim <- read.csv("../phd/data/OrchardInfo/ParentTrees/locations_for_climatena_Normal_1961_1990MSY.csv") %>%
  rename(Clone = ID1, Provenance = ID2) %>%
  select(Clone, Provenance, Latitude, Longitude, Elevation, ends_with("sm")) %>%
  pivot_longer(!c(Clone, Provenance), names_to = ".variable", values_to = ".value") %>%
  mutate(Clone = as.character(Clone))

# zparentclim <- parentclim %>% mutate_at(varcols, ~(scale(.) %>% as.vector))  %>%
#   pivot_longer(!c(Clone, Provenance), names_to = ".variable", values_to = ".value") %>%
#   mutate(Clone = as.character(Clone))

histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
#phendat <- flowers::lodgepole_phenology_event %>%
 # mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one
#phenf <- prepare_data(phendat = phendat, clim = histclim, spu = spudat)
alldat <- readRDS("objects/alldat.rds") %>%
  unite(fullevent, event, Sex, remove = FALSE)

phenclim <- left_join(alldat, parentclim) %>%
  filter(! is.na(.value)) %>%
  rename(modelname = fullevent)

#note that not all climate variables have unique values for every parent
#
ns <- phenclim %>% group_by(.variable) %>%
  summarise(n=length(unique(.value)))

ns %>%
  mutate(.variable = fct_reorder(.variable, n)) %>%
ggplot(aes(y=.variable, x=n)) +
  geom_point() +
  ggtitle("Number of unique values per variable")

#drop clones without parents in spring climate
spclim <- left_join(alldat, spclim) %>%
  filter(! is.na(.value)) %>%
  rename(modelname = fullevent)


# create a dataframe of events and variables
modelplusvar <- purrr::cross_df(list(model_name = unique(phenclim$modelname), variable = unique(phenclim$.variable)))
modelplusvarsp <- purrr::cross_df(list(model_name = unique(spclim$modelname), variable = unique(spclim$.variable)))

corr_function <- function(model.abb, variable){

  dat <- filter(phenclim, modelname == model.abb, .variable == variable)
  corr <- cor(dat$sum_forcing, dat$.value)

  # metaout <- glance(model) %>%
  #   mutate(modelname = model.abb, variable = variable)
  #
  # return(list(pars = output, meta = metaout))
  return(list(model_name = model.abb, variable = variable, correlation = corr))
}

corr_functionsp <- function(model.abb, variable){

  dat <- filter(spclim, modelname == model.abb, .variable == variable)
  corr <- cor(dat$sum_forcing, dat$.value)

  return(list(model_name = model.abb, variable = variable, correlation = corr))
}

corr_df <- map2_df(.x = modelplusvar$model_name, .y=modelplusvar$variable, .f=corr_function)
corr_dfsp <- map2_df(.x = modelplusvarsp$model_name, .y=modelplusvarsp$variable, .f=corr_functionsp)

corr_df %>%
  rename(model = model_name) %>%
  #mutate(model=as.character(model)) %>%
  #filter(term == ".value") %>%
  #select(-term) %>%
  rename(term = variable, estimate = correlation) %>%
  arrange(estimate) %>%
  dwplot() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Pearson correlation") + ylab("") +
  ggtitle("Sum forcing vs. parent climate")

corr_dfsp %>%
  rename(model = model_name) %>%
  #mutate(model=as.character(model)) %>%
  #filter(term == ".value") %>%
  #select(-term) %>%
  rename(term = variable, estimate = correlation) %>%
  arrange(estimate) %>%
  dwplot() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Pearson correlation") + ylab("") +
  ggtitle("Sum forcing vs. parent climate spring")

# result_df <- map2_df(.x = modelplusvar$model_name, .y=modelplusvar$variable, .f=model_function)
#
# result_df$pars %>%
#   rename(model = event) %>%
#   filter(term == ".value") %>%
#   select(-term) %>%
#   rename(term = variable) %>%
#   filter(term %in% colnames(parentclim)) %>%
#   arrange(estimate) %>%
#   filter(term == "DD18_at") %>%
#   dwplot() +
#   theme_minimal() +
#   geom_vline(xintercept = 0, alpha=.5, linetype=2) +
#   theme(legend.position = 'top') +
#   xlab("Estimate") + ylab("")
#
# result_df$meta %>%
#   distinct() %>%
#   rename(model = event) %>%
#   arrange(r.squared) %>%
#   ggplot(aes(x = r.squared, y = variable, colour = model)) +
#   geom_point() +
#   theme_minimal() +
#   geom_vline(xintercept = 0, alpha=.5, linetype=2) +
#   theme(legend.position = 'top') +
#   xlab("Correlation") + ylab("") +
#   facet_wrap("model", dir = "v")
#
# correlations <- result_df$meta %>% distinct() %>% arrange(desc(r.squared), event)
# aic <- result_df$meta %>% distinct() %>% arrange(AIC)
# meancorr <- result_df$meta %>%
#   group_by(variable) %>%
#   summarise(meanrsquared = mean(r.squared)) %>%
#   arrange(desc(meanrsquared))
#
# effects <- result_df$pars %>% distinct() %>% arrange(desc(abs(estimate)), event)
# meaneff <- result_df$pars %>%
#   group_by(variable) %>%
#   summarise(meaneffect = mean(estimate)) %>%
#   arrange(desc(abs(meaneffect)))
#
# beginmeaneff <- result_df$pars %>%
#   filter(event %in% c("FEMALE_begin", "MALE_begin")) %>%
#   group_by(variable) %>%
#   summarise(meaneffect = mean(estimate)) %>%
#   arrange(desc(abs(meaneffect)))
#
# beginmeancorr <- result_df$meta %>%
#   filter(event %in% c("FEMALE_begin", "MALE_begin")) %>%
#   group_by(variable) %>%
#   summarise(meanrsquared = mean(r.squared)) %>%
#   arrange(desc(meanrsquared)) %>%
#   filter(meanrsquared > 1.5e-2)
#
# ggplot(filter(phenclim, .variable == "MAT"), aes(x=.value, y=sum_forcing)) +
#   geom_point()
#
# parentclim[, c(246:ncol(parentclim))] %>%
#   pairs()

