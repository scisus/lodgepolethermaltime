# Compare clone effect to source climate
# not actually very interesting since clone SPU isnt the actual provenance of the clone


library(dplyr)
library(tidyr)
library(broom)
library(dotwhisker)

# list climate variables
provclim <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv")
varcols <- select(provclim, -SPU_Number, - Pl_SPU, -X_FREQ_) %>% colnames()

# read in clone effects
cloner <- readRDS("objects/cloner.rds") %>%
  unite(modelname, event, Sex, remove = FALSE) %>%
  select(-model) %>%
  rename(Clone = level) %>%
  mutate(Clone = as.character(Clone)) #

clonermed <- cloner %>%
  group_by(Clone, modelname) %>%
  summarise(medianvalue = median(.value))

# read in parent climate
parentclim <- read.csv("../phd/data/OrchardInfo/ParentTrees/locations_for_climatena_Normal_1961_1990MSY.csv") %>%
  rename(Clone = ID1, SPZ = ID2) %>%
  select(Clone, SPZ, Latitude, Longitude, Elevation, all_of(varcols)) %>%
  mutate(Clone = as.character(Clone))
#pivot_longer(!c(Clone, SPZ), names_to = ".variable", values_to = ".value") %>%
 #

clonedat <- left_join(clonermed, parentclim) %>%
  tidyr::pivot_longer(cols = all_of(varcols), names_to = "climvar", values_to = "climval") %>%
  #filter(!Clone == "3172") %>%
  ungroup()

modelplusvar <- purrr::cross_df(list(model_name = unique(clonedat$modelname), variable = unique(clonedat$climvar)))

corr_function <- function(model.abb, variable){

  dat <- filter(clonedat, modelname == model.abb, climvar == variable) %>%
    filter(!is.na(climval))
  corr <- cor(dat$medianvalue, dat$climval)


  # output <- tidy(model, conf.int = TRUE) %>%
  #   select(term, estimate, std.error, starts_with("conf")) %>%
  #   mutate(modelname = model.abb, variable = variable)
  #
  # metaout <- glance(model) %>%
  #   mutate(modelname = model.abb, variable = variable)
  #
  # return(list(pars = output, meta = metaout))
  return(list(model_name = model.abb, variable = variable, correlation = corr))
}

corr_df_clone <- map2_df(.x = modelplusvar$model_name, .y=modelplusvar$variable, .f=corr_function)

corr_subset <- corr_df_clone %>% filter(abs(correlation) > 0.3)

corr_df_clone %>%
  rename(model = model_name) %>%
  #filter(term == ".value") %>%
  #select(-term) %>%
  rename(term = variable, estimate = correlation) %>%
  arrange(estimate) %>%
  dwplot() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Pearson correlation") + ylab("") +
  ggtitle("Clone effect vs prov climate")

corr_subset %>%
  rename(model = model_name) %>%
  #filter(term == ".value") %>%
  #select(-term) %>%
  rename(term = variable, estimate = correlation) %>%
  arrange(estimate) %>%
  dwplot() +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha=.5, linetype=2) +
  theme(legend.position = 'top') +
  xlab("Pearson correlation") + ylab("") +
  ggtitle("Clone effect vs prov climate")

climcors <- select(parentclim, all_of(unique(corr_subset$variable))) %>% cor()
climcors_sub <- select(parentclim, Latitude, Eref, MCMT, EMT, MAT) %>%cor()

climcors %>%
  abs() %>%
  apply(MARGIN = 2, FUN = mean) %>%
  sort()


# Clone 3172 is from latitude 58.53. This is 2.6 degrees further north than the next most northerly collection in the dataset.

# graph clone effects and climate ####
library(ggplot2)

ggplot(filter(clonedat, modelname == "begin_MALE"), aes(x = climval, y = medianvalue)) +
  geom_point(shape = 1, alpha = 0.6) +
  geom_smooth(method = "lm") +
  facet_wrap("climvar", scales = "free") +
  ggtitle("Male begin")

#without clone 3172
ggplot(filter(clonedat, modelname == "begin_MALE", Clone != 3172), aes(x = climval, y = medianvalue)) +
  geom_point(shape = 1, alpha = 0.6) +
  geom_smooth(method = "lm") +
  facet_wrap("climvar", scales = "free") +
  ggtitle("Male begin")

ggplot(filter(clonedat, modelname == "begin_FEMALE"), aes(x = climval, y = medianvalue)) +
  geom_point(shape = 1, alpha = 0.6) +
  geom_smooth(method = "lm") +
  facet_wrap("climvar", scales = "free") +
  ggtitle("Female begin")

# r2
lm_function <- corr_function <- function(model.abb, variable){

  dat <- filter(clonedat, modelname == model.abb, climvar == variable) %>%
    filter(!is.na(climval))
  model <- lm(dat$medianvalue ~ dat$climval)


  output <- tidy(model, conf.int = TRUE) %>%
    select(term, estimate, std.error, starts_with("conf")) %>%
    mutate(modelname = model.abb, variable = variable) %>%
    distinct()

  metaout <- glance(model) %>%
    mutate(modelname = model.abb, variable = variable) %>%
    distinct()

  return(list(pars = output, meta = metaout))
  #return(list(model_name = model.abb, variable = variable, correlation = corr))
}

lm_df_clone <- map2_df(.x = modelplusvar$model_name, .y = modelplusvar$variable, .f = lm_function)

r2 <- arrange(lm_df_clone$meta, desc(r.squared))

ggplot(r2, aes(x = r.squared, y = variable, colour = modelname)) +
  geom_point()

lm_df_clone$pars %>% filter(term == "dat$climval") %>%
  arrange(desc(abs(estimate)))

