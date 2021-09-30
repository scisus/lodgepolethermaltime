# parent climate x phenology fishing

# depends #####
library(flowers)
library(dplyr)
library(brms)
#library(ggplot2)
#library(tidyr)
#library(tidybayes)
#library(forcats)
#library(ggbeeswarm)
#library(lubridate)

#theme_set(theme_dark())

source('phenology_functions.R')



# data ####

# climate
histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
provclim <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv") %>% # climate for provenances
  select(SPU_Number, Latitude, MAT, MCMT, MWMT)
parentclim <- read.csv("../phd/data/OrchardInfo/ParentTrees/locations_for_climatena_Normal_1961_1990MSY.csv") %>%
  rename(Clone = ID1, Provenance = ID2) #%>%
#mutate(Clone = as.character(Clone))

# phenology
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one

# meta
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  right_join(parentclim, by = c("SPU_Name" = "Provenance"))

## data preparation for phenology model ####
phenf <- prepare_data(phendat, clim = histclim, spu = spudat)
saveRDS(phenf, file = "objects/phenf.rds")

# now create a variable and a value column

# function
#
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

