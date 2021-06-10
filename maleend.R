# male end

library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)

source('phenology_functions.R')

phendat <- flowers::lodgepole_phenology_event

model_phenology(sex = "MALE", event = "end", inits = lapply(1:4, function(id) list(sigma = 30 )), phendat = phendat)
