# Run all phenology models

library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)

source('phenology_functions.R')

# Data prep ######

phendat <- flowers::lodgepole_phenology_event

phenf <- prepare_data(phendat)

ggplot(phenf, aes(x = sum_forcing_centered, color = Sex)) +
  stat_ecdf() +
  facet_grid(Event_Label ~ .) +
  ggtitle("Cumulative distribution of accummulated forcing for flowering events") +
  theme_bw()


fb <- readRDS("FEMALEbegin.rds")
mb <- readRDS("MALEbegin.rds")
fe <- readRDS("FEMALEend.rds")
me <- readRDS("MALEend.rds")
