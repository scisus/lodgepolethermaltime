# this script reports the phenology model results

# depends ####
library(dplyr)
library(purrr)
library(tidybayes)


source('phenology_functions.R')

factororder <- readRDS("objects/factororder.rds")

# globals ####
nsamp <- 2000 # how many samples from the posterior (full posterior is big and slow)
seed <- 738

# models #####
modells <- list(fb = readRDS("female_begin_gen.rds"),
                fe = readRDS("female_end_gen.rds"),
                mb = readRDS("male_begin_gen.rds"),
                me = readRDS("male_end_gen.rds"))
saveRDS(modells, "objects/modells.rds")

# data ####

# phenology data
phenf <- readRDS("objects/phenf.rds")

# extract and summarise parameter values from the posterior

# means ####

labdf <- data.frame(Sex = c("FEMALE", "FEMALE", "MALE", "MALE"), event = c('begin', 'end', 'begin', 'end'), model = c('fb', 'fe', 'mb', 'me'))

means <- purrr::map(modells, gather_means_draws) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) # label the models for plotting
saveRDS(means, file = "objects/means.rds")

meanssummary <- means %>%
  group_by(Sex, event) %>%
  median_hdci(.value)

# generation ####
#get_variables(modells$fb)
gens <- purrr::map(modells, gather_gen_draws) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) %>%
  rename(simo1 = `simo_moGeneration1[1]`, simo2 = `simo_moGeneration1[2]`, simo3 = `simo_moGeneration1[3]`)
saveRDS(gens, file = "objects/gens.rds")

gens %>%
  group_by(Sex, event) %>%
  median_hdci(bsp_moGeneration, simo1, simo2, simo3)

simos <- gens %>%
  select(-contains("bsp")) %>%
  pivot_longer(cols = contains("simo"), names_to = "simplex", values_to = "distance")
saveRDS(simos, file = "objects/simos.rds")

# variation ####

variation <- purrr::map(modells, gather_var_draws) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) #%>%
  mutate(.variable = case_when(.variable != "sigma" ~ stringr::str_sub(.variable, 4, -12),
                               .variable == "sigma" ~ "sigma")) %>%
  mutate(.variable = factor(.variable)) %>%
  mutate(.variable = forcats::fct_relevel(.variable, "sigma", "Year", "Site",  "Clone", "Tree"))
saveRDS(variation, file = "objects/variation.rds")

# offsets ####

offsets_raw <- purrr::map(modells, gather_offset_draws) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) #%>%

# turn brms .variable names into useful names (slow)
varlevel <- offsets_raw$.variable %>% stringr::str_split_fixed("[_\\[\\,]", n=4) %>% data.frame() %>%
  select("X2", "X3")
colnames(varlevel) <- c("factor", "level")

# order factors and factor levels
yearclonetree <- filter(varlevel, factor %in% c("Year", "Clone", "Tree")) %>% distinct()
yctorder <- sort(yearclonetree$level)

syctorder <- unique(c(factororder$site, yctorder))

offsets <- offsets_raw %>% cbind(varlevel) %>%
  ungroup() %>%
  mutate(factor = forcats::fct_relevel(factor, "Year", "Site", "Clone", "Tree")) %>%
  mutate(level = forcats::fct_relevel(level, syctorder))

# slow
offsets_summary <- offsets %>%
  group_by(model, Sex, event, factor, level) %>%
  median_hdci(.value, .width = c(0.5, 0.89)) %>%
  ungroup()
saveRDS(offsets_summary, file = "objects/offsets_summary.rds")

siter <- filter(offsets, factor == "Site") %>%
  mutate(level = forcats::fct_relevel(level, factororder$site))
saveRDS(siter, file = "objects/siter.rds")

cloner <- filter(offsets, factor == "Clone")
saveRDS(cloner, file = "objects/cloner.rds")

yearr <- filter(offsets, factor == "Year") %>%
  mutate(level = forcats::fct_relevel(level, as.character(factororder$year)))
saveRDS(yearr, file = "objects/yearr.rds")



