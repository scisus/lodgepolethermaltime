# this script reports the phenology model results

# depends ####
library(dplyr)
library(lubridate)
library(tidybayes)
library(forcats)

source('phenology_functions.R')


# globals ####
nsamp <- 2000 # how many samples from the posterior (full posterior is big and slow)
seed <- 1657


# models #####
modells <- list(fb = readRDS("female_begin.rds"),
                fe = readRDS("female_end.rds"),
                mb = readRDS("male_begin.rds"),
                me = readRDS("male_end.rds"))

# data ####
siteclim <- read.csv("../lodgepole_climate/processed/PCIC_all_seed_orchard_sites_adjusted.csv")
# histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
 # filter(forcing_type == "gdd")
provclim <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv")
#futclim <- read.csv("../lodgepole_climate/processed/future_daily_temps.csv")

# this section copied from modelmethods.R and required only for factor ordering. I think a single function that creates phenf would be useful - or writing out phenf and reading it back in?
library(flowers)
histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
phendat <- flowers::lodgepole_phenology_event %>%
  mutate(Tree = paste0(Orchard, Clone, X, Y)) # create a unique Tree identifier since original data doesn't always have one
phenf <- prepare_data(phendat, clim = histclim, spu = spudat)

# meta ####
spudat <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv", header = TRUE, stringsAsFactors = FALSE)

## factor ordering for nice plots ####
# order site, prov, and year levels by MAT
siteMAT <- siteclim %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Site) %>%
  summarise(MAT = mean(mean_temp_corrected)) %>%
  arrange(MAT)

sitefactororder <- siteMAT$Site

provMAT <- provclim %>% select(SPU_Number, MAT) %>%
  full_join(spudat) %>%
  filter(SPU_Name %in% unique(phenf$Provenance)) %>%
  select(MAT, SPU_Name) %>%
  distinct() %>%
  arrange(MAT)

provfactororder <- provMAT$SPU_Name %>%
  stringr::str_replace_all(pattern = "\\s", replacement = "\\.")  # format names to work with stan output

yearMAT <- siteclim %>%
  mutate(Year = lubridate::year(Date)) %>%
  right_join(data.frame(Year = as.numeric(unique(phenf$Year)))) %>%
  group_by(Year) %>%
  summarise(MAT = mean(mean_temp_corrected)) %>%
  arrange(MAT)

yearfactororder <- yearMAT$Year

# extract and summarise parameter values from the posterior

# means ####

labdf <- data.frame(Sex = c("FEMALE", "FEMALE", "MALE", "MALE"), event = c('begin', 'end', 'begin', 'end'), model = c('fb', 'fe', 'mb', 'me'))

means <- purrr::map(modells, gather_means_draws) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) # label the models for plotting

# plot of population means, maybe better as a table
ggplot(means, aes(y = fct_rev(event), x = .value, colour = Sex)) +
  stat_halfeye(position = "dodge") +
  scale_colour_viridis_d() +
  labs(title = "Population mean", caption = "2000 draws from the posterior") +
  ylab("") +
  xlab("GDD") +
  theme_dark(base_size = 18) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))


# variation ####

variation <- purrr::map(modells, gather_var_draws) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) %>%
  mutate(.variable = case_when(.variable != "sigma" ~ stringr::str_sub(.variable, 4, -12),
                               .variable == "sigma" ~ "sigma")) %>%
  mutate(.variable = factor(.variable)) %>%
  mutate(.variable = forcats::fct_relevel(.variable, "sigma", "Year", "Site", "Provenance", "Clone", "Tree"))

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

spyctorder <- unique(c(sitefactororder, provfactororder, yctorder))

offsets <- offsets_raw %>% cbind(varlevel) %>%
  ungroup() %>%
  mutate(factor = forcats::fct_relevel(factor, "Year", "Site", "Provenance", "Clone", "Tree")) %>%
  mutate(level = forcats::fct_relevel(level, spyctorder))

# slow
offsets_summary <- offsets %>%
  group_by(model, Sex, event, factor, level) %>%
  median_hdci(.value, .width = c(0.5, 0.89)) %>%
  ungroup()

siter <- filter(offsets, factor == "Site") %>%
  mutate(level = forcats::fct_relevel(level, sitefactororder))

provr <- filter(offsets, factor == "Provenance") %>%
  mutate(level = forcats::fct_relevel(level, provfactororder))

yearr <- filter(offsets, factor == "Year") %>%
  mutate(level = forcats::fct_relevel(level, as.character(yearfactororder)))



