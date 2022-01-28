# retrodict and predict critical forcing for events

# depends
library(dplyr)
library(purrr)
library(tidybayes)
library(tidyr)

source('phenology_functions.R')

alldatls <- readRDS("objects/datlist.rds")
modells <- readRDS("objects/modells.rds") #1.5GB

# data
histclim <- read.csv("data/all_clim_PCIC.csv") %>% # site clim with forcing
  filter(forcing_type == "gdd")
futclim <- read.csv("../lodgepole_climate/processed/future_daily_temps.csv")

alldat <- alldatls %>% bind_rows()
saveRDS(alldat, file = "objects/alldat.rds")

# global

seed <- 738


# simulate forcing ####
#
# simulate 4 sets of data from the model - retrodictions, retrodictions accounting for censorship, predictions for 5 new sites/years with 10 clones per provenance in every site and year and 2 trees per clone (at different sites))
# using 200 instead of 2000 samples from the posterior so my computer doesn't fall over

allsim <- purrr::map2(alldatls, modells, function(x,y) {simulate_from_model(data = x, model = y,
                                                                            n_lct = c(5,10,2),
                                                                            nsamples = 200,
                                                                            seed = seed)}) %>%
  bind_rows()
saveRDS(allsim, file = "objects/allsim.rds")

# historical forcing -> day of year ####

# first get DoY predictions for specific predictions & retrodictions (uncensored, fully crossed)
# filter simulations for only real sites and years, not new levels and simplify to only columns needed for matching
specificsim <- filter(allsim, prediction_type %in% c("retrodiction - uncensored", "prediction - full cross")) %>%
  ungroup() %>%
  select(Year, Site, .prediction) %>%
  distinct()

specific_doy_preds_temp <- forcing_to_doy(filter(histclim, forcing_type == "gdd"), specificsim, aforce = "sum_forcing", bforce = ".prediction", newdoycolname = "newdoycol")

specific_doy_preds <- full_join(filter(allsim, prediction_type %in% c("retrodiction - uncensored", "prediction - full cross")), specific_doy_preds_temp)
rm(specific_doy_preds_temp)
saveRDS(specific_doy_preds, file = "objects/specific_doy_preds.rds")

# retrodict DoY using censored forcing estimates

censorsim <- filter(allsim, prediction_type == "retrodiction - censored") %>%
  ungroup() %>%
  select(Year, Site, .prediction) %>%
  distinct()

censor_doy_retro_temp <- forcing_to_doy(filter(histclim, forcing_type == "gdd"), censorsim, aforce = "sum_forcing", bforce = ".prediction", newdoycolname = "newdoycol")

censor_doy_retro <- full_join( filter(allsim, prediction_type == "retrodiction - censored"), censor_doy_retro_temp)

rm(censor_doy_retro_temp)
saveRDS(censor_doy_retro, file = "objects/censor_doy_retro.rds")

# now do a median version of above
# first get DoY predictions for specific predictions & retrodictions (uncensored, fully crossed)
# filter simulations for only real sites and years, not new levels and simplify to only columns needed for matching
specificsimmed <- filter(allsim, prediction_type %in% c("retrodiction - uncensored", "prediction - full cross")) %>%
  ungroup() %>%
  select(Year, Site, .prediction) %>%
  distinct()

specific_doy_preds_temp <- forcing_to_doy(filter(histclim, forcing_type == "gdd"), specificsim, aforce = "sum_forcing", bforce = ".prediction", newdoycolname = "newdoycol")

specific_doy_preds <- full_join(filter(allsim, prediction_type %in% c("retrodiction - uncensored", "prediction - full cross")), specific_doy_preds_temp)
rm(specific_doy_preds_temp)
saveRDS(specific_doy_preds, file = "objects/specific_doy_preds.rds")

# get DoY predictions for the general predictions - which means assigning all sites and years to each prediction

# extract heat sum predictions at new sites and years and provs, clones, trees. downsample - 30 draws per "row"/new obs
generalsim <- filter(allsim, prediction_type == "prediction - new levels") %>%
  ungroup() %>%
  select(Sex, event, .row, .draw, .prediction, prediction_type, Site, Year, Provenance, Clone, Tree) %>%
  group_by(Sex, event, .row, prediction_type, Site, Year, Provenance, Clone, Tree) %>%
  slice_sample(n = 30) %>%
  rename(new_Site = Site, new_Year = Year)

# assign each prediction to fully crossed site x climate dataset
climsites <- unique(histclim$Site)
climyears <- unique(histclim$Year)
generalcross <- tidyr::crossing(climsites, climyears, .prediction = unique( generalsim$.prediction) ) %>%
  dplyr::rename(Site = climsites, Year = climyears)

general_doy_preds_temp <- forcing_to_doy(filter(histclim, forcing_type == "gdd"), generalcross, aforce = "sum_forcing", bforce = ".prediction", newdoycolname = "newdoycol")

general_doy_preds <- full_join(generalsim, general_doy_preds_temp)
rm(general_doy_preds_temp)

# so slow
general_doy_preds_med <- general_doy_preds %>% group_by(Sex, event, .row, Site, Year) %>%
  summarise(median = median(newdoycol))

general_doy_preds_med_siteyearsex <- general_doy_preds %>% group_by(Sex, event, Site, Year) %>%
  point_interval(newdoycol, .width = c(0.5, 0.95), .point = median, .interval = hdci) %>%
  # change doy to dates for plotting
  mutate(Date = as.Date("2020-12-31") + newdoycol,
         .lowerdate = as.Date("2020-12-31") + .lower,
         .upperdate = as.Date("2020-12-31") + .upper)
saveRDS(general_doy_preds_med_siteyearsex, file = "objects/general_doy_preds_med_siteyearsex.rds")


#  future forcing -> DoY predictions ####

## calculate forcing in future climates #####
futclimf <- futclim %>%
  # calculate heatsum forcing with 5 degree threshold
  mutate(forcing = case_when(mean_temp_gcm >= 5 ~ mean_temp_gcm - 5,
                             mean_temp_gcm < 5 ~ 0) )%>%
  arrange(SSP, Site, Year, normal_period, DoY) %>%
  group_by(SSP, Site, Year, normal_period) %>%
  mutate(sum_forcing = cumsum(forcing)) %>%
  ungroup() %>% group_by(Site, Year, name) %>%
  group_split()


# split futclimf dataframe (group_split) by Site, Year, and name and then use purrr map to run find day of forcing function

## DoY predictions for future (general) #####

# select only columns necessary for matching to DoY in climate dataset
smallgs <- generalsim %>%
  ungroup() %>%
  select(.prediction)%>%
  arrange(.prediction)


doypredmatchfut <- purrr::map(futclimf, match_force_future, bdf = smallgs, aforce = "sum_forcing", bforce = ".prediction")  %>%
  bind_rows() %>%
  full_join(generalsim)


# calculate medians for each event at each site for each SSP and normal period
doypredmatchfut_medians <- doypredmatchfut %>% group_by(Sex, event, Site, SSP, normal_period, climate_forcing) %>%
  point_interval(newdoycol, .width = c(0.5, 0.95), .point = median, .interval = hdci) %>%
  mutate(Date = as.Date("2020-12-31") + newdoycol, .lowerdate = as.Date("2020-12-31") + .lower, .upperdate = as.Date("2020-12-31") + .upper)
saveRDS(doypredmatchfut_medians, "objects/doypredmatchfut_medians.rds")


