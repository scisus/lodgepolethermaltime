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
phenf <- readRDS("objects/phenf.rds")

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

retro_doy_uncensored <- filter(specific_doy_preds, prediction_type %in% c("retrodiction - uncensored")) %>%
  left_join(specific_doy_preds_temp)

# summarise uncensored doy retrodictions by start, end, and length

retro_doy_summary <- specific_doy_preds %>%
  filter(prediction_type == "retrodiction - uncensored") %>%
  group_by(Sex, Site, Year, Clone, Tree, event) %>%
  summarise(meandoy = mean(newdoycol), sddoy = sd(newdoycol)) %>%
  pivot_wider(names_from = event, values_from = c("meandoy", "sddoy")) %>%
  mutate(length_mean = meandoy_end - meandoy_begin, length_sd = sqrt(sddoy_begin^2 + sddoy_end^2))

saveRDS(retro_doy_summary, file = "objects/retro_doy_summary.rds")

## begin, end, length retrodictions ####
# are the mean begin, end, and length within the expected ranges?
# The exact flowering period is never observed because of censoring.
# Using the first and last observed flowering days we construct a the max begin and minimum end day and minimum flowering period length for the data.
# Using the last observed before flowering day and the first observed after flowering day, we construct a minimum begin day, maximum end day, and maximum flowering period length for the data. We expect model estimates to be between the min and max ranges for the observations.

bel_min <- phenf %>%
  filter(Event_Obs %in% c(2,3)) %>%
  mutate(event = case_when(Event_Obs == 2 ~ "begin",
                           Event_Obs == 3 ~ "end")) %>%
  select(-contains("censored"), -Source, -X, -Y, -bound, -mean_temp, -contains("forcing"), -Date, -contains("Event_"), -State) %>%
  group_by(Index, Year, Sex, Site, Orchard, Clone, Tree) %>%
  pivot_wider(names_from = event, values_from = DoY) %>%
  mutate(length_min = end - begin) %>%
  rename(begin_max = begin, end_min = end)

bel_max <- phenf %>%
  filter(Event_Obs %in% c(1,4)) %>%
  mutate(event = case_when(Event_Obs == 1 ~ "begin",
                           Event_Obs == 4 ~ "end")) %>%
  select(-contains("censored"), -Source, -X, -Y, -bound, -mean_temp, -contains("forcing"), -Date, -contains("Event_"), -State) %>%
  group_by(Index, Year, Sex, Site, Orchard, Clone, Tree) %>%
  pivot_wider(names_from = event, values_from = DoY) %>%
  mutate(length_max = end - begin) %>%
  rename(begin_min = begin, end_max = end)

bel <- full_join(bel_min, bel_max) %>%
  left_join(retro_doy_summary) %>%

  mutate(length_in_int = case_when(!is.na(length_min) & !is.na(length_max) ~ length_mean >= length_min & length_mean <= length_max,
                                          is.na(length_max) ~ length_mean > length_min,
                                          is.na(length_min) ~ length_mean < length_max),
         begin_in_int = case_when(!is.na(begin_min) & !is.na(begin_max) ~ meandoy_begin >= begin_min & meandoy_begin <= begin_max,
                                  is.na(begin_min) ~ meandoy_begin < begin_max),
         end_in_int = case_when(!is.na(end_min) & !is.na(end_max) ~ meandoy_end >= end_min & meandoy_end <= end_max,
                                is.na(end_max) ~ meandoy_end > end_min)) %>%
  select(Index, Sex, Year, Site, Orchard, Clone, Tree, contains("begin"), contains("end"), contains("length"))
# calculate sd interval for model estimates
sd <- 1
bel_uncertain <- bel %>%
  mutate(begin_min_mod = meandoy_begin - sd * sddoy_begin,
         begin_max_mod = meandoy_begin + sd * sddoy_begin,
         end_min_mod = meandoy_end - sd * sddoy_end,
         end_max_mod = meandoy_end + sd * sddoy_end)

bel_props <- bel %>%
  ungroup() %>%
  summarise(length_in_int = length(which(length_in_int == TRUE))/n(),
            begin_in_int = length(which(begin_in_int == TRUE))/n(),
            end_in_int = length(which(end_in_int == TRUE))/n())


#
do_intervals_overlap <- function(datmin, datmax, modmin, modmax) {
  # left/right censored data
  if (is.na(datmin) | is.na(datmax)) {
    # censored begin date
    if (is.na(datmin)) {
      overlap <- modmin <= datmax # overlap if model predicts potential start date before first observed flowering
    } #censored end date
    if (is.na(datmax)) {
      overlap <- modmax >= datmin # overlap if model predicts potential end date after last observed flowering
    }
  } else {
  # interval censored data
    overlap <- findInterval(datmin:datmax, modmin:modmax) %>% any()
  }
  return(overlap)
}


do_bel_overlap <- function(dat) {
  n <- nrow(dat)
  # begin
  begin_overlap <-c()
  end_overlap <- c()
  for (i in 1:n) {
    begin_overlap[i] <- do_intervals_overlap(dat$begin_min[i],
                                             dat$begin_max[i],
                                             dat$begin_min_mod[i],
                                             dat$begin_max_mod[i])
    end_overlap[i] <- do_intervals_overlap(dat$end_min[i],
                                           dat$end_max[i],
                                           dat$end_min_mod[i],
                                           dat$end_max_mod[i])

  }

  return(data.frame(begin_overlap = begin_overlap, end_overlap = end_overlap))
}

inint <- select(bel_uncertain, contains("in_int")) %>%
  bind_cols(do_bel_overlap(bel_uncertain))

inintprop <- inint %>% ungroup() %>%
  summarise_at(vars(contains("_")), mean)



# interval censored
foo <- findInterval(bel_uncertain$begin_min[1]:bel_uncertain$begin_max[1],
                    bel_uncertain$begin_min_mod[1]:bel_uncertain$begin_max_mod[1])
any(foo)

# end censoring begin
foo <- bel_uncertain$begin_min_mod[1] < bel_uncertain$begin_max[1]

# end censoring end
foo <- bel_uncertain$end_max_mod[1] > bel_uncertain$end_min[1]

# now do a median version of above
# first get DoY predictions for specific predictions & retrodictions (uncensored, fully crossed)
# filter simulations for only real sites and years, not new levels and simplify to only columns needed for matching
specificsimmed <- filter(allsim, prediction_type %in% c("retrodiction - uncensored", "prediction - full cross")) %>%
  ungroup() %>%
  select(Year, Site, .prediction) %>%
  distinct()



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


