# Compare retrodictions to observations
#
library(dplyr)
library(purrr)
library(tidybayes)
library(tidyr)

# functions ####
calc_prop_in_int <- function(x) {sum(x)/length(x)}

minmaxdat <- function(df, minormax) {
  if (minormax == "min") {
    events <- c(2,3)
    cols <- c("dat_max_begin", "dat_min_end", "dat_range_min")
  }
  if (minormax == "max") {
    events <- c(1,4)
    cols <- c("dat_min_begin", "dat_max_end", "dat_range_max")
  }

  stopifnot("argument minormax must be \"min\" or \"max\"" = minormax %in% c("min", "max"))

  mmdf <- df %>%
    filter(Event_Obs %in% events) %>%
    mutate(event = case_when(Event_Obs == events[1] ~ "begin",
                             Event_Obs == events[2] ~ "end")) %>%
    select(-contains("censored"), -Source, -X, -Y, -bound, -mean_temp, -contains("forcing"), -Date, -contains("Event_"), -State) %>%
    group_by(Index, Year, Sex, Site, Orchard, Clone, Tree) %>%
    pivot_wider(names_from = event, values_from = DoY) %>%
    mutate(length = end - begin)

  colnames(mmdf)[9:11] <- cols

  return(mmdf)
}

# determine the proportion of forcing retrodictions that match observations ####
# observations are ranges that the event occurred in and retrodictions are from models with uncertainty intervals

# forcing data ####

obsim <- readRDS(file = "objects/allsim.rds") %>%
  filter(prediction_type == "retrodiction - uncensored")

# what proportion of retrodiction are within the true range?
# not sure how best to do this. calculate a 68.2% HDPI and test overlap?
# maybe better to calculate summary from full distribution? using add_epred_draws?
#
alldatls <- readRDS("objects/datlist.rds")
modells <- readRDS("objects/modells.rds") #1.5GB

# this is a slow step. I'm using the full model to make retrodictions, not subsampling
fretro <- purrr::map2(alldatls, modells, function(x,y) {add_predicted_draws(newdata = x, object = y)}) %>%
  bind_rows()

fsim <- fretro %>%
  # summarise by observation
  group_by(Index, Sex, event, censored, sum_forcing, upper) %>%
  summarise(retro_mean = mean(.prediction), retro_sd = sd(.prediction)) %>%
  # calculate min and max of a 1 sigma interval around the mean
  mutate(retro_min = retro_mean - retro_sd, retro_max = retro_mean + retro_sd) %>%
  # identify min and max of range for observations
  mutate(dat_min = case_when(censored == "left" ~ 0,
                             censored == "right" ~ sum_forcing,
                             censored == "interval" ~ sum_forcing),
         dat_max = case_when(censored == "left" ~ sum_forcing,
                             censored == "right" ~ Inf,
                             censored == "interval" ~ upper)) %>%
  ungroup() %>%
  select(-sum_forcing, -upper, -censored)

flen <- fsim %>%
  select(-retro_min, -retro_max) %>%
  tidyr::pivot_wider(id_cols = c(Index, Sex), names_from = event, values_from = contains("_")) %>%
  mutate(dat_range_min = dat_min_end - dat_max_begin,
         dat_range_max = dat_max_end - dat_min_begin,
         retro_len_mean = retro_mean_end - retro_mean_begin,
         retro_len_sd = sqrt(retro_sd_end^2 + retro_sd_begin^2),
         retro_len_min = retro_len_mean - retro_len_sd,
         retro_len_max = retro_len_mean + retro_len_sd) %>%
  group_by(Index, Sex) %>%
  mutate(inint_mean_len = between(x = retro_len_mean, left = dat_range_min, right = dat_range_max),
         inint_onesd_len = any(findInterval(c(dat_range_min, dat_range_max), c(retro_len_min, retro_len_max))))

retrocomp <- fsim %>%
  group_by(Index, Sex, event) %>%
  # are begin and end mean retrodictions in the obs interval
  mutate(inint_mean = between(x = retro_mean, left = dat_min, right = dat_max)) %>%
  # do begin and end 1 sigma interval retrodictions overlap the obs interval
  mutate(inint_onesd = any(findInterval(c(dat_min,dat_max), c(retro_min,retro_max)))) %>%
  # join with length interval tests
  select(Index, Sex, event, contains("inint")) %>%
  pivot_wider(id_cols = c(Index, Sex), names_from = event, values_from = contains("inint")) %>%
  full_join(select(flen, Index, Sex, inint_mean_len, inint_onesd_len)) %>%
  ungroup() %>%
  group_by(Sex) %>%
  summarise_at(vars(contains("inint")), calc_prop_in_int)





