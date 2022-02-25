# Compare retrodictions to observations
#
library(dplyr)
library(purrr)
library(tidybayes)
library(tidyr)

# functions ####
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

# data ####

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
  select(-sum_forcing, -upper)


#flen <-

retrocomp <- fsim %>%
  mutate(inint_mean = dplyr::between(x = retro_mean, left = dat_min, right = dat_max)) #%>%
  #mutate(inint_onesd = do_intervals_overlap(datmin = dat_min, datmax = dat_max, modmin = retro_min, modmax = retro_max))
  # need to rewrite this line and probably function. might just be able to use the findInterval function now that my intervals are fully numeric. Not sure how it copes with infinity tho

