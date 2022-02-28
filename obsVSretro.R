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
  summarise_at(vars(contains("inint")), calc_prop_in_int) %>%
  select(Sex, contains("begin"), contains("end"), contains("len"))
colnames(retrocomp) <- c("Sex", "Begin", "Begin_sd", "End", "End_sd", "Length", "Length_sd")

# determine the proportion of doy retrodictions that match observations ####
# are the mean begin, end, and length retrodictions within the expected ranges?
# The exact flowering period is never observed because of censoring.
# Using the first and last observed flowering days we construct the max begin and minimum end day and minimum flowering period length for the data.
# Using the last observed before flowering day and the first observed after flowering day, we construct a minimum begin day, maximum end day, and maximum flowering period length for the data. We expect model estimates to be between the min and max ranges for the observations.
#
# doy data ####
phenf <- readRDS("objects/phenf.rds")

bel_min <- minmaxdat(phenf, minormax = "min")
bel_max <- minmaxdat(phenf, minormax = "max")


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

bel_uncertain <- bel %>%
  mutate(begin_min_mod = meandoy_begin -sddoy_begin,
         begin_max_mod = meandoy_begin + sddoy_begin,
         end_min_mod = meandoy_end - sddoy_end,
         end_max_mod = meandoy_end + sddoy_end)

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

