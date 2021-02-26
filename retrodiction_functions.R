# functions for retrodiction.R
# 

# prepare dataframes a and b for interval finding by splitting into lists and ensuring that the climate list (a) and the phenology list (b) contain information for the same sites and years. a and b must both have Site and Year columns
split_df_to_lists <- function(a, b) {
  lista <- split(a, f = list(a$Site, a$Year), drop = TRUE)
  listb <- split(b, f = list(b$Site, b$Year), drop = TRUE)
  
  # check that all sites and years in the phenology frame are in the climate frame
  assertthat::assert_that(all(names(listb) %in% names(lista))) # all entries in B must be in A
  
  # subset lista so it only contains Site x Year that also occur in B
  ainb <- lista[names(listb)] 
  
  assertthat::are_equal(names(ainb), names(listb)) 
  
  return(list(listainb = ainb, listb = listb))
}


# given dataframes adf (climate) and bdf (phenology) identify the day of year corresponding to reaching each sum_forcing bcol in df. adf must have a sum_forcing column identified by name acol and a DoY column and b must have a sum_forcing column identified with name bcol. name the new_day_col arg with a string
find_day_of_forcing <- function(adf, bdf, aforce, bforce) {
  
  # what row in a contains the interval for entries in b. Add 1 to the index because phenological events require the threshold to be reached. this introduces error, but is unavoidable in some direction.
  a_index <- findInterval(bdf[[bforce]], adf[[aforce]]) + 1
  
  
  # add a column to b for Day of Year and extract the correct Day of year from a using the index
  bdf$newdoycol <- adf$DoY[a_index] 
  
  # when sum_forcing in b is exactly identical to sum_forcing in b, a_index will be +1 day. Re-write those 
  identical_forcing_index <- which(bdf[[bforce]] %in% adf[[aforce]])
  bdf$newdoycol[identical_forcing_index] <- bdf$newdoycol[identical_forcing_index] - 1
  
  # (indirectly) test whether the correct day of year is being assigned to the correct forcing unit - for any site x year, sorting by sum_forcing_rep or newdoycol should produce the same ordering of newdoycol in bdf
  
  # order_by_sumforcing <- arrange(bdf, bforce, newdoycol)$newdoycol
  # order_by_newdoycol <- arrange(bdf, newdoycol)$newdoycol
  # 
  # assertthat::are_equal(order_by_sumforcing, order_by_newdoycol)
  
  return(bdf)
}


# find DoY from a climate dataset a corresponding to each sum_forcing in a phenology dataset b. Identify column names of forcing columns as strings aforce and bforce and the name of the new doy column as a string to newdoycol. Day of Year in the climate dataset must be DoY
forcing_to_doy <- function(a, b, aforce, bforce, newdoycolname) {
  # prepare dataframes for interval finding by splitting into lists
  splitdfs <- split_df_to_lists(a, b)
  
  df <- purrr::map2(splitdfs$listainb, splitdfs$listb, find_day_of_forcing, aforce = aforce, bforce = bforce) %>% # find DoY in A corresponding to each sum_forcing in B
    purrr::map_dfr(bind_rows) # combine into a single dataframe
  
  names(df)[which(names(df) == "newdoycol")] <- newdoycolname 
  
  return(df)
}

# build a dataframe of retrodictions for sum_forcing and doy with associate Site, Provenance, Clone and Year information. Requires model file with sum_forcing_rep parameter and data used in the model as well as daily temperature with sum_forcing for sites and years in the data.
retrodict <- function(modelfile, dat, climate) {
  fit <- readRDS(modelfile)
  
  fit %<>% recover_types(dat)
  
  rep <- fit %>%
    #tidybayes::spread_draws(`sum_forcing_rep.*`[i], regex=TRUE, n=n, seed=seed) %>% # y_ppc generated in stan model into tidy df
    tidybayes::spread_draws(`sum_forcing_rep.*`[i], regex=TRUE) %>% # y_ppc generated in stan model into tidy df
    dplyr::left_join(
      dplyr::select(dat, Site, Year, i) # add identifying information (Site, Year) from data for matching with climate
    ) 
  
  # convert forcing units to day of year
  rep <- forcing_to_doy(a = climate, b = data.frame(rep), aforce = "sum_forcing", bforce = "sum_forcing_rep", newdoycolname = "doy_rep") 
  
  retrodiction <- dplyr::left_join(dat,rep) # dataframe with observed and modeled sum_forcing and DoY
  
  return(retrodiction)
}

# calculate HPDIs for each group, determine Day of Year associated with HPDIs, and calculate whether group observations are within intervals for day and sum_forcing
intervalate <- function(retrodictions, climate, dat) {
  intervals <- retrodictions %>%
    group_by(i) %>%
    median_hdi(sum_forcing_rep, .width=c(0.50, 0.75, 0.90)) %>%
    rename(sum_forcing_rep_median = sum_forcing_rep) %>%
    full_join(dat) 
  
  # what DoY is associated with each forcing?
  intervals <- forcing_to_doy(a = climate, b = intervals, aforce = "sum_forcing", bforce = ".lower", newdoycolname = ".lower_doy") 
  intervals <- forcing_to_doy(a = climate, b = intervals, aforce = "sum_forcing", bforce = ".upper", newdoycolname = ".upper_doy") 
 # intervals$doy_rep_median <- forcing_to_doy(a = climate, b = intervals, aforce = "sum_forcing", bforce = "sum_forcing_rep_median") 
  
  # What proportion of observations are within the HDPIs?
  intervals <- intervals %>%
    dplyr::mutate(in_forcing_int = case_when(sum_forcing >= .lower & sum_forcing <= .upper ~ TRUE,
                                             sum_forcing < .lower | sum_forcing > .upper ~ FALSE),
                  in_doy_int = case_when(DoY >= .lower_doy & DoY <= .upper_doy ~ TRUE,
                                         DoY < .lower_doy | DoY > .upper_doy ~ FALSE))
  
  return(intervals)
}

# Create a table showing the proportion of observations in the modeled forcing interval and doy interval

retrotable <- function(intervaldf) {
  tab <- intervaldf %>%
    group_by(.width) %>%
    summarize(prop_in_forcing_int = sum(in_forcing_int)/n(), 
              prop_in_doy_int = sum(in_doy_int)/n()) %>%
    rename("HDPI width" = .width, Forcing = prop_in_forcing_int, "Day of Year" = prop_in_doy_int)
  
  return(tab) # PAPER
}

# functions for f statistics ########
# calculate f statistics for factors in a dataset - requires observations ("y") and a factor ("factor_id") that are columns in a dataframe "df"
calc_f_stat <- function(df, y, factor_id) {
  
  y <- df[[y]]
  factor_id <- df[[factor_id]]
  
  assertthat::are_equal(length(y), length(factor_id))
  
  groupmeans <- tapply(y, factor_id, mean)
  assertthat::assert_that(!any(is.na(groupmeans)), msg = "One of the group means is NA - something is wrong")
  
  groupsds <- tapply(y, factor_id, sd)
  if (any(is.na(groupsds))) { print("One or more of the group sds is NA - check your sample sizes") } # no means are NA
  
  grandmean <- mean(y)
  n_group <- tapply(y, factor_id, length)
  k_group <- length(n_group)
  n_tot <- length(y)
  
  between <- sum(n_group * (groupmeans - grandmean)^2) / (k_group - 1)
  within <- sum(groupsds^2 * (n_group - 1)) / (n_tot - k_group)
  
  fstat <- between/within
  return(fstat)
}

# calculate site, provenance, and year f_statistic for a Site, Provenance, Year, and Clone factors in dataset (df) with observations (y)
calc_f_facs <- function(df, y) {
  fsite <- calc_f_stat(df, y, "Site")
  fprov <- calc_f_stat(df, y, "Provenance")
  fyear <- calc_f_stat(df, y, "Year")
  
  # sd is infinite when only one observation for a level exists. This is a problem for the fstat calculation for clones. so limit clone test to those clones with more than one observation.
  clone_multiples <- df %>%
    dplyr::group_by(Clone) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::filter(count > 1) %>%
    dplyr::select(Clone)
  
  dfclone <- dplyr::filter(df, Clone %in% clone_multiples$Clone)
  fclone <- calc_f_stat(dfclone, y, "Clone") #sd is infinite where clone only observed once and fstat cannot be calculated for this factor
  
  fstat <- data.frame(Site = fsite, Provenance = fprov, Year = fyear, Clone = fclone)
  return(fstat)
}

# MCMC draws from stan produce estimates for the entire dataset at each iteration. For each draw of a dataset within a model, calculate the fstatistic factors Site, Provenance, and Year

calc_f_draws <- function(splitlist, y) {
  
  fstat_frame <-  splitlist %>% 
    purrr::map(.f = calc_f_facs, y = y) %>%
    dplyr::bind_rows(.id = ".draw")
  
  return(fstat_frame)
}

# calculate f_statistics on the observations for both sum forcing and doy and format data into a tidy dataframe
calculate_fstat_obs <- function(obs) {
  fstat_obs_forcing <- calc_f_facs(obs, "sum_forcing")
  fstat_obs_day <- calc_f_facs(obs, "DoY")
  
  fstat_obs <- rbind(fstat_obs_forcing, fstat_obs_day)
  fstat_obs$y <- c("Sum Forcing", "Day of Year")
  
  fstat_obs <- tidyr::pivot_longer(fstat_obs, cols = any_of(facs), names_to = "factors", values_to = "F_statistic")
  return(fstat_obs)
}

# calculate f_statistics on the model output for both sum forcing and doy and format data into a tidy dataframe
calculate_fstat_mod <- function(retrodictions) {
  retrosplit <- retrodictions %>% 
    split(.$.draw) 
  
  fstat_mod_forcing <- calc_f_draws(retrosplit, "sum_forcing_rep")
  
  fstat_mod_doy <- calc_f_draws(retrosplit, "doy_rep")
  
  fstat_forcing_long <- fstat_mod_forcing %>%
    tidyr::pivot_longer(cols = any_of(facs), names_to = "factors", values_to = "F_statistic") 
  fstat_forcing_long$y <- "Sum Forcing"
  
  fstat_doy_long <- fstat_mod_doy %>%
    tidyr::pivot_longer(cols = any_of(facs), names_to = "factors", values_to = "F_statistic") 
  fstat_doy_long$y <- "Day of Year"
  
  fstat_mod <- rbind(fstat_forcing_long, fstat_doy_long)
  
  return(fstat_mod)
}

# calculate the mean of each level of a factor (fac) in the observations and model output and plot
plot_level_means <- function(retrodictions, observations, fac, y, y_rep) {
  
  retro.means <- retrodictions %>%
    dplyr::group_by(.draw, {{fac}}) %>%
    dplyr::summarise("{{y}}" := mean({{y_rep}}))
  
  obs.means <- observations %>%
    dplyr::group_by({{fac}}) %>%
    dplyr::summarise("{{y}}" := mean({{y}}))
  
  plot <- ggplot(retro.means, aes(x = {{y}})) +
    geom_histogram(bins = 50) +
    facet_wrap(vars({{fac}}), scales = "free_y") +
    geom_vline(data = obs.means, aes(xintercept = {{y}})) +
    theme_bw() +
    ggtitle(label = paste("Mean of", substitute(fac), "factor levels"), subtitle = "data is vertical line, model is distribution")
  
  print(plot)
}