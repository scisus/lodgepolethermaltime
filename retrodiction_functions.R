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
  rep$doy_rep <- forcing_to_doy(a = climate, b = data.frame(rep), aforce = "sum_forcing", bforce = "sum_forcing_rep", newdoycolname = "doy_rep") 
  
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