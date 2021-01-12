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
  
  return(list(listainb = ainb, listb = listb))
}

# given dataframes a (climate) and b (phenology) identify the day of year in a corresponding to reaching each sum_forcing in b. a must have a sum_forcing column and a DoY column and b must have a sum_forcing column. name the new_day_col arg with a string
# find_day_of_forcing <- function(a, b, new_doy_col) {
#   assertthat::assert_that(isFALSE(unique(b$sum_forcing %in% a$sum_forcing))) # no sum_forcing values are exactly identical
#   
#   # what row in a contains the interval for entries in b. Add 1 to the index because phenological events require the threshold to be reached. this introduces error, but is unavoidable in some direction.
#   a_index <- findInterval(b$sum_forcing, a$sum_forcing) + 1
#   
#   # add a column to b for Day of Year and extract the correct Day of year from a using the index
#   b[new_doy_col] <- a$DoY[a_index] 
#   
#   return(b)
# }
# 
find_day_of_forcing <- function(a, b, new_doy_col) {
  
  # what row in a contains the interval for entries in b. Add 1 to the index because phenological events require the threshold to be reached. this introduces error, but is unavoidable in some direction.
  a_index <- findInterval(b$sum_forcing, a$sum_forcing) + 1
  
  # add a column to b for Day of Year and extract the correct Day of year from a using the index
  b[new_doy_col] <- a$DoY[a_index] 
  
  # when sum_forcing in b is exactly identical to sum_forcing in b, a_index will be +1 day. Re-write those 
  identical_forcing_index <- which(b$sum_forcing %in% a$sum_forcing)
  b[[new_doy_col]][identical_forcing_index] <- b[[new_doy_col]][identical_forcing_index] - 1
  
  return(b)
}

# find DoY from a climate dataset corresponding to each sum_forcing in a phenology dataset
forcing_to_doy <- function(a, b, new_doy_col) {
  # prepare dataframes for interval finding by splitting into lists
  splitdfs <- split_df_to_lists(a, b)
  
  df <- purrr::map2(splitdfs$listainb, splitdfs$listb, find_day_of_forcing, new_doy_col = new_doy_col) %>% # find DoY in A corresponding to each sum_forcing in B
    purrr::map_dfr(bind_rows) # combine into a single dataframe
  
  return(df)
}