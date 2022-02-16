# Compare retrodictions to observations

obsim <- readRDS(file = "objects/allsim.rds") %>%
  filter(prediction_type == "retrodiction - uncensored")

# what proportion of retrodiction are within the true range?
# not sure how best to do this. calculate a 68.2% HDPI and test overlap?
# maybe better to calculate summary from full distribution? using add_epred_draws?
