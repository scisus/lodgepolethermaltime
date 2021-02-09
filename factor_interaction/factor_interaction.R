library(flowers)
library(dplyr)

# read in data

phenbe <- filter_start_end() 

# extract female start

phenfb <- filter(phenbe, Sex == "FEMALE", DoY == First_RF)

# organize factors into a matrix

base_data <- tidybayes::compose_data(phenfb, .n_name=tidybayes::n_prefix(prefix="N"))
data$N_main_factors <- 4
data$N_main_factor_levels <- c(N_Year = length(unique(phenfb$Year)), N_Clone = length(unique(phenfb$Clone)), N_Provenance = length(unique(phenfb$Provenance)), N_Site = length(unique(phenfb$Site)))
data$main_factor_level_idx <- rbind(data$location_idx, data$method_idx, data$source_idx)
