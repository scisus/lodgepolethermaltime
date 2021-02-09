library(flowers)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(rstan)

source('phenology_functions.R')

# read in data

phenbe <- filter_start_end() 

# extract female start

phenfb <- filter(phenbe, Sex == "FEMALE", DoY == First_RF)

# identify factors
factors <- c("Site", "Provenance", "Year")

# view factor counts
ggplot(phenfb, aes(x = Site, fill = Year)) +
  geom_bar(stat="count")

ggplot(phenfb, aes(x = Provenance, fill = Site)) +
  geom_bar(stat = "count")

ggplot(phenfb, aes(x = Year, fill = Provenance)) +
  geom_bar(stat = "count")

# clone is almost completely nested
ggplot(phenfb, aes(x = Clone, fill = Provenance)) +
  geom_bar(stat = "count")

phenfb %>%
  group_by(Clone) %>%
  summarise(atsites = length(unique(Site))) %>%
  filter(atsites > 1) %>%
  summarise(n())
# only 24 clones occur at 2 sites (PGTIS and Kalamalka)

phenfb %>%
  group_by(Clone) %>%
  summarise(inprov = length(unique(Provenance))) %>%
  filter(inprov > 1) %>%
  summarise(n())
# 37 clones are assigned to 2 provenances. 7 are assigned to 3 provenances, 2 to 4 provenances

# read in parent data
parents <- read.csv("../phd/data/OrchardInfo/ParentTrees/ParentTreeExtractReport_AreasOfUse_2014_05_15_13_38_17.csv")

phenfb[!phenfb$Clone %in% parents$Parent.Tree.Number,] # 5 clones aren't in parent tree index, most are

# organize factors into a matrix
# 
phenfb$factor_configs <- dplyr::group_by(phenfb, Site, Provenance, Year) %>% # enumerate factor configurations
  group_indices() %>%
  as.character()

# create a dataframe
factorsdf <- phenfb %>% 
  select(Site, Provenance, Year, factor_configs) %>%
  distinct() %>%
  mutate(Site = factor(Site), Provenance = factor(Provenance), Year = factor(Provenance)) 

base_data <- tidybayes::compose_data(phenfb, .n_name=tidybayes::n_prefix(prefix="N"))

data <- base_data
data$N_main_factors <- length(factors)
data$N_main_factor_levels <- c(base_data$N_Site, base_data$N_Provenance, base_data$N_Year)
data$main_factor_level_idx <- rbind(factorsdf$Site, factorsdf$Provenance, factorsdf$Year)

inter_data <-  NULL
inter_data$N <- base_data$N_factor_configs
inter_data$N_main_factors <- data$N_main_factors
inter_data$N_main_factor_levels <- data$N_main_factor_levels
inter_data$main_factor_level_idx <- data$main_factor_level_idx


#programmatically construct first and second order interactions
inter <- stan(file='factor_interaction/construct_inter_idx_general.stan',
              data=inter_data, iter=1, warmup=0, chains=1,
              algorithm="Fixed_param")

inter_output <- extract(inter)

data$N_inter1_factors <- inter_output$N_inter1_factors[1]
data$N_inter1_factor_levels <- array(inter_output$N_inter1_factor_levels[1,])
data$inter1_factor1_idx <- inter_output$inter1_factor1_idx[1,]
data$inter1_factor2_idx <- inter_output$inter1_factor2_idx[1,]
data$inter1_factor_level_idx <- inter_output$inter1_factor_level_idx[1,,]
dim(data$inter1_factor_level_idx) <- c(data$N_inter1_factors, data$N_factor_configs)

data$N_inter2_factors <- inter_output$N_inter2_factors[1]
data$N_inter2_factor_levels <- array(inter_output$N_inter2_factor_levels[1,])
data$inter2_factor1_idx <- inter_output$inter2_factor1_idx[1,]
data$inter2_factor2_idx <- inter_output$inter2_factor2_idx[1,]
data$inter2_factor3_idx <- inter_output$inter2_factor3_idx[1,]
data$inter2_factor_level_idx <- inter_output$inter2_factor_level_idx[1,,]
dim(data$inter2_factor_level_idx) <- c(data$N_inter2_factors, data$N_factor_configs)

# readable names
# 
main_factor_names <- c("Site", "Provenance", "Year")
inter1_factor_names <- c("Site/Provenance", "Site/Year", "Provenance/Year")
inter2_factor_names <- c("Site/Provenance/Year")

# pack all of the main and interaction factors into one-dimensional arrays to facilitate specifying the overlapping factor models in Stan.
# 
pack <- stan(file='factor_interaction/pack_idx.stan', data=data,
             iter=1, warmup=0, chains=1, algorithm="Fixed_param")

pack_output <- extract(pack)

data$N_all_main_factor_levels <- pack_output$N_all_main_factor_levels[1]
data$packed_main_factor_idx <- array(pack_output$packed_main_factor_idx[1,])
data$main_factor_start_idx <- array(pack_output$main_factor_start_idx[1,])
data$main_factor_end_idx <- array(pack_output$main_factor_end_idx[1,])

data$N_all_inter1_factor_levels <- pack_output$N_all_inter1_factor_levels[1]
data$packed_inter1_factor_idx <- array(pack_output$packed_inter1_factor_idx[1,])
data$inter1_factor_start_idx <- array(pack_output$inter1_factor_start_idx[1,])
data$inter1_factor_end_idx <- array(pack_output$inter1_factor_end_idx[1,])

data$N_all_inter2_factor_levels <- pack_output$N_all_inter2_factor_levels[1]
data$packed_inter2_factor_idx <- array(pack_output$packed_inter2_factor_idx[1,])
data$inter2_factor_start_idx <- array(pack_output$inter2_factor_start_idx[1,])
data$inter2_factor_end_idx <- array(pack_output$inter2_factor_end_idx[1,])
