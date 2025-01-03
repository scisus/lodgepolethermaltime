# predict flowering events
# posterior predictive includes individual observation uncertainty
# grand means ignore group specific effects
# conditional effects include group specific effects as well as the uncertainty of fixed coefficients and the uncertainty of variance parameters for groups

# libraries
library(purrr)
library(dplyr)
library(tidybayes)

# data

modells <- readRDS("objects/modells.rds")
alldatls <- readRDS("objects/datlist.rds")
sitedat <- read.csv("../lodgepole_climate/data/climateBC/climatebc_locs_Normal_1961_1990Y.csv") %>% filter(id == "site")

labdf <- readRDS("objects/labdf.rds")

n <- 6000 # when downsampling required

siteMAT <- sitedat %>%
  filter(id == "site") %>%
  select(Site, MAT, Elevation) %>%
  mutate(MAT = round(MAT, 1)) #DUPLICATED IN DOY TRANS

# orchards ############
## generic orchard ############
## at 100 points spanning full range of provenances in the data. Use fewer MAT points with length.out for smaller object.
neworchdat_avg <- expand.grid(MAT = seq(from = range(alldatls$fbdat$MAT)[1],
                                    to = range(alldatls$fbdat$MAT)[2], length.out = 100),
                          Year = "newyear",
                          Tree = "newtree",
                          Genotype = "newgenotype",
                          Site = "neworchard",
                          event = c("begin", "end"),
                          Sex = c("FEMALE", "MALE")) %>%
  split(list(.$event, .$Sex))

# posterior prediction #########
# for each site for the full range of provenances using an average site, year, genotype, and tree (using estimated gaussian prior to generate random effects). n draws, 95% HDPI ("generic predictions) #######

fpred_orch_avg <- purrr::map2(neworchdat_avg, modells,
                          .f = function(x,y) {add_predicted_draws(newdata = x,
                                                                  object = y,
                                                                  re_formula = NULL,
                                                                  allow_new_levels = TRUE,
                                                                  sample_new_levels = "gaussian",
                                                                  ndraws = n)}) %>%
  bind_rows()
saveRDS(fpred_orch_avg, file = "objects/fpred_orch_avg.rds")

fpred_orch_avg_summary <- fpred_orch_avg %>%
  group_by(MAT, Year, Tree, Genotype, Site, event, Sex) %>%
  median_hdci(.prediction)
saveRDS(fpred_orch_avg_summary, "objects/fpred_orch_avg_summary.rds")



## orchard specific predictions using full posterior ############

# shortsites <- c("PGTIS", "KettleRiver", "Sorrento", "Tolko", "PRT", "Vernon", "Kalamalka") # drop border and trench
# neworchdat <- expand.grid(MAT = seq(from = range(alldatls$fbdat$MAT)[1],
#                                     to = range(alldatls$fbdat$MAT)[2], length.out = 100),
#                          Year = "newyear",
#                          Tree = "newtree",
#                          Genotype = "newgenotype",
#                          Site = shortsites,
#                          event = c("begin", "end"),
#                          Sex = c("FEMALE", "MALE")) %>%
#   split(list(.$event, .$Sex))

# posterior prediction #########
# for each site for the full range of provenances using an average year, genotype, and tree (using estimated gaussian prior to generate random effects). 6000 draws, 95% HDPI #######

# fpred_orch <- purrr::map2(neworchdat, modells,
#                           .f = function(x,y) {add_predicted_draws(newdata = x,
#                                                                   object = y,
#                                                                   re_formula = NULL,
#                                                                   allow_new_levels = TRUE,
#                                                                   sample_new_levels = "gaussian",
#                                                                   ndraws = n)}) %>%
#   bind_rows()
# saveRDS(fpred_orch, file = "objects/fpred_orch.rds")
#
# fpred_orch_summary <- fpred_orch %>%
#   group_by(MAT, Year, Tree, Genotype, Site, event, Sex) %>%
#   median_hdci(.prediction) %>%
#   mutate(Site = forcats::fct_relevel(Site, shortsites))
# saveRDS(fpred_orch_summary, "objects/fpred_orch_summary.rds")



# predict the global grand means: average predicted outcome ignoring group-specific deviations in intercept or slope

# grand mean ####
# build a dataframe with one entry for each dataset - just Sex and event, no groups, and calculate the average predicted outcome ignoring group specific deviations and individual level variation.


## expectation for trees sourced from all sites ####
fepred_allsites <- purrr::map(modells, function(x) {
  add_epred_draws(newdata = select(siteMAT, Site, MAT), object = x, re_formula = NA)}) %>%
  bind_rows(.id = "model") %>%
  select(-.chain, -.iteration) %>%
  left_join(labdf)
saveRDS(fepred_allsites, file = "objects/fepred_allsites.rds")


## expectation for all trees (provenances) in dataset ####
## This is identical to the old object fepred
fepred_allprovs <- purrr::map2(alldatls, modells, function(x,y) {
  add_epred_draws(newdata = select(x, Sex, event, MAT) %>% distinct(), object = y, re_formula = NA)}) %>%
  bind_rows(.id = "model") %>%
  select(-.chain, -.iteration) %>%
  left_join(labdf)
saveRDS(fepred_allprovs, file = "objects/fepred_allprovs.rds")

## try to understand effect size ####
# Calculate the maximum and minimum of MAT
max_MAT <- max(fepred_allprovs$MAT, na.rm = TRUE)
min_MAT <- min(fepred_allprovs$MAT, na.rm = TRUE)

# Filter rows where MAT is the maximum or minimum, and add a new column
minmaxMAT_fepred <- fepred_allprovs %>%
  # keep only coldest and warmest provenances and label
  filter(MAT == max_MAT | MAT == min_MAT) %>%
  mutate(size = case_when(
    MAT == max_MAT ~ "max",
    MAT == min_MAT ~ "min"
  )) %>%
  # order model samples
  group_by(model, Sex, event, size) %>%
  arrange(Sex, event, size, .epred) %>%
  group_by(model, size) %>%
  mutate(order = 1:n()) %>%
  select(-.row, -.draw, -MAT) %>%
  pivot_wider(names_from = size, values_from = .epred) %>%
  # get differences
  mutate(differences = max - min) %>%
  group_by(Sex, event) %>%
  mean_hdci(differences)
minmaxMAT_fepred

## posterior predictive for all provs ####
## same as expectation, but including individual level variation
fpred <- purrr::map2(alldatls, modells, function(x,y) {
  add_predicted_draws(newdata = select(x, Sex, event, MAT) %>% distinct(), object = y, re_formula = NA)}) %>%
    bind_rows()
saveRDS(fpred, file = "objects/fpred.rds")



