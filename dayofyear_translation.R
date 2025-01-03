# convert forcing to day of year

library(purrr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidybayes)
library(tidyr)
library(forcats)


source('phenology_functions.R')
focalsites <- c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border") # warmest to coldest
shortsites <- c("PGTIS", "KettleRiver", "Sorrento", "Kalamalka")
seedorchardsites <- c("PGTIS", "KettleRiver", "Sorrento", "Tolko", "PRT", "Vernon", "Kalamalka")
factororder <- readRDS('objects/factororder.rds')

# daily forcing data ############

# daily "real" forcing
dailyforc <- read.csv("data/forcing/dailyforc_1945_2012.csv") %>%
  group_by(Site, Year) %>%
  mutate(index = cur_group_id()) %>% ungroup() %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site))
#orchard only historic, excluding vernon, tolko, prt and letting kal stand in for them
dailyforc_so <- dailyforc %>% filter(Site %in% seedorchardsites) %>%
  mutate(Site = forcats::fct_relevel(Site, seedorchardsites)) %>%
  arrange(Site)
# from temp mean at each site across 1945-2012
typical_year_forc <- read.csv("data/forcing/typical_year_forc.csv") %>%
  mutate(Date = as.Date(Date_scale)) %>% select(-Date_scale)
# averaged over 30 year periods
normal_forc <- read.csv("data/forcing/normalforc_1901-2100.csv") %>%
  group_by(Site, period, scenario) %>%  # index
  mutate(index = cur_group_id()) %>% ungroup()

#fepred_allprovs <- readRDS("objects/fepred_allprovs.rds")  ## expectation for observed trees (sources)
fepred_allsites <- readRDS("objects/fepred_allsites.rds")  %>% ## expectation for trees sourced from all sites - no downsampling
  ungroup() %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site))
fpred_orch <- readRDS("objects/fpred_orch.rds") %>% #posterior prediction for each site for the full range of provenances using an average year, genotype, and tree (using estimated gaussian prior to generate random effects). 6000 draws
  ungroup() %>%
  select(-.row, -.draw, -.chain, -.iteration) %>%
  mutate(Site = forcats::fct_relevel(Site, seedorchardsites)) %>%
  arrange(Site)
factororder <- readRDS("objects/factororder.rds")
sitedat <- read.csv("../lodgepole_climate/data/climateBC/climatebc_locs_Normal_1961_1990Y.csv")

# for each site, generate a timeseries of mean temperatures and associated accumulated forcing that reflects the general pattern of temperatures throughout the year. Do this by averaging temperatures on each day between 1945 and 2011 at each site.
# alternate source for this data would be typical_ts.csv in lodgepole_climate project


siteMAT <- sitedat %>%
  filter(id == "site") %>%
  select(Site, MAT, Elevation) %>%
  mutate(MAT = round(MAT, 1))


# avg predicted DoY for flowering at seed orchard sites (my observations/retrodictions) ####
# in a typical year at all my sites (mean temp 1945-2012 to create sum_forcing), calculate average predicted DoY for flowering (excluding site effects)
# 6000 draws per site
# doy_typical <- map_dfr(split(typical_year_forc, f = list(typical_year_forc$Site), drop = TRUE),
#                        find_day_of_forcing, .id = ".id",
#                        bdf = fepred, aforce = "sum_forcing", bforce = ".epred") %>%
#   rename(Site = .id, DoY = newdoycol) %>%
#   ungroup() %>%
#   select(-.row, -.chain, -.iteration, -.draw) %>%
#   mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
#   left_join(select(typical_year_forc, Date, DoY) %>% distinct())
# saveRDS(doy_typical, "objects/doy_typical.rds")


# HOME vs AWAY ############
# typical year where trees grown only at their source ###########
# now consider doy expectations in a typical year for trees from each of the sites of interest grown at those sites of interest

## intercept only #######
intercepts <- readRDS("objects/intercepts.rds")
doy_typical_allsites_interceptonly <- map_dfr(split(typical_year_forc, f = list(typical_year_forc$Site), drop = TRUE),
                                              find_day_of_forcing, .id = ".id",
                                              bdf = intercepts, aforce = "sum_forcing", bforce = ".value") %>%
  rename(Site = .id, DoY = newdoycol) %>%
  ungroup() %>%
  select(-.chain, -.iteration, -.draw) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  left_join(select(typical_year_forc, Date, DoY) %>% distinct()) %>%
  mutate(provenance_effect = FALSE) %>%
  left_join(siteMAT )
  #filter(Site %in% focalsites)
saveRDS(doy_typical_allsites_interceptonly, "objects/doy_typical_allsites_interceptonly.rds")

# including MAT #######
doy_typical_allsites <- map_dfr(split(typical_year_forc, f = list(typical_year_forc$Site), drop = TRUE),
                       find_day_of_forcing, .id = ".id",
                       bdf = rename(fepred_allsites, Source = Site), aforce = "sum_forcing", bforce = ".epred") %>%
  rename(Site = .id, DoY = newdoycol) %>%
  ungroup() %>%
  select(-.row, -.draw) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  left_join(select(typical_year_forc, Date, DoY) %>% distinct()) %>%
  left_join(select(ungroup(intercepts), model, Sex, event) %>% distinct) %>% # add sex and event
  mutate(provenance_effect = TRUE)
 # filter(Site %in% focalsites)
saveRDS(doy_typical_allsites, "objects/doy_typical_allsites.rds")

# only when provenances are grown at home
doy_typical_home <- filter(doy_typical_allsites,
                           #Source %in% focalsites,
                          # Site %in% focalsites,
                           Site == Source) %>%
  full_join(doy_typical_allsites_interceptonly) %>%
  group_by(Site, MAT, Sex, event, provenance_effect) %>%
  median_hdci(DoY) %>%
  filter(.width == 0.95) %>%
  select(-starts_with(".")) %>%
  pivot_wider(names_from = provenance_effect, values_from = DoY) %>%
  rename(intercept = `FALSE`, DoY = `TRUE`)
saveRDS(doy_typical_home, "objects/doy_typical_home.rds")

# provenance effect pushes southern provenances to flower later and northern to flower earlier, shortening the overall flowering period from south to north - and increasing overlap between north and south

doy_typical_allsites_interceptonly_intermediate <- doy_typical_allsites_interceptonly %>%
  filter(Site == "PGTIS") %>%
  select(Sex, event, DoY) %>%
  rename(intercept = DoY) %>%
  group_by(Sex, event) %>%
  median_hdci(intercept) %>%
  filter(.width == 0.95) %>%
  select(-starts_with("."))

# now do all sources grown at a single site
doy_typical_all_at_PGTIS <- doy_typical_allsites %>%
  #filter(Source %in% focalsites) %>%
  filter(Site == "PGTIS") %>%
  group_by(Site, MAT, Sex, event) %>%
  median_hdci(DoY) %>%
  filter(.width == 0.95) %>%
  select(-starts_with(".")) %>%
  merge(doy_typical_allsites_interceptonly_intermediate)
saveRDS(doy_typical_all_at_PGTIS, "objects/doy_typical_all_at_PGTIS.rds")

 # When all sources are grown at the same Site (PGTIS), MAT effect reduces overlap


# year to year variation ####

## posterior prediction, 6000 draws, avg year, genotype, tree, site effects. 1945-2011. See comments on fpred_orch generation in predict.R####

# split climate data and forcing predictions into lists based on site
# dailyforc_so_list <- split(dailyforc_so, f = list(dailyforc_so$Site), drop = TRUE)
# fpred_orch_list <- split(fpred_orch, f = list(fpred_orch$Site), drop = TRUE)
#
# all(names(dailyforc_so_list) == names(fpred_orch_list)) # test that lists have sites in same order

# doy_annual_pp <- map2_dfr(.x = dailyforc_so_list, .y = fpred_orch_list, .f = find_day_of_forcing_mapper, bforce = ".prediction") %>%
#   rename(DoY = newdoycol, index = .id) %>%
#   mutate(index = as.numeric(index)) %>%
#   ungroup() %>%
#   select(-Year, -Tree, -Genotype) %>%
#   left_join(select(dailyforc_so, index, Site, Year) %>% distinct())
#
# #now summarise
# doy_annual_pp_sum <- doy_annual_pp %>%
#   group_by(MAT, Site, event, Sex, Year) %>%
#   median_hdci(DoY) %>%
#   ungroup() %>%
#   mutate(Site = forcats::fct_relevel(Site, seedorchardsites)) # correct to full sites
# doy_annual_pp_sum$MAT_label <- paste("MAT:", doy_annual_pp_sum$MAT)
# saveRDS(doy_annual_pp_sum, "objects/doy_annual_pp_sum.rds")

## no site posterior prediction, 6000 draws, avg year, genotype, tree, NO site effects. 1945-2011. See comments on fpred_orch generation in predict.R####


# split climate data into list based on site and year
dailyforc_list <- dailyforc %>%
  arrange(Site, Year, DoY) %>%
  split(f = list(.$Site, .$Year), drop = TRUE)
fpred_orch_avg <- readRDS('objects/fpred_orch_avg.rds') %>%
  filter(MAT %in% c(-0.7, 6.8)) %>%
  ungroup() %>%
  select( -Year, -Site, -Tree, -Genotype, -.chain, -.iteration)

# match forcing predictions in fpred_orch_avg to doy in dailyforc_list.
doy_annual_avg_pp <- map_dfr(dailyforc_list, .f = find_day_of_forcing,
                          .id = "index",
                          bdf = fpred_orch_avg,
                          aforce = "sum_forcing",
                          bforce = ".prediction") %>%
  rename(DoY = newdoycol)

# Use strsplit to split the .id column by the period (separate toooo slow)
split_id <- strsplit(doy_annual_avg_pp$index, "\\.")

# Create new Site and Year columns by extracting the split components
doy_annual_avg_pp$Site <- sapply(split_id, `[`, 1)
doy_annual_avg_pp$Year <- as.numeric(sapply(split_id, `[`, 2))

#now summarise
doy_annual_avg_pp_sum <- doy_annual_avg_pp %>%
  select(-index) %>%
  group_by(MAT, Site, Year, event, Sex) %>%
  median_hdci(DoY, .width = c(0.50, 0.95)) %>%
  ungroup() %>%
  mutate(Year = as.numeric(Year)) %>%
  #left_join(select(dailyforc, index, Site, Year), relationship = "many-to-many") %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site)) # order sites
doy_annual_avg_pp_sum$MAT_label <- paste("MAT:", doy_annual_avg_pp_sum$MAT)
saveRDS(doy_annual_avg_pp_sum, "objects/doy_annual_avg_pp_sum.rds")

# calculate year to year variancevariance
vardoy <- doy_annual_avg_pp %>%
  rename(provMAT = MAT) %>%
  group_by(Site, event, Sex, provMAT) %>%
  summarise(standarddev = sd(DoY)) %>%
  left_join(siteMAT)

ggplot(vardoy, aes(x = MAT, y = standarddev, colour = as.factor(provMAT))) +
  geom_point(size = 2, alpha = 0.7) +
  facet_grid(event ~ Sex) +
  xlab("Site MAT") +
  ylab("Standard deviation (GDD)") +
  theme_bw()

## expectation and no random effects for y2y var and ranking correlation####
fepred_allsites_ls <- split(fepred_allsites, f = list(fepred_allsites$Site), drop = TRUE)
dailyforc_ls <- split(dailyforc, f = list(dailyforc$Site), drop = TRUE)
all(names(dailyforc_ls) == names(fepred_allsites_ls))

doy_annual_exp <- map2_dfr(.x = dailyforc_ls, .y = fepred_allsites_ls, .f = find_day_of_forcing_mapper, bforce = ".epred") %>%
  rename(DoY = newdoycol, index = .id) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  left_join(select(dailyforc, index, Site, Year) %>% distinct())

### and summarize
doy_annual_exp_sum <- doy_annual_exp %>%
  group_by(MAT, Site, event, Sex, Year) %>%
  median_hdci(DoY) %>%
  ungroup() %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site)) # correct to full sites
doy_annual_exp_sum$MAT_label <- paste("MAT:", doy_annual_exp_sum$MAT)
saveRDS(doy_annual_exp_sum, "objects/doy_annual_exp_sum.rds")

# normal periods ########
normal_forc_focal <- normal_forc %>% filter(Site %in% focalsites)
fepred_allsites_focal <- fepred_allsites %>% filter(Site %in% focalsites) %>% ungroup()

climatelist_nff <- split(normal_forc_focal, f = list(normal_forc_focal$Site), drop = TRUE)
phenlist_af <- split(fepred_allsites_focal, f = list(fepred_allsites_focal$Site), drop = TRUE)

doy_normal <- map2_dfr(.x = climatelist_nff, .y = phenlist_af, .f = find_day_of_forcing_mapper) %>%
  rename(index = .id, DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  #select(-.row, -.draw) %>%
  select(-.row) %>%
  left_join(select(normal_forc_focal, index, Site, period, scenario) %>% distinct()) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))

saveRDS(doy_normal, 'objects/doy_normal.rds')

 # graph climate change normals ####

doy_normal_subset <- doy_normal %>%
  filter(#! scenario %in% c( "ssp370"),
         period %in% c("1951-1980", "1981-2010", "2011-2040", "2041-2070", "2071-2100"),
         Site %in% c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border"),
         scenario %in% c("historical", "ssp245", "ssp585")) %>%
  mutate(Date = lubridate::ymd("2023-12-31") + DoY)
saveRDS(doy_normal_subset, "objects/doy_normal_subset.rds")

# historical dates
# this is now in paper as historicalnormal
# doy_normal_subset %>%
#   filter(period %in% c('1951-1980')) %>%
#   group_by(Site, Sex, event, period) %>%
#   median_qi(DoY) %>%
#   mutate(DoY = as.Date(DoY - 1, origin = "2024-01-01"),
#          .lower = as.Date(.lower - 1, origin = "2024-01-01"),
#          .upper = as.Date(.upper -1, origin = "2024-01-01")) %>%
#   ungroup() %>%
#   arrange(DoY)

# difference between 1951-1980 and 1981-2010
# doy_normal_subset %>%
#   filter(period %in% c('1951-1980', '1981-2010')) %>%
#   group_by(Site, Sex, event, period, MAT) %>%
#   median_hdci(DoY) %>%
#   select(Site, Sex, event, period, DoY, MAT) %>%
#   pivot_wider(names_from = period, values_from = DoY) %>%
#   mutate(advancement = `1981-2010` - `1951-1980`) %>%
#   arrange(advancement)

# advancement between 1951-1980 and 2071-2100
# doy_normal_subset %>%
#   filter(period %in% c('1951-1980', '2071-2100')) %>%
#   select(-period) %>%
#   group_by(Site, Sex, event, scenario, MAT) %>%
#   median_hdci(DoY) %>%
#   select(Site, Sex, event, scenario, DoY, MAT) %>%
#   pivot_wider(names_from = scenario, values_from = DoY) %>%
#   mutate(ssp2_adv = historical - ssp245, ssp5_adv = historical - ssp585) %>%
#   arrange(ssp5_adv)

# doy_normal_subset %>%
#   select(.draw, Site, Sex, event, DoY, period, scenario) %>%
#   filter(period %in% c('1951-1980', '2071-2100'),
#     scenario %in% c("historical", "ssp245", "ssp585")) %>%
#   select(-period) %>%
#   pivot_wider(names_from = scenario, values_from = DoY) %>%
#   mutate(ssp2_adv = historical - ssp245, ssp5_adv = historical - ssp585) %>%
#   group_by(Site, Sex, event) %>%
#   summarize(
#     median_qi_ssp2 = median_qi(ssp2_adv, .width = 0.95), #quantile interval
#     median_qi_ssp5 = median_qi(ssp5_adv, .width = 0.95)
#   ) %>%
#   unnest_wider(median_qi_ssp2, names_sep = "_") %>%
#   unnest_wider(median_qi_ssp5, names_sep = "_") %>%
#   select(-contains(".width"), -contains(".point"), -contains(".interval")) %>%
#   rename_with(~ gsub("median_qi", "adv", .), starts_with("median_qi")) %>%
#   arrange(adv_ssp5_y)

## climate change uncertainty
# doy_normal_subset %>%
#   group_by(index, Site, event, Sex, period) %>%
#   median_qi(DoY, .width = c(.50, 0.95))
