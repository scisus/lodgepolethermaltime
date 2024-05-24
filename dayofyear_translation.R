# convert forcing to day of year

library(purrr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidybayes)
library(tidyr)


source('phenology_functions.R')
focalsites <- c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border")
shortsites <- c("PGTIS", "KettleRiver", "Sorrento", "Kalamalka")

# daily forcing data ############

# daily "real" forcing
dailyforc <- read.csv("data/forcing/dailyforc_1945_2012.csv") %>%
  group_by(Site, Year) %>%
  mutate(index = cur_group_id()) %>% ungroup()
#orchard only, excluding vernon, tolko, prt and letting kal stand in for them
dailyforc_ss <- dailyforc %>% filter(Site %in% shortsites)
# from temp mean at each site across 1945-2012
typical_year_forc <- read.csv("data/forcing/typical_year_forc.csv") %>%
  mutate(Date = as.Date(Date_scale)) %>% select(-Date_scale)
# averaged over 30 year periods
normal_forc <- read.csv("data/forcing/normalforc_1901-2100.csv") %>%
  group_by(Site, period, scenario) %>%  # index
  mutate(index = cur_group_id()) %>% ungroup()

fepred <- readRDS("objects/fepred.rds") ## expectation for observed trees (sources)
fepred_allsites <- readRDS("objects/fepred_allsites.rds") ## expectation for trees sourced from all sites
fpred_orch <- readRDS("objects/fpred_orch.rds") %>% #posterior prediction for each site for the full range of provenances using an average year, genotype, and tree (using estimated gaussian prior to generate random effects). 3000 draws
  ungroup() %>%
  select(-.row, -.draw, -.chain, -.iteration)
fpred_focalsites_landscape <- readRDS("objects/fpred_focalsites_landscape.rds") # simulate for 5 sites on the landscape. posterior prediction # for each site as a site on the landscape with trees sourced from that site only using an average year, genotype, and tree (using estimated gaussian prior to generate random effects). 3000 draws, 95% HDPI #######
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
# 9000 draws per site
doy_typical <- map_dfr(split(typical_year_forc, f = list(typical_year_forc$Site), drop = TRUE),
                       find_day_of_forcing, .id = ".id",
                       bdf = fepred, aforce = "sum_forcing", bforce = ".epred") %>%
  rename(Site = .id, DoY = newdoycol) %>%
  ungroup() %>%
  select(-.row, -.chain, -.iteration, -.draw) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  left_join(select(typical_year_forc, Date, DoY) %>% distinct())
saveRDS(doy_typical, "objects/doy_typical.rds")


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

# normal periods ########
# use fpred instead of fepred
normal_forc_focal <- normal_forc %>% filter(Site %in% focalsites)
fepred_allsites_focal <- fepred_allsites %>% filter(Site %in% focalsites) %>% ungroup()
fpred_focalsites_landscape <- readRDS('objects/fpred_focalsites_landscape.rds')

# doy_normal <- map_dfr(split(normal_forc_focal, f = list(normal_forc_focal$index), drop = TRUE),
#                       find_day_of_forcing, .id = ".id",
#                       bdf = fepred_allsites_focal, aforce = "sum_forcing", bforce = ".epred") %>%
#   rename(index = .id, DoY = newdoycol) %>%
#   mutate(index = as.numeric(index)) %>%
#   ungroup() %>%
#   select(-.row, -.draw) %>%
#   left_join(select(normal_forc_focal, index, Site, period, scenario), distinct()) #%>%
#   mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))

climate_nff <- normal_forc_focal %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  arrange(Site)
climatelist_nff <- split(climate_nff, f = list(climate_nff$Site), drop = TRUE)
phen_af <- fpred_focalsites_landscape %>%
  ungroup() %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site))) %>%
  arrange(Site) %>%
  select(MAT, Site, event, Sex, .row, .draw, .prediction)
phenlist_af <- split(phen_af, f = list(phen_af$Site), drop = TRUE)

names(climatelist_nff) == names(phenlist_af)
# doy_normal <- map2_dfr(.x = climatelist_nff, .y = phenlist_af, .f = find_day_of_forcing_mapper, bforce = ".prediction") %>%
#   rename(index = .id, DoY = newdoycol) %>%
#   mutate(index = as.numeric(index)) %>%
#   ungroup() %>%
#   select(-.row, -.draw) %>%
#   left_join(select(normal_forc_focal, index, Site, period, scenario) %>% distinct()) %>%
#   mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))

doy_normal <- map2_dfr(.x = climatelist_nff, .y = phenlist_af, .f = find_day_of_forcing_mapper, bforce = ".prediction") %>%
  rename(index = .id, DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  select(-.row, -.draw) %>%
  left_join(select(climate_nff, index, Site, period, scenario) %>% distinct()) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))
#saveRDS(doy_normal, 'objects/doy_normal.rds')

# graph climate change normals ####

doy_normal_plotting <- doy_normal %>%
  filter(period %in% c("1951-1980", "1981-2010", "2011-2040", "2041-2070", "2071-2100"),
    Site %in% c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border"),
    scenario %in% c("historical", "ssp245", "ssp585")) %>%
  mutate(Date = ymd("2023-12-31") + DoY) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, focalsites)))
doy_normal_plotting$MATlabel <- paste(doy_normal_plotting$Site, " (", doy_normal_plotting$MAT, "\u00B0C", ")", sep = "")
doy_normal_plotting <- doy_normal_plotting %>%
  arrange(desc(MAT)) %>%
  mutate(MATlabel = factor(MATlabel, levels = unique(MATlabel), ordered = TRUE))
saveRDS(doy_normal_plotting, "objects/doy_normal_plotting.rds")

doy_normal_summary <- doy_normal_plotting %>%
  group_by(Sex, event, Site, period, scenario) %>%
  mean_hdci(DoY, .width = .95) %>%
  ungroup() %>%
  arrange(DoY)

# calculate how much advancement occurs between the two historical normal periods (1951-1980 and 1981-2010) and between the earliest normal period and the end of the 21st century

historicaltemp <- doy_normal_summary %>%
  filter(scenario == "historical") %>%
  pivot_wider(names_from = period, values_from = DoY, id_cols = c(Sex, event, Site))

doy_normal_advance <- doy_normal_summary %>%
  filter(! scenario == "historical", period == "2071-2100") %>%
  pivot_wider(names_from = period, values_from = DoY, id_cols = c(Sex, event, Site, scenario)) %>%
  left_join(historicaltemp) %>%
  rename(endofcentury = `2071-2100`, endhist = `1981-2010`, starthist = `1951-1980`) %>%
  mutate(historicalchange = starthist - endhist, fullchange = starthist - endofcentury) %>%
  arrange(scenario, Site, Sex, event) %>%
  left_join(siteMAT)

ggplot(doy_normal_advance, aes(x = MAT, y = fullchange, colour = event)) +
  geom_point() +
  facet_grid(scenario ~ Sex)
# year to year variation ####

## posterior prediction, 2000 draws, avg year, genotype, tree. 1945-2011. See comments on fpred_orch generation in predict.R####
doy_annual_pp <- map_dfr(split(dailyforc_ss, f = list(dailyforc_ss$index), drop = TRUE),
                         find_day_of_forcing, .id = "index",
                         bdf = fpred_orch %>% ungroup() %>% select(-Year), aforce = "sum_forcing", bforce = ".prediction") %>%
  rename(DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  select(-Tree, -Genotype) %>%
  left_join(select(dailyforc_ss, index, Site, Year) %>% distinct())

doy_annual_pp_sum <- doy_annual_pp %>%
  group_by(MAT, Site, event, Sex, Year) %>%
  median_hdci(DoY) %>%
  ungroup() %>%
  mutate(Site = forcats::fct_relevel(Site, shortsites))
doy_annual_pp_sum$MAT_label <- paste("MAT:", doy_annual_pp_sum$MAT)
saveRDS(doy_annual_pp_sum, "objects/doy_annual_pp_sum.rds")

## posterior expectation, retrodictions ####
# 2000 draws per year 1945-2011 per site

fepred_allsites_downsampled <- fepred_allsites %>%
  group_by(model, Site, MAT, .row) %>%
  sample_n(size = 2000)

doy_annual <- map_dfr(split(dailyforc, f = list(dailyforc$index), drop = TRUE),
                      find_day_of_forcing, .id = "index",
                      bdf = fepred_allsites_downsampled, aforce = "sum_forcing", bforce = ".epred") %>%
  rename(DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  select(-.row, -.draw) %>%
  left_join(select(dailyforc, index, Site, Year) %>% distinct()) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))


# graph year to year variation ####

labdf <- readRDS("objects/labdf.rds")
doy_annual_plotting <- doy_annual %>%
  filter(Site %in% c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border")) %>%
  left_join(labdf) %>%
  group_by(Site, Year, Sex, event) %>%
  median_hdci(DoY) #slow
saveRDS(doy_annual_plotting, 'objects/doy_annual_plotting.rds')

dplot2 <- doy_annual_plotting %>%
  pivot_wider(values_from = c(DoY, .lower, .upper), names_from = event)
saveRDS(dplot2, "objects/dplot2.rds")


# variation
summary_doy_annual <- doy_annual %>%
  mutate(normal_period = case_when(Year >= 1951 & Year <= 1980 ~ "1951-1980",
                                   Year >= 1981 & Year <= 2010 ~ "1981-2010")) %>%
  filter(!is.na(normal_period)) %>%
  group_by(normal_period, Sex, event, Site) %>%
  summarise(median_forcing = median(.epred), median_DoY = median(DoY), sd_forcing = sd(.epred), sd_DoY = sd(DoY))
saveRDS(summary_doy_annual, "objects/summary_doy_annual.rds")




# contrast ####
# this contrast does not compare site effects - uses grand means to describe how different sites are on average. forcing the same for each site

# how much do the max sites differ from one another
sitediff <- doy_typical %>%
  group_by(Site, Sex, event) %>%
  summarise(med_doy = mean(DoY)) %>%
  pivot_wider(names_from = "Site", values_from = "med_doy") %>%
  mutate(contrast_PGTIS = PGTIS - Kalamalka)






