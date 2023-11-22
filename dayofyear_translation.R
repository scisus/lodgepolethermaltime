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
# daily forcing data

dailyforc <- read.csv("data/dailyforc_1945_2012.csv") %>% # daily "real" forcing
  group_by(Site, Year) %>%
  mutate(index = cur_group_id()) %>% ungroup()
dailyforc_ss <- dailyforc %>% filter(Site %in% shortsites) #orchard only, excluding vernon, tolko, prt and letting kal stand in for them
typical_year_forc <- read.csv("data/typical_year_forc.csv") %>% # from temp mean at each site across 1945-2012
  mutate(Date = as.Date(Date_scale)) %>% select(-Date_scale)
normal_forc <- read.csv("data/normalforc_1901-2100.csv") %>% # averaged over 30 year periods
  group_by(Site, period, scenario) %>%  # index
  mutate(index = cur_group_id()) %>% ungroup()

fepred <- readRDS("objects/fepred.rds") ## expectation for observed trees (sources)
fepred_allsites <- readRDS("objects/fepred_allsites.rds") ## expectation for trees sourced from all sites
fpred_orch <- readRDS("objects/fpred_orch.rds") %>% #posterior prediction for each site for the full range of provenances using an average year, clone, and tree (using estimated gaussian prior to generate random effects). 3000 draws
  ungroup() %>%
  select(-.row, -.draw, -.chain, -.iteration)
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
  left_join(siteMAT ) %>%
  filter(Site %in% focalsites)
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
saveRDS(doy_typical_allsites, "objects/doy_typical_allsites.rds")

# only when provenances are grown at home
doy_typical_home <- filter(doy_typical_allsites, Source %in% focalsites,
                           Site %in% focalsites,
                           Site == Source) %>%
  full_join(doy_typical_allsites_interceptonly) %>%
  group_by(Site, MAT, Sex, event, provenance_effect) %>%
  median_hdci(DoY) %>%
  filter(.width == 0.95) %>%
  select(-starts_with(".")) %>%
  pivot_wider(names_from = provenance_effect, values_from = DoY) %>%
  rename(intercept = `FALSE`, wMAT = `TRUE`)
saveRDS(doy_typical_home, "objects/doy_typical_home.rds")

library(ggalt)
#presentation
ggplot(doy_typical_home, aes(x = intercept, xend = wMAT, y=Sex, shape = Sex)) +
  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#0e668b",
    size = 3
  ) +
  facet_grid(MAT ~ event) +
  xlab("Day of Year") +
  ggtitle("Change in flowering day of year expectation with MAT effect", subtitle = "typical year, trees grown at home") +
  theme(legend.position = "top")
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
  filter(Site == "PGTIS") %>%
  group_by(Site, MAT, Sex, event) %>%
  median_hdci(DoY) %>%
  filter(.width == 0.95) %>%
  select(-starts_with(".")) %>%
  merge(doy_typical_allsites_interceptonly_intermediate)
saveRDS(doy_typical_all_at_PGTIS, "objects/doy_typical_all_at_PGTIS.rds")

ggplot(doy_typical_all_at_PGTIS, aes(x = intercept, xend = DoY, y=MAT, shape = Sex)) +
  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#0e668b",
    size = 4
  ) +
  facet_grid(Sex ~ event) +
  xlab("Day of Year") +
  ggtitle("Change in flowering day of year expectation with MAT effect", subtitle = "typical year, trees grown at PGTIS") +
  geom_vline(data = doy_typical_all_at_PGTIS, aes(xintercept = intercept)) +
  theme(legend.position = "top")
# When all sources are grown at the same Site (PGTIS), MAT effect reduces overlap

# normal periods ########
doy_normal <- map_dfr(split(normal_forc, f = list(normal_forc$index), drop = TRUE),
                      find_day_of_forcing, .id = ".id",
                      bdf = fepred_allsites %>% filter(Site %in% focalsites), aforce = "sum_forcing", bforce = ".epred") %>%
  rename(index = .id, DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  select(-.row, -.draw) %>%
  left_join(select(normal_forc, index, Site, period, scenario) %>% distinct()) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_relevel(Site, factororder$site)))
saveRDS(doy_normal, 'objects/doy_normal.rds')


# year to year variation ####

## posterior prediction, 2000 draws, avg year, clone, tree. 1945-2011. See comments on fpred_orch generation in predict.R####
doy_annual_pp <- map_dfr(split(dailyforc_ss, f = list(dailyforc_ss$index), drop = TRUE),
                         find_day_of_forcing, .id = "index",
                         bdf = fpred_orch %>% ungroup() %>% select(-Year), aforce = "sum_forcing", bforce = ".prediction") %>%
  rename(DoY = newdoycol) %>%
  mutate(index = as.numeric(index)) %>%
  ungroup() %>%
  select(-Tree, -Clone) %>%
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

doy_annual_plotting <- doy_annual %>%
  filter(Site %in% c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border")) %>%
  left_join(labdf) %>% #labdf in modelparams
  group_by(Site, Year, Sex, event) %>%
  median_hdci(DoY) #slow
saveRDS(doy_annual_plotting, 'objects/doy_annual_plotting.rds')

dplot2 <- doy_annual_plotting %>%
  pivot_wider(values_from = c(DoY, .lower, .upper), names_from = event)
saveRDS(dplot2, "objects/dplot2.rds")

ggplot(dplot2, aes(x = Year, ymin = .lower_begin, ymax = .upper_begin, fill = Sex)) +
  geom_ribbon(alpha = 0.5) +
  geom_ribbon(aes(x = Year, ymin = .lower_end, ymax = .upper_end, fill = Sex), alpha = 0.5) +
  geom_line(data=doy_annual_plotting, aes(x = Year, y = DoY, color = Sex, linetype = event), inherit.aes = FALSE) +
  facet_grid(Sex ~ Site) +
  labs(title = "Predicted flowering periods", subtitle = "posterior expectation, ribbons = uncertainty, lines = medians") +
  ylab("Day of Year") +
  scale_color_viridis_d(option = "B") +
  scale_fill_viridis_d(option = "B") +
  theme_dark(base_size = 18) +
  theme(legend.position = "bottom")
ggsave("../flowering-cline/figures/yearly_phenology.png", width = 14, height = 7, units = "in")

# variation
summary_doy_annual <- doy_annual %>%
  mutate(normal_period = case_when(Year >= 1951 & Year <= 1980 ~ "1951-1980",
                                   Year >= 1981 & Year <= 2010 ~ "1981-2010")) %>%
  filter(!is.na(normal_period)) %>%
  group_by(normal_period, Sex, event, Site) %>%
  summarise(median_forcing = median(.epred), median_DoY = median(DoY), sd_forcing = sd(.epred), sd_DoY = sd(DoY))

ggplot(summary_doy_annual, aes(x = Site, y = sd_DoY, color = normal_period)) +
  geom_point() +
  facet_grid(Sex ~ event) +
  labs(title = "Year-to-year variation in flowering phenology at 9 Sites", subtitle = "over two 30-year climate normal periods") +
  theme(legend.position = "bottom") +
  theme_bw()
ggsave("../flowering-cline/figures/year2yearvar.png", width = 10, height = 6, units = "in")


 # graph normals ####

doy_normal_plotting <- doy_normal %>%
  filter(#! scenario %in% c( "ssp370"),
         period %in% c("1951-1980", "1981-2010", "2011-2040", "2041-2070", "2071-2100"),
         Site %in% c("Kalamalka", "KettleRiver", "PGTIS", "Trench", "Border"),
         scenario %in% c("historical", "ssp245", "ssp585")) %>%
  mutate(Date = ymd("2023-12-31") + DoY)

ggplot(filter(doy_normal_plotting, event == "begin"), aes(x = scenario, y = DoY, colour = Site, shape = Sex)) +
  stat_pointinterval(position = "dodge") +
  stat_pointinterval(data = filter(doy_normal_plotting, event == "end"), position = "dodge") +
  #scale_y_date(date_breaks = "1 month", date_labels =  "%b") +
  facet_wrap("period", scales = "free_x", nrow = 1) +
  theme_bw() +
  theme(legend.position = "bottom")  +
  labs(title = "Flowering period expectation", subtitle = "1951-2100 for 4 Shared Socioeconomic Pathways") +
  xlab("Shared Socioeconomic Pathway") +
  ylab("Day of Year")
ggsave("../flowering-cline/figures/normal_predictions.png", width = 13, height = 5, units = "in")




# contrast ####
# this contrast does not compare site effects - uses grand means to describe how different sites are on average. forcing the same for each site

# how much do the max sites differ from one another
sitediff <- doy_typical %>%
  group_by(Site, Sex, event) %>%
  summarise(med_doy = mean(DoY)) %>%
  pivot_wider(names_from = "Site", values_from = "med_doy") %>%
  mutate(contrast_PGTIS = PGTIS - Kalamalka)






