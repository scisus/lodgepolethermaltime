
# owens 2005 ####
#
phenf <- readRDS('objects/phenf.rds')
dailyforc <- read.csv("data/forcing/dailyforc_1945_2012.csv") %>%
  group_by(Site, Year) %>%
  mutate(index = cur_group_id()) %>% ungroup() %>%
  filter(Year == "2000")

doy_annual_pp_sum <- readRDS('objects/doy_annual_pp_sum.rds') %>%
  filter(Year == 2000) %>%
  mutate(Date = as.Date(DoY - 1, origin = "1999-12-31"))
# PRT 2000 ####
fig1 <- phenf %>%
  filter(Site == "PRT", Year == "2000")

# I don't have this in my data, but pollen shed is from 5/10 to 5/25 and receptivity is from 5/15 to 5/28

dailyforc %>%
  filter(Year == 2000, Site == "PRT", Date %in% c("2000-05-10", "2000-05-28"))
# sum forcing 185-314

doy_annual_pp_sum %>%
  filter(Site == "PRT") %>%
  arrange(Date)
# May 6 to 28

# PGTIS 2000 ####
fig2 <- phenf %>%
  filter(Site == "PGTIS", Year == "2000", Event_Obs %in% c(2,3)) %>%
  group_by(Sex) %>%
  summarise(earliest = min(Date), latest = max(Date), minforc = min(sum_forcing), maxforc = max(sum_forcing))

# graph shows pollen on pollen monitor from 5/30 to 6/10 and receptive cones from 6/3 to 6/13 and my observations show both from June 1 to June 16 for pollen shed and through the 18th for receptivity.
#
dailyforc %>%
  filter(Year == 2000, Site == "PGTIS", Date %in% c("2000-05-30","2000-06-13"))
#sum forcing 152-253

doy_annual_pp_sum %>%
  filter(Site == "PGTIS") %>%
  arrange(Date)
# May 30 to June 19

# Kalamalka 2000 ####
fig3 <- phenf %>%
  filter(Site == "Kalamalka", Year == 2000)

# i don't have this in my data but pollen shed is from 5/7 to 5/22 and receptivity is from 5/8 to 5/21

dailyforc %>%
  filter(Year == 2000, Site == "Kalamalka", Date %in% c("2000-05-07", "2000-05-21"))
# sum forcing 185-284

doy_annual_pp_sum %>%
  filter(Site == "Kalamalka") %>%
  arrange(Date)
# May 3 to 18

intercepts_summary <- readRDS('objects/intercepts.rds') %>%
  group_by(Sex, event) %>%
  mean_hdci(.value)

genpred <- readRDS('objects/fpred_orch_avg.rds') %>%
  group_by(Sex, event) %>%
  mean_hdci(.prediction)

## nilsson comparison
nilsson <- read.csv('data/Nilsson1981/swedish-pollen-timeseries.csv')



