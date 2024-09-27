library(dplyr)
library(purrr)
library(tidybayes)

# provenance and gdd data from Nilsson 1981
nilssongdd <- read.csv('data/Nilsson1981/swedish-pollen-timeseries.csv') %>%
  rename(provid = provenance)
nilssonprovs <- read.csv('data/Nilsson1981/swedish-sites-with-MATs.csv') %>%
  rename(provenance = Name, provid = This.study, MAT = MAT.1961.1990) %>%
  filter(Used.Ange == "X") %>%
  select(provenance, provid, MAT) %>%
  mutate(provid = as.character(provid), Site = "Central Sweden")

# provenance and date data from o'reilly and owens 1988

ooprovsdat <- read.csv('data/oreilly-and-owens-1988/oreilly-and-owens-1988-table-1.csv')
oophendat <- read.csv('data/oreilly-and-owens-1988/oreilly-and-owens-1988-table-2.csv')

# format o'reilly and owens data and add forcing information
dailyforc_oo <- read.csv("data/forcing/dailyforc_1945_2012.csv") %>%
  group_by(Site, Year) %>%
  mutate(index = cur_group_id()) %>% ungroup() %>%
  filter(Year == "1983", Site == 'PGTIS')

ooprovs <- ooprovsdat%>%
  rename(MAT = MAT.1961.1990, provenance = Location.description) %>%
 # mutate(provenance = gsub(".*\\((.*)\\).*", "\\1", Location.description)) %>%
  select(provenance, MAT)

# tree ages - 14, just reaching maturity in provenance trials at prince george

oophen <- oophendat %>%
  select(provenance = Provenance, starts_with("Seed"), starts_with("Pollen"), -ends_with("3.Date")) %>%
  pivot_longer(cols = contains("Date"), names_to = "event", values_to = "Date") %>%
  mutate(Sex = case_when(
    grepl("^Seed", event) ~ "FEMALE",
    grepl("^Pollen", event) ~ "MALE"),
    event = case_when(
      grepl("[26]", event) ~ "begin",
      grepl("[74]", event) ~ "end"),
    # Date to DoY
    Date_with_year = paste(Date, "1983"),
    Date_parsed = mdy(Date_with_year),
    DoY = yday(Date_parsed) ) %>%
  select(-starts_with("Date")) %>%
  left_join(ooprovs) %>%
  left_join(dailyforc_oo) # add forcing


modells <- readRDS("objects/modells.rds")

# alldatls <- readRDS("objects/datlist.rds")
# sitedat <- read.csv("../lodgepole_climate/data/climateBC/climatebc_locs_Normal_1961_1990Y.csv") %>% filter(id == "site")

# combine nilsson & oreilly & owens data and prep for model comparison
independentdat <- left_join(nilssongdd, nilssonprovs) %>%
  full_join(oophen %>%
              select(event, provenance, Year, Sex, sum_forcing, MAT) %>%
              mutate(Site = "Central BC")) %>%
  filter(!is.na(MAT), !is.na(sum_forcing)) %>%
  split(list(.$event, .$Sex))


# posterior prediction #
# ignoring random effects) #######

indpred <- purrr::map2(independentdat, modells,
                              .f = function(x,y) {add_predicted_draws(newdata = x,
                                                                      object = y,
                                                                      re_formula = NA, #NA ignore random effects, NULL include random effects
                                                                      ndraws = 6000)}) %>%
  bind_rows()
# expectation, ignoring random effects#
# nilssonepred <- purrr::map2(nilssondat, modells,
#                            .f = function(x,y) {add_epred_draws(newdata = x,
#                                                                    object = y,
#                                                                    re_formula = NA,
#                                                                    ndraws = 6000)}) %>%
#   bind_rows()


# saveRDS(fpred_orch_avg, file = "objects/fpred_orch_avg.rds")

indpredsummary <- indpred %>%
  group_by(MAT, Year, Site, provid, provenance, event, Sex, sum_forcing) %>%
  median_hdci(.prediction, .width = c(0.5, 0.95)) %>%
  mutate(ssp. = case_when(provid == 4846 ~ "contorta",
                          provid != 4846 | is.na(provid) ~ "latifolia"))


# nilssonepredsummary <- nilssonepred %>%
#   group_by(MAT, Year, Site, provenance, event, Sex, sum_forcing) %>%
#   median_hdci(.epred, .width = c(0.5, 0.95)) %>%
#   mutate(ssp. = case_when(provenance == 4846 ~ "contorta",
#                           provenance != 4846 ~ "latifolia"))

saveRDS(indpredsummary, "objects/indpredsummary.rds")


#MAT range -4.5 to 5.4

