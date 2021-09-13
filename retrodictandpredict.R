# retrodict and predict critical forcing for events

alldatls <- list(fbdat, fedat, mbdat, medat)
alldat <- rbind(fbdat, fedat, mbdat, medat)

# simulate forcing ####
# simulate 4 sets of data from the model - retrodictions, retrodictions accounting for censorship, predictions for 5 new sites/provenances/years with 10 clones per provenance in every site and year and 2 trees per clone (at different sites))
# using 200 instead of 2000 samples from the posterior so my computer doesn't fall over

allsim <- purrr::map2(alldatls, modells, function(x,y) {simulate_from_model(data = x, model = y,
                                                                            n_lct = c(5,10,2),
                                                                            nsamples = 200,
                                                                            seed = seed)}) %>%
  bind_rows()

ggplot(alldat, aes(x = sum_forcing, y = "observations" , colour = Sex)) +
  stat_dotsinterval( .width = c(0.5, 0.89), point_interval = median_qi) +
  stat_slab(data = allsim,
            aes(x = .prediction, y = prediction_type, group=.draw),
            .width = c(0.5, 0.89), point_interval = median_hdci,
            slab_color = "gray65", alpha = 1/10, fill = NA) +
  stat_pointinterval(data = allsim, aes(x = .prediction, y = prediction_type),
                     .width = c(0.5, 0.89), point_interval = median_hdci ) +
  # stat_dots(data = may15, aes(x = sum_forcing, y = "Forcing at May 15")) +
  theme_bw(base_size = 18) +
  facet_grid(event ~ Sex) +
  labs(title = "Modeled and observed flowering events",  caption = "200 samples from the posterior, 5 for fully crossed predictions") +
  xlab("GDD") +
  ylab("") +
  scale_colour_viridis_d() +
  theme(legend.position = "none")

# historical forcing -> day of year ####

# first get DoY predictions for specific predictions & retrodictions (uncensored, fully crossed)
# filter simulations for only real sites and years, not new levels and simplify to only columns needed for matching
specificsim <- filter(allsim, prediction_type %in% c("retrodiction - uncensored", "prediction - full cross")) %>%
  ungroup() %>%
  select(Year, Site, .prediction) %>%
  distinct()

specific_doy_preds_temp <- forcing_to_doy(filter(histclim, forcing_type == "gdd"), specificsim, aforce = "sum_forcing", bforce = ".prediction", newdoycolname = "newdoycol")

specific_doy_preds <- full_join(filter(allsim, prediction_type %in% c("retrodiction - uncensored", "prediction - full cross")), specific_doy_preds_temp)
rm(specific_doy_preds_temp)

# get DoY predictions for the general predictions - which means assigning all sites and years to each prediction

# extract heat sum predictions at new sites and years and provs, clones, trees. downsample - 30 draws per "row"/new obs
generalsim <- filter(allsim, prediction_type == "prediction - new levels") %>%
  ungroup() %>%
  select(Sex, event, .row, .draw, .prediction, prediction_type, Site, Year, Provenance, Clone, Tree) %>%
  group_by(Sex, event, .row, prediction_type, Site, Year, Provenance, Clone, Tree) %>%
  slice_sample(n = 30) %>%
  rename(new_Site = Site, new_Year = Year)

# assign each prediction to fully crossed site x climate dataset
climsites <- unique(histclim$Site)
climyears <- unique(histclim$Year)
generalcross <- tidyr::crossing(climsites, climyears, .prediction = unique( generalsim$.prediction) ) %>%
  dplyr::rename(Site = climsites, Year = climyears)

general_doy_preds_temp <- forcing_to_doy(filter(histclim, forcing_type == "gdd"), generalcross, aforce = "sum_forcing", bforce = ".prediction", newdoycolname = "newdoycol")

general_doy_preds <- full_join(generalsim, general_doy_preds_temp)
rm(general_doy_preds_temp)

# so slow
general_doy_preds_med <- general_doy_preds %>% group_by(Sex, event, .row, Site, Year) %>%
  summarise(median = median(newdoycol))

general_doy_preds_med_siteyearsex <- general_doy_preds %>% group_by(Sex, event, Site, Year) %>%
  point_interval(newdoycol, .width = c(0.5, 0.95), .point = median, .interval = hdci)

# time series lines with begin and end for one site a piece on each plot
ggplot(general_doy_preds_med_siteyearsex, aes(x=Year, y = newdoycol, linetype = Sex, colour = event)) +
  #geom_point() +
  geom_line() +
  scale_colour_viridis_d() +
  facet_wrap("Site")


# change doy to dates for plotting
general_doy_preds_med_siteyearsex <- general_doy_preds_med_siteyearsex %>%
  mutate(Date = as.Date("2020-12-31") + newdoycol, .lowerdate = as.Date("2020-12-31") + .lower, .upperdate = as.Date("2020-12-31") + .upper)

# Plot to compare time series
ggplot(general_doy_preds_med_siteyearsex, aes(y = Date, x = Year, colour = Sex, group = Year)) +
  geom_line() +
  facet_grid(Site ~ Sex) +
  #scale_colour_viridis_d(end = 0.9) +
  theme_dark(base_size = 18) +
  labs(title = "Flowering period from 1945-2012 at 7 sites", subtitle = "median start day of year to median end day of year", caption = "1500 forcing observations simulated  from 200 draws of the posterior with new factor levels \n and matched to forcing data for plotted sites and years. Daily temperature data from PCIC \nand adjusted using monthly climateNA") +
  theme(legend.position = "none") +
  scale_y_date(date_labels = "%b %e") +
  scale_colour_viridis_d()

#  future forcing -> DoY predictions ####

## calculate forcing in future climates #####
futclimf <- futclim %>%
  # calculate heatsum forcing with 5 degree threshold
  mutate(forcing = case_when(mean_temp_gcm >= 5 ~ mean_temp_gcm - 5,
                             mean_temp_gcm < 5 ~ 0) )%>%
  arrange(SSP, Site, Year, normal_period, DoY) %>%
  group_by(SSP, Site, Year, normal_period) %>%
  mutate(sum_forcing = cumsum(forcing)) %>%
  ungroup() %>% group_by(Site, Year, name) %>%
  group_split()


# split futclimf dataframe (group_split) by Site, Year, and name and then use purrr map to run find day of forcing function

## DoY predictions for future (general) #####

# select only columns necessary for matching to DoY in climate dataset
smallgs <- generalsim %>%
  ungroup() %>%
  select(.prediction)%>%
  arrange(.prediction)


doypredmatchfut <- purrr::map(futclimf, match_force_future, bdf = smallgs, aforce = "sum_forcing", bforce = ".prediction")  %>%
  bind_rows() %>%
  full_join(generalsim)

# calculate medians for each event at each site for each SSP and normal period
doypredmatchfut_medians <- doypredmatchfut %>% group_by(Sex, event, Site, SSP, normal_period, climate_forcing) %>%
  point_interval(newdoycol, .width = c(0.5, 0.95), .point = median, .interval = hdci) %>%
  mutate(Date = as.Date("2020-12-31") + newdoycol, .lowerdate = as.Date("2020-12-31") + .lower, .upperdate = as.Date("2020-12-31") + .upper)

ggplot(filter(doypredmatchfut_medians, climate_forcing %in% c(4.5, 8.5)), aes(y = Date, x = normal_period, ymin = .lowerdate, ymax = .upperdate, group = interaction(normal_period, Sex), colour = Sex)) +
  geom_pointinterval(position = "dodge", alpha = 0.5)  +
  facet_grid(climate_forcing ~ Site) +
  #scale_colour_viridis_d(end = 0.9) +
  theme_dark(base_size = 18) +
  labs(title = "Future flowering periods at 7 sites for 2 Climate forcing scenarios", subtitle = "median start day to median end day", caption = "medians of 1500 forcing observations simulated from 30 draws of the posterior with new factor levels and matched \nto day of year data for plotted sites and years. Daily temperature timeseries for 7 sites from PCIC & adjusted using ClimateNA") +
  #theme(legend.position = "none") +
  scale_colour_viridis_d() +
  scale_y_date(date_labels = "%b %e") +
  theme(axis.text.x = element_text(angle = 30, hjust=1), legend.position = "top") +
  xlab("Normal period")
