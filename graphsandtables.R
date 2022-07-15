# visualizations
library(dplyr)
library(ggplot2)
library(forcats)
library(ggbeeswarm)
library(tidybayes)
library(patchwork)
library(sjPlot) #html tables
#library(RColorBrewer)
library(ggokabeito)   # Neat accessible color palette
library(ggthemes)    # Nice themes

theme_set(theme_dark())

# cumulative_distribution ####
# raw data plot using phenf from modelmethods.R
phenf <- readRDS("objects/phenf.rds")
ggplot(phenf, aes(x = sum_forcing, color = Event_Label, linetype = Sex)) +
  stat_ecdf() +
  labs(title = "Cumulative distribution of accumulated forcing at observation", caption = "raw data") +
  scale_colour_viridis_d() +
  #theme_dark(base_size = 18) +
  ylab("") +
  xlab("GDD")
ggsave("plots/cumulative_distribution.png", width = 6, height = 5)

# censoring table ####
censdf <- readRDS("objects/censdf.rds")
knitr::kable(censdf, caption="Proportion of observations for each event interval censored or left or right end censored")

# censoring graph
# censdf <- readRDS("objects/censdf.rds")
# ggplot(censdf, aes(x = Sex, y = prop_cens, fill = censored)) +
#   geom_bar(stat = "identity") +
#   facet_wrap("Event_Label")

# parameters ####
## means ####
# plot of population means using means from modelparameters.R - better as a table?
means <- readRDS("objects/means.rds")
meanplot <- ggplot(means, aes(y = fct_rev(event), x = .value, colour = Sex)) +
  stat_eye(position = "dodge") +
  scale_colour_viridis_d() +
  labs(title = "Population mean", caption = "2000 draws from the posterior") +
  ylab("") +
  xlab("GDD") +
 # theme_dark(base_size = 18) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))

smallmeans <- filter(means, .draw %in% sample(unique(means$.draw), size = 200))
#ggsave("plots/means.pdf", width = 6, height = 6)

## sd ####
# plot sd parameters using variation from modelparameters.R
variation <- readRDS("objects/variation.rds")
varplot <- ggplot(variation, aes(y = fct_rev(.variable), x = .value, colour = .variable, linetype = Sex)) +
  stat_pointinterval(position = "dodge") +
  scale_colour_viridis_d() +
  labs(title = "Standard deviation of pop mean & offsets", caption = "2000 draws from the posterior") +
  ylab("") +
  xlab("GDD") +
  facet_grid(event ~ .) +
  guides(color = "none", size = "none") +
  theme_dark() +
  theme(legend.position = "top")
#ggsave("plots/sd.pdf", width = 6, height = 5)
ggsave("../flowering-cline/figures/sd.png", width = 6, height = 5)

## generations ####
gens <- readRDS("objects/gens.rds")
ggplot(gens, aes(y = forcats::fct_rev(event), x = bsp_moGeneration, colour = Sex)) +
  stat_pointinterval(position = "dodge") +
  scale_colour_viridis_d() +
  labs(title = "", caption = "2000 draws from the posterior") +
  ylab("") +
  xlab("GDD") +
  theme_dark(base_size = 18) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))

simos <- readRDS("objects/simos.rds")
ggplot(simos, aes(x=simplex, y = distance, color = Sex)) +
  stat_pointinterval(position = "dodge") +
  facet_wrap("event") +
  scale_colour_viridis_d() +
  theme_dark(base_size = 18) +
  theme(legend.position = "bottom") +
  labs(title = "Relative distance between generations", caption = "2000 draws from the posterior")

## offset_medians ####
# plot medians of offset parameters in point clouds (like beeswarm)
offsets_summary <- readRDS("objects/offsets_summary.rds")
offsetplot <- offsets_summary %>%
  select(model, Sex, event, factor, level, .value, .point) %>% distinct() %>%
  ggplot(aes(y=.value, x = model, colour = Sex, shape = event)) +
  geom_quasirandom(alpha = 0.5) +
  facet_wrap("factor") +
  labs(title = "Offset medians", caption = "2000 draws from posterior") +
  geom_hline(yintercept = 0, linetype =3, colour = "darkgray") +
 # theme_dark(base_size = 18) +
  ylab("GDD") +
  scale_colour_viridis_d() +
  theme(legend.position = "top") +
  geom_hline(yintercept = 0, linetype = 3)
#ggsave("plots/offsets_medians.pdf", width = 6, height = 5)
#

meanplot + offsetplot
ggsave("plots/meanandoffset.png", width = 7, height = 6)

## site_offsets ####
# interval plot for site level offsets using siter from modelparameters.R
siter <- readRDS("objects/siter.rds")
ggplot(siter, aes(y=level, x = .value, colour = Sex)) +
  stat_pointinterval() +
  facet_grid(event ~ Sex) +
  ggtitle("Site offsets", subtitle = "Ordered warmest to coldest MAT") +
 # theme_dark(base_size = 18) +
  ylab("Site") +
  xlab("GDD") +
  geom_vline(xintercept = 0, linetype =3) +
  xlim(c(-90,90)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top")
ggsave("plots/site_offsets.pdf", width = 6, height = 5)

## prov_offsets ####
# interval plot for provenance level offsets using provr from modelparameters.R
provr <- readRDS("objects/provr.rds")
ggplot(provr, aes(y=level, x = .value, colour = Sex)) +
  stat_pointinterval() +
  facet_grid(event ~ Sex) +
  ggtitle("Provenance offsets", subtitle = "Ordered warmest to coldest MAT") +
  # theme_dark(base_size = 18) +
  ylab("Provenance") +
  xlab("GDD") +
  geom_vline(xintercept = 0, linetype =3)  +
  xlim(c(-90,90)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top")
ggsave("plots/prov_offsets.pdf", width = 6, height = 5)

## year_offsets.rds ####
# interval plot for year level offsets using yearr from modelparameters.R
yearr <- readRDS("objects/yearr.rds")
ggplot(yearr, aes(y=level, x = .value, colour = Sex)) +
  stat_pointinterval() +
  facet_grid(event ~ Sex) +
  ggtitle("Year offsets", subtitle = "Ordered warmest to coldest MAT") +
 # theme_dark(base_size = 18) +
  xlab("GDD") +
  ylab("Year") +
  geom_vline(xintercept = 0, linetype =3, colour = "darkgray")  +
  xlim(c(-90,90)) +
  scale_colour_viridis_d() +
  theme(legend.position = "top")
ggsave("plots/year_offsets.pdf", width = 6, height = 5)

# retrodictions ####
# plot simulations of GDD flowering event with alldat and allsim from `modelparameters.R`
alldat <- readRDS("objects/alldat.rds")
allsim <- readRDS("objects/allsim.rds") #2.4GB
ggplot(alldat, aes(x = sum_forcing, y = "observations" , colour = Sex)) +
  stat_dotsinterval( .width = c(0.5, 0.89), point_interval = median_qi) +
  stat_slab(data = allsim,
            aes(x = .prediction, y = prediction_type, group=.draw),
            .width = c(0.5, 0.89), point_interval = median_hdci,
            slab_color = "gray65", alpha = 1/10, fill = NA) +
  stat_pointinterval(data = allsim, aes(x = .prediction, y = prediction_type),
                     .width = c(0.5, 0.89), point_interval = median_hdci ) +
  stat_slab(data = smallmeans, aes(x = .value, y = "population mean"),
            .width = c(0.5, 0.89), point_interval = median_hdci,
            slab_color = "gray65", fill = NA) +
  stat_pointinterval(data = smallmeans, aes(x = .value, y = "population mean"),
                     .width = c(0.5, 0.89), point_interval = median_hdci ) +
 # theme_bw(base_size = 18) +
  theme_bw() +
  facet_grid(event ~ Sex) +
  labs(title = "Modeled and observed flowering events", caption = "200 samples from the posterior except 30 for fully crossed predictions") +
  xlab("GDD") +
  ylab("") +
  scale_colour_viridis_d() +
  theme(legend.position = "none")

#ggsave("plots/retrodictions.pdf", width = 7, height = 5)
ggsave("../flowering-cline/figures/retrodictions.png", width = 7, height = 5.5, units = "in")

## day of year ####
specific_doy_preds <- readRDS("objects/specific_doy_preds.rds")
specific_median <- specific_doy_preds %>%
  group_by(prediction_type, .draw) %>%
  median_hdci(.prediction)
ggplot(filter(specific_doy_preds, prediction_type == "retrodiction - uncensored"), aes(x = .prediction, y = newdoycol, colour = Site, group = Year)) +
  geom_point(pch = 1) +
  facet_grid(event ~ Sex)

## day_of_year_lines ####
# time series line plot with begin and end faceted by site with general_doy_preds_med_siteyearsex from `retrodictandpredict.R`
general_doy_preds_med_siteyearsex <- readRDS("objects/general_doy_preds_med_siteyearsex.rds")
ggplot(general_doy_preds_med_siteyearsex, aes(x=Year, y = newdoycol, linetype = Sex, colour = event)) +
  #geom_point() +
  geom_line() +
  scale_colour_viridis_d() +
  facet_wrap("Site") +
  theme(legend.position = "top")
ggsave("plots/day_of_year_lines.pdf", width = 6, height = 5)

# predictions ####

## grand mean posterior predictions ####
fepred <- readRDS("objects/fepred.rds")
fpred <- readRDS("objects/fpred.rds")

# just expectations
ggplot(fepred,
       aes(x = .epred, y = Generation, fill = Sex)) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_okabe_ito() +
  labs(title = "Grand mean",
       x = "Predicted forcing", y = "Sex",
       subtitle = "Posterior expectations") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_clean() +
  facet_wrap("event") +
  theme(legend.position = "bottom")

# expectations and full posterior
gmean <- full_join(fepred, fpred) %>%
  dplyr::rename(expectation = .epred, pp = .prediction) %>%
  pivot_longer(cols = c("expectation", "pp"), names_to = "pred_type", values_to = "forcing")

ggplot(gmean,
       aes(x = forcing, y = Generation, fill = Sex, group = interaction(Sex,event))) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_okabe_ito() +
  labs(title = "Grand mean",
       x = "Predicted forcing", y = "Sex",
       subtitle = "Posterior expectations") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_clean() +
  facet_grid(. ~ pred_type) +
  theme(legend.position = "bottom")

# historical_flowering_periods ####
# day of year predictions. Plot to compare time series.
# with general_doy_preds_med_siteyearsex from `retrodictandpredict.R`
general_doy_preds_med_siteyearsex <- readRDS("objects/general_doy_preds_med_siteyearsex.rds")
ggplot(general_doy_preds_med_siteyearsex, aes(y = Date, x = Year, colour = Sex, group = Year)) +
  geom_line() +
  facet_grid(Site ~ Sex) +
  #scale_colour_viridis_d(end = 0.9) +
 # theme_dark(base_size = 18) +
  labs(title = "Flowering period from 1945-2012 at 7 sites", subtitle = "median start day of year to median end day of year", caption = "1500 forcing observations simulated  from 200 draws of the posterior with new factor levels \n and matched to forcing data for plotted sites and years. Daily temperature data from PCIC \nand adjusted using monthly climateNA") +
  theme(legend.position = "none") +
  scale_y_date(date_labels = "%b %e") +
  scale_colour_viridis_d()
ggsave("plots/historical_flowering_periods.pdf", width = 6, height = 7)

# future_flowering_periods ####
# plot flowering periods for 2 climate change scenarios over 21st century normal periods with doypredmatchfut_medians from `retrodictandpredict.R`
doypredmatchfut_medians <- readRDS("objects/doypredmatchfut_medians.rds")
ggplot(filter(doypredmatchfut_medians, climate_forcing %in% c(4.5, 8.5)), aes(y = Date, x = normal_period, ymin = .lowerdate, ymax = .upperdate, group = interaction(normal_period, Sex), colour = Sex)) +
  geom_pointinterval(position = "dodge", alpha = 0.5)  +
  facet_grid(climate_forcing ~ Site) +
  #scale_colour_viridis_d(end = 0.9) +
  #theme_dark(base_size = 18) +
  labs(title = "Future flowering periods at 7 sites for 2 Climate forcing scenarios", subtitle = "median start day to median end day", caption = "medians of 1500 forcing observations simulated from 30 draws of the posterior with new factor levels and\n matched to day of year data for plotted sites and years. Daily temperature timeseries for 7 sites from PCIC & adjusted using ClimateNA") +
  #theme(legend.position = "none") +
  scale_colour_viridis_d() +
  scale_y_date(date_labels = "%b %e") +
  theme(axis.text.x = element_text(angle = 30, hjust=1), legend.position = "top") +
  xlab("Normal period")
ggsave("plots/future_flowering_periods.pdf", width = 8, height = 5)

# length_histogram ####
# histogram + intervals for flowering period length with specific_doy_preds_length from `floweringlength.R`
# might work better as a boxplot, or with limits argument set
specific_doy_preds_length <- readRDS("objects/specific_doy_preds_length.rds")
ggplot(specific_doy_preds_length, aes(x = length, y = prediction_type, colour = Sex)) +
  stat_histinterval(position = "dodge", .width = c(0.5, 0.89)) +
  scale_colour_viridis_d() +
  theme_bw() +
  theme(legend.position = "top") +
  labs(title = "Length of phenological period for individuals")
ggsave("plots/length_histogram.pdf", width = 6, height = 5)


# length_beeswarm ####
# beeswarm plots of flowering period length at each site, ordered warmest to coolest using specific_doy_preds_length_ts from `floweringlength.R`

specific_doy_preds_length_ts <- readRDS("objects/specific_doy_preds_length_ts.rds")
ggplot(filter(specific_doy_preds_length_ts, prediction_type == "prediction - full cross"), aes(x = Site, y = length, colour = Sex)) +
  geom_beeswarm(dodge.width =0.75) +
  scale_colour_viridis_d() +
  labs(title = "Median flowering period length at a site", subtitle = "Each point represents one year at one site 1997-2012", caption = "fully crossed predictions") +
  theme(legend.position = "top") +
  ylab("Length of flowering period (days)")
ggsave("plots/length_beeswarm.pdf", width = 6.5, height = 5)

# length_future ####
# plots of flowering period length under different climate change scenarios using futlen from `floweringlength.R`
futlen <- readRDS("objects/futlen.rds")
ggplot(filter(futlen, climate_forcing %in% c(4.5, 8.5)), aes(y = period_length, x = normal_period, colour = Sex)) +
  geom_point()  +
  facet_grid(climate_forcing ~ Site) +
  #scale_colour_viridis_d(end = 0.9) +
  #theme_dark(base_size = 18) +
  #theme(legend.position = "none") +
  scale_colour_viridis_d() +
  theme(legend.position = "bottom") +
  ylab("Length of flowering period (days)") +
  labs(title = "Future flowering period median length", caption = "fully crossed - only 5 posterior samples!") +
  theme(axis.text.x = element_text(angle = 30, hjust=1), legend.position = "top") +
  xlab("Normal period")
ggsave("plots/length_future.pdf", width = 7, height =5)

# sitexsiteoverlap ####
# violin plots of overlap between sites with med_overlap_sys from `overlap.R`
med_overlap_sys <- readRDS("objects/med_overlap_sys.rds")
ggplot(filter(med_overlap_sys, .width == 0.5), aes(x = 1, y = overlap, fill = Site )) +
  geom_violin(draw_quantiles = c(0.5), alpha = 0.5) +
  facet_grid(Site ~ male_Site) +
  scale_fill_viridis_d() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top") +
  labs(title = "Days of flowering overlap between sites", subtitle = "1945-2012", caption = "30 forcing samples from the model translated into DoY of flowering event for 7 Sites 1945-2012. \nThen calculated median DoY across samples and used those to construct flowering period \nintervals (begin to end). Then determined the intersection of those intervals for all sites and years")
ggsave("plots/sitexsiteoverlap.pdf", width = 7, height = 8)

# site_overlap ####
# histogram plots of overlap between sites with med_overlap_sys from `overlap.R`. Total number of days a site has overlap with any other site
med_overlap_sys <- readRDS("objects/med_overlap_sys.rds")
ggplot(filter(med_overlap_sys, .width == 0.5, overlap > 0), aes(x = overlap, fill = male_Site )) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_grid(Site ~ .) +
  scale_fill_brewer(type = "div", palette = 3) +
  theme(legend.position = "top") +
  labs(title = "Days receptivity overlaps with pollen shed at different sites", subtitle = "1945-2012", caption = "30 forcing samples from the model translated into DoY of flowering event for 7 Sites 1945-2012.\n Then calculated median DoY across samples and used those to construct flowering period \nintervals (begin to end). Then determined the intersection of those intervals for all sites and years") +
  xlab("days") +
  ylab("years")
ggsave("plots/site_overlap.pdf", width = 6, height = 8)

# future_overlap ####
# bar plots of days of flowering overlap between sites for 3 normal periods across five climate change scenarios with fut_overlap_sys from `overlap.R`
fut_overlap_sys <- readRDS("objects/fut_overlap_sys.rds")
ggplot(filter(fut_overlap_sys, .width == 0.5), aes(x = as.factor(climate_forcing), y = overlap, fill=normal_period )) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(Site ~ male_Site) +
  scale_fill_brewer(type = "seq") +
  theme(legend.position = "top")+
  labs(title = "Days of flowering overlap between sites", subtitle = "Normal period 2011-2040", caption = "30 forcing samples from the model translated into DoY of flowering event for 7 Sites 1945-2012. \nThen calculated median DoY across samples and used those to construct flowering period intervals (begin to end).\n Then determined the intersection of those intervals for all sites and years") +
  xlab("climate forcing") +
  ylab("number of overlap days")
ggsave("plots/future_overlap.pdf", width = 7, height = 6)

# clone model parameters ####

## table ####
clonemodells <- readRDS("objects/clonemodells.rds") # from clonemodelanalysis.R

## begin
tab_model(list(clonemodells$fb, clonemodells$mb), dv.labels = c("Female", "Male"), title = "Begin flowering", file = "../flowering-cline/tables/provclimeffstart.html")

## end
tab_model(list(clonemodells$fe, clonemodells$me), dv.labels = c("Female", "Male"), title = "End flowering", file = "../flowering-cline/tables/provclimeffend.html")


# clone model predictions ####
clonedat <- readRDS("objects/clonedat.rds")
clonepred <- readRDS("objects/clonepred.rds")

# lines & points ####
theme_set(theme_dark())
# caption = "points: 1961-81 climate normal MAT, mean of clone offset estimates from thermal time model (2000 posterior samples). lines: posterior distribution of genotype offset from the provenance climate effect model"
ggplot(clonepred, aes(x = MAT, y = meanoffset)) +
  stat_lineribbon(aes(y = .prediction, colour = Sex)) +
  geom_point(data = bind_rows(clonedat), aes(x = MAT, y = meanoffset, colour = Sex), pch = 1, alpha = 0.7) +
  scale_fill_brewer(palette = "Greys") +
  facet_grid(event ~ Sex) +
  ylab("Genotype offset") +
  xlab(expression("Provenance Mean Annual Temperature " ( degree*C))) +
  scale_color_viridis_d() +
  labs(title = "Predicted and observed genotype effects")
ggsave("../flowering-cline/figures/genotype_effects_modeled_observed.png", width = 7, height = 6)

