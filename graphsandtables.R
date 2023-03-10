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
library(html2latex) # convert sjplot tables to tex and pdf

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
  geom_hline(yintercept = 0, linetype = 3) +
  coord_flip()
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
# alldat <- readRDS("objects/alldat.rds")
# allsim <- readRDS("objects/allsim.rds") #2.4GB
# ggplot(alldat, aes(x = sum_forcing, y = "observations" , colour = Sex)) +
#   stat_dotsinterval( .width = c(0.5, 0.89), point_interval = median_qi) +
#   stat_slab(data = allsim,
#             aes(x = .prediction, y = prediction_type, group=.draw),
#             .width = c(0.5, 0.89), point_interval = median_hdci,
#             slab_color = "gray65", alpha = 1/10, fill = NA) +
#   stat_pointinterval(data = allsim, aes(x = .prediction, y = prediction_type),
#                      .width = c(0.5, 0.89), point_interval = median_hdci ) +
#   stat_slab(data = smallmeans, aes(x = .value, y = "population mean"),
#             .width = c(0.5, 0.89), point_interval = median_hdci,
#             slab_color = "gray65", fill = NA) +
#   stat_pointinterval(data = smallmeans, aes(x = .value, y = "population mean"),
#                      .width = c(0.5, 0.89), point_interval = median_hdci ) +
#  # theme_bw(base_size = 18) +
#   theme_bw() +
#   facet_grid(event ~ Sex) +
#   labs(title = "Modeled and observed flowering events", caption = "200 samples from the posterior except 30 for fully crossed predictions") +
#   xlab("GDD") +
#   ylab("") +
#   scale_colour_viridis_d() +
#   theme(legend.position = "none")

## day of year ####
# specific_doy_preds <- readRDS("objects/specific_doy_preds.rds")
# specific_median <- specific_doy_preds %>%
#   group_by(prediction_type, .draw) %>%
#   median_hdci(.prediction)
# ggplot(filter(specific_doy_preds, prediction_type == "retrodiction - uncensored"), aes(x = .prediction, y = newdoycol, colour = Site, group = Year)) +
#   geom_point(pch = 1) +
#   facet_grid(event ~ Sex)
#
# ## day_of_year_lines ####
# # time series line plot with begin and end faceted by site with general_doy_preds_med_siteyearsex from `retrodictandpredict.R`
# general_doy_preds_med_siteyearsex <- readRDS("objects/general_doy_preds_med_siteyearsex.rds")
# ggplot(general_doy_preds_med_siteyearsex, aes(x=Year, y = newdoycol, linetype = Sex, colour = event)) +
#   #geom_point() +
#   geom_line() +
#   scale_colour_viridis_d() +
#   facet_wrap("Site") +
#   theme(legend.position = "top")
# ggsave("plots/day_of_year_lines.pdf", width = 6, height = 5)

# predictions ####

## grand mean posterior predictions ####
fepred <- readRDS("objects/fepred.rds")
fpred <- readRDS("objects/fpred.rds")
fepred_cenew <- readRDS("objects/fepred_cenew.rds")
fepred_ceold <- readRDS("objects/fepred_ceold.rds")
fepred_newsites <- readRDS("objects/fepred_newsites.rds")

# just expectations
ggplot(fepred,
       aes(x = .epred, y = event, fill = Sex)) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_okabe_ito() +
  labs(title = "Grand mean",
       x = "Predicted forcing", y = "Sex",
       subtitle = "Posterior expectations") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_clean() +
  theme(legend.position = "bottom")

# expectations table

etab <- fepred %>%
  group_by(Sex, event) %>%
  median_hdi(.epred)  %>%
  #rename(Event = event, Median = .epred, `2.5 qi` = .lower, `97.5 qi` = .upper) %>%
  select(-.width, -.interval) %>%
  mutate(Expectation = paste(round(.epred,0), " (", round(.lower, 0), ", ", round(.upper,0), ")", sep = "")) %>%
  select(- starts_with(".")) %>%
  pivot_wider(names_from = event, values_from = Expectation) %>%
  rename(Begin = begin, End = end)
saveRDS(etab, "objects/etab.rds")

## forcing and day of year expectations ####
# doy
doy_typical <- readRDS("objects/doy_typical.rds") %>%
  filter(! Site %in% c("Sorrento", "Tolko", "PRT", "Vernon"))
expdoy <- ggplot(doy_typical, aes(x = DoY, y = forcats::fct_rev(Sex), color = Sex, shape = event)) +
  stat_pointinterval() +
  facet_grid(forcats::fct_rev(Site) ~ .) +
 # labs(caption = "typical year based on mean daily heat sum accumulation at 7 sites between 1945 and 2012") +
  xlab("Day of Year") +
  scale_color_viridis_d() +
  theme_dark() +
  theme(strip.text.y.right = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom")


# forcing, which is the same for each site!
expforc <- ggplot(fepred, aes(x=.epred, y = forcats::fct_rev(Sex), color = Sex, shape = event)) +
  stat_halfeye() +
  xlab("Accumulated Growing Degree Days") +
  scale_color_viridis_d() +
  theme_dark() +
  theme(legend.position = "none", axis.title.y = element_blank())
#labs(title = "Forcing requirements", subtitle = "in any year or site")

# combine forcing & doy plots
expected <- expforc + expdoy
expected + plot_annotation(title = "Event expectations",
                           subtitle = "from thermal time model",
                           tag_levels = "A")
ggsave("../flowering-cline/figures/eventexpecations.png", width = 8, height = 6 )

## expectations and full posterior ################
fpred <- readRDS("objects/fpred.rds")
gmean <- full_join(fepred, fpred) %>%
  dplyr::rename(expectation = .epred, `full posterior` = .prediction) %>%
  pivot_longer(cols = c("expectation", "full posterior"), names_to = "pred_type", values_to = "forcing")

ggplot(gmean,
       aes(x = forcing, y = pred_type, fill = Sex, group = interaction(Sex,event))) +
  stat_halfeye(alpha = 0.8) +
  scale_fill_viridis_d() +
  labs(title = "Forcing requirements",
       x = "Predicted forcing (GDD)", y = "",
       subtitle = "Posterior expectations and posterior predictive")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_dark() +
  theme(legend.position = "right")
ggsave("plots/forcing_fullandexpectation.png", width = 6, height = 5)

factororder <- readRDS("objects/factororder.rds")

fepred_cenew <- readRDS("objects/fepred_cenew.rds")
fepred_ceold <- readRDS("objects/fepred_ceold.rds")
fepred_ce <- full_join(fepred_cenew, fepred_ceold)

factororder_site_so <- factororder$site[-c(1,2)]
fepred_ceold <- mutate(fepred_ceold, Site = forcats::fct_relevel(Site, factororder_site_so))
# fepred_ce %>%
#   sample_frac(0.25) %>%
#   mutate(Site = forcats::fct_relevel(Site, c(factororder_site_so, "new_Site")))

ggplot(fepred_cenew,
       aes(x = .epred, y = Sex, fill = Sex)) +
  stat_halfeye(alpha = 0.7) +
  scale_fill_okabe_ito() +
  theme_clean() +
  facet_grid(. ~ event)


## table ####
clonemodells <- readRDS("objects/clonemodells.rds") # from clonemodelanalysis.R

## begin
sjPlot::tab_model(list(clonemodells$fb, clonemodells$mb), dv.labels = c("Female", "Male"), title = "Begin flowering", file = "../flowering-cline/tables/provclimeffstart.html")
provclimeffstart_knit <- tab_model(list(clonemodells$fb, clonemodells$mb), dv.labels = c("Female", "Male"), title = "Begin flowering")$knitr
saveRDS(provclimeffstart_knit, "../flowering-cline/tables/proveclimeffstart_knit.rds")

## end
tab_model(list(clonemodells$fe, clonemodells$me), dv.labels = c("Female", "Male"), title = "End flowering", file = "../flowering-cline/tables/provclimeffend.html")
provclimeffend_knit <- tab_model(list(clonemodells$fe, clonemodells$me), dv.labels = c("Female", "Male"), title = "End flowering", file = "../flowering-cline/tables/provclimeffend.html")$knitr
saveRDS(provclimeffend_knit, "../flowering-cline/tables/provclimeffend_knit.rds")



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
  ylab("Genotype offset (GDD)") +
  xlab(expression("Provenance Mean Annual Temperature " ( degree*C))) +
  scale_color_viridis_d() +
  labs(title = "Predicted and observed genotype effects")
ggsave("../flowering-cline/figures/genotype_effects_modeled_observed.png", width = 7, height = 6)

