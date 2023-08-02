# visualizations
library(flowers)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggbeeswarm)
library(tidybayes)
library(patchwork)
#library(sjPlot) #html tables
#library(RColorBrewer)
library(ggokabeito)   # Neat accessible color palette
library(ggthemes)    # Nice themes
library(html2latex) # convert sjplot tables to tex and pdf
library(tidyr)
library(ggrepel)

theme_set(theme_dark())
factororder <- readRDS("objects/factororder.rds")

# forcing and climate
typical_year_forc <- read.csv("data/typical_year_forc.csv") %>% # from temp mean at each site across 1945-2012
  mutate(Date = as.Date(Date_scale)) %>%
  select(-Date_scale) %>%
  mutate(Site = factor(Site, levels = factororder$site)) %>%
  mutate(Site_type = case_when(Site %in% c("Border", "Trench") ~ "comparison",
                               !Site %in% c("Border", "Trench") ~ "orchard")) %>%
  mutate(Site_type = fct_rev(Site_type)) %>%
  filter(DoY < 180)

meantempplot <- ggplot(typical_year_forc, aes(x = Date, y = mean_temp, color = Site, linetype = Site_type)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  ggtitle("Mean daily temperature") +
  scale_colour_viridis_d() +
  theme_bw() +
  ylab("Temperature (\u00B0C)") + xlab("") +
  theme(legend.position = "none")

sumforcplot <- ggplot(typical_year_forc, aes(x = Date, y = sum_forcing, color = Site, linetype = Site_type)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  ggtitle("Forcing accumulation") +
  scale_color_viridis_d() +
  theme_bw() +
  ylab("Growing Degree Days (GDD)") + xlab("") +
  guides(linetype = "none", color = guide_legend(ncol = 3)) +
  theme(legend.position = "bottom")

forcplot <- ggplot(typical_year_forc, aes(x = Date, y = forcing, color = Site, linetype = Site_type)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  ggtitle("Daily forcing") +
  scale_color_viridis_d() +
  theme_bw() +
  ylab("Growing Degree Days (GDD)") + xlab("") +
  guides(linetype = "none", color = guide_legend(ncol = 3)) +
  theme(legend.position = "none")


siteclimplot <- meantempplot / forcplot / sumforcplot+
  plot_annotation(tag_levels = 'A')
ggsave("../flowering-cline/figures/siteclimplot.png", width = 4, height = 9)

# MAT ####
sites <- read.csv("../lodgepole_climate/data/climateBC/climatebc_locs_Normal_1961_1990Y.csv") %>%
  filter(id == "site") %>%
  select(Site, MAT) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_reorder(Site, MAT)))
sites$type <- c(rep("Seed Orchard", 7), rep("Comparison", 2))

provs <- readRDS("objects/phenf.rds") %>%
  select(Tree, MAT, Site) %>%
  distinct()

bcspus <- read.csv("../phd/data/OrchardInfo/lodgepole_SPU_climsum.csv")

ggplot(data=sites) +
  geom_point(aes(x = "Sites", y = MAT, colour = Site, shape = type), size = 2.5) +
  geom_text_repel(aes(x = "Sites", y = MAT, label = Site),size = 2) +
  geom_quasirandom(data = provs, aes(x = "Provenances", y = MAT, colour = Site), alpha = 0.7, pch = 3, varwidth = TRUE) +
  scale_color_brewer(type = "seq") +
  theme_dark() +
  xlab("") +
  ylab("Mean Annual Temperature (\u00B0C)") +
  guides(color = "none", shape = "none")


ggsave("../flowering-cline/figures/MAT.png", width = 6, height = 5)

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

# sampling events ####
phenf <- readRDS("objects/phenf.rds")
surveydf <- phenf %>%
  select(Year, Site, Orchard, DoY) %>%
  distinct() %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site)) %>%
  group_by(Site, Year, Orchard) %>%
  mutate(sampleindex = cur_group_id()) %>%
  ungroup()

yrspersite <- surveydf %>%
  select(Site, Year) %>%
  distinct() %>%
  group_by(Site) %>%
  summarise(nyears = n()) %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site)) %>%
  arrange(as.factor(Site))

facet_labeller_site <- function(variable, value) {
  c(
    "PGTIS",
    rep("", yrspersite$nyears[1] - 1),
    "KettleRiver",
    rep("", yrspersite$nyears[2] - 1),
    "Sorrento",
    "Tolko",
    rep("", yrspersite$nyears[4] - 1),
    "PRT",
    rep("", yrspersite$nyears[5] - 1),
    "Vernon",
    rep("", yrspersite$nyears[6] - 1),
    "Kalamalka",
    rep("", yrspersite$nyears[7] - 1)
  )
}

# https://stackoverflow.com/questions/54178285/how-to-remove-only-some-facet-labels

ggplot(surveydf, aes(x=DoY, y=as.factor(sampleindex), colour = Site, group = as.factor(Orchard))) +
  geom_point(pch=3) +
  geom_line(alpha = 0.5) +
  facet_grid(rows=vars(Site,Year), scales="free_y",
             labeller = labeller(Site = as_labeller(facet_labeller_site))) +
  #facet_grid(rows=vars(Site, Year), scales = "free_y") +
  #scale_color_viridis_d(option="B") +
  scale_color_okabe_ito() +
  scale_shape_manual(values=c(1:7)) +
  theme_bw(base_size = 15) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        panel.border=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        strip.background = element_blank()) +
  xlab("Day of Year") +
  # annotate("text", x = 121, y = 1, label = "May") +
  # geom_vline(xintercept = c(121,152), alpha = 0.5) +
  ggtitle("Observation Dates", subtitle = "for each orchard at each site")
ggsave("../flowering-cline/figures/sampling.png", width = 6, height = 8)

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
intercepts <- readRDS("objects/intercepts.rds")
interceptplot <- ggplot(intercepts, aes(y = fct_rev(event), x = .value, colour = Sex)) +
  stat_halfeye(position = "dodge") +
  scale_colour_viridis_d() +
  #labs(title = expression(paste("Mean forcing requirement at 0 \u00B0C (", mu, ")")), caption = "6000 draws from the posterior") +
  ylab("") +
  xlab("GDD") +
 # theme_dark(base_size = 18) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
  scale_y_discrete(expand = expansion(add = c(0, .75)))
interceptplot
#smallmeans <- filter(means, .draw %in% sample(unique(means$.draw), size = 200))
ggsave("../flowering-cline/figures/intercepts.png", width = 6, height = 6)

## slopes ####
ggplot(fepred_allprovs, aes(x = MAT, y = .epred)) +
  stat_lineribbon(aes(y = .epred, linetype = event), .width = c(.95, .5), show.legend = FALSE) +
  scale_fill_brewer() +
  theme_bw() +
  facet_grid(. ~ Sex) +
  theme(legend.position = "none") +
  #ggtitle("Forcing requirements across all provenances", subtitle = "expectation (mean) predictions, caption = "6000 draws from the posterior") +
  ylab("Accumulated forcing (Growing Degree Days)") +
  xlab("Mean Annual Temperature (\u00B0C)")

slopes <- readRDS(file = "objects/slopes.rds")
slopesummary <- slopes %>%

ggplot(slopes, aes(x = .value, y = event, fill = Sex)) +
  stat_slabinterval(alpha = 0.5) +
  scale_fill_viridis_d(option = "B") +
  xlab("GDD per MAT") +
  ylab("") +
  labs(title = 'MAT effect', caption = '6000 draws from the posterior')

## sd ####
# plot sd parameters using variation from modelparameters.R
variation <- readRDS("objects/variation.rds")
varplot <- ggplot(variation, aes(y = fct_rev(.variable), x = .value, colour = .variable, linetype = Sex)) +
  stat_pointinterval(position = "dodge") +
  scale_colour_viridis_d(option = "B") +
  labs(title = "Standard deviation of pop mean & offsets", caption = "6000 draws from the posterior") +
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
  labs(title = "Offset medians", caption = "6000 draws from posterior") +
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

