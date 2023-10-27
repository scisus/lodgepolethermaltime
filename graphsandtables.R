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
library(cols4all)

theme_set(theme_dark())
factororder <- readRDS("objects/factororder.rds")
factororder_site_so <- factororder$site[-c(1,2)]

# forcing and climate ##########
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

# censoring ####

## table
censdf <- readRDS("objects/censdf.rds")
knitr::kable(censdf, caption="Proportion of observations for each event interval censored or left or right end censored")

# Observed vs retrodicted ##############
fretro_summary <- readRDS("objects/fretro_summary.rds")
censorpal <- c4a("icefire", 3)
ggplot(fretro_summary, aes(x = sum_forcing, y = .prediction, color = censored)) +
  geom_point(alpha = .5, shape = 3) +
  facet_grid(Sex ~ event) +
  geom_abline(color = "grey20") +
  xlab("Observed accumulated forcing (GDD)") +
  ylab("Median retrodicted accumulated forcing (GDD)") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_colour_manual(values = c(censorpal[2], censorpal[3], censorpal[1]))
ggsave("../flowering-cline/figures/obsvsretro.png", width = 6, height = 5)

# parameters ####
## means ####
# plot of population means using means from modelparameters.R - better as a table?
intercepts <- readRDS("objects/intercepts.rds")
interceptplot <- ggplot(intercepts, aes(y = fct_rev(event), x = .value, colour = Sex)) +
  stat_halfeye(position = "dodge") +
  scale_colour_viridis_d() +
  #labs(title = expression(paste("Mean forcing requirement at 0 \u00B0C (", mu, ")")), caption = "6000 draws from the posterior") +
  ylab("") +
  xlab(expression(paste("Mean forcing accumulation required at MAT 0 \u00B0C - ", mu, " (GDD)"))) +
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
varplot <- ggplot(variation, aes(y = forcats::fct_rev(event), x = .value, colour = forcats::fct_rev(Sex), shape = event)) +
  stat_pointinterval(position = "dodge") +
  scale_colour_viridis_d(limits = c("FEMALE", "MALE")) +
 # labs(title = "Standard deviation of pop mean & offsets", caption = "6000 draws from the posterior") +
  ylab("Event") +
  xlab("Standard deviation (GDD)") +
  facet_grid(.variable ~ ., scales = "free_y") +
  guides(color = "none", shape = "none") +
  theme_dark() +
  theme(legend.position = "top")
#ggsave("plots/sd.pdf", width = 6, height = 5)
#ggsave("../flowering-cline/figures/sd.png", width = 6, height = 5)

## offset_medians ####
# plot medians of offset parameters in point clouds (like beeswarm)
offsets_summary <- readRDS("objects/offsets_summary.rds") %>%
  mutate(model = factor(model, levels = c("mb", "fb", "me", "fe")))
offsetplot <- offsets_summary %>%
  select(model, Sex, event, factor, level, .value, .point) %>% distinct() %>%
  ggplot(aes(y=.value, x = model, colour = Sex, shape = event)) +
  geom_quasirandom(alpha = 0.5) +
  facet_wrap("factor") +
  #labs(title = "Offset medians", caption = "6000 draws from posterior") +
  #guides(color = "none", shape = "none") +
  geom_hline(yintercept = 0, linetype =3, colour = "darkgray") +
  scale_colour_viridis_d() +
  theme_dark() +
  xlab("") +
  ylab("Offset median (GDD)") +
  theme(legend.position = "bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 0, linetype = 3) +
  coord_flip()
#ggsave("plots/offsets_medians.pdf", width = 6, height = 5)
#

varplot + offsetplot +
  plot_layout(widths = c(1,2.5)) +
  plot_annotation(tag_levels = 'A')
ggsave("../flowering-cline/figures/varoffsets.png", width = 7, height = 6)

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

## orchard specific retrodictions ####
### GDD ##########
# using 95% HDCI, median posterior prediction for each site for the full range of provenances (MATs) using an average year, clone, and tree (i.e. using estimated gaussian prior to generate those random effects), but using site specific effects estimated from the model (delta offset). Posterior predictions contain full range of uncertainty because I want the orchard managers to know what to actually expect

fpred_orch_summary <- readRDS("objects/fpred_orch_summary.rds")

widefpredorchsum <- fpred_orch_summary %>%
  tidyr::pivot_wider(
    id_cols = c(MAT, Year, Tree, Clone, Site, Sex),
    names_from = event,
    values_from = c(.prediction, .lower, .upper),
    names_sep = "."
  )

gdd_orch <- ggplot(fpred_orch_summary) +
  # colored ribbons for start and end
  geom_ribbon(aes(x = MAT, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  # solid ribbon for median flowering period
  geom_ribbon(data = widefpredorchsum, aes(x = MAT, ymin = .prediction.begin, ymax = .prediction.end), alpha = 0.7) +
  # outlines of start and end
  #geom_ribbon(data=fpred_orch_summary, aes(x = MAT, ymin = .lower, ymax = .upper, colour = event), fill = "transparent", size = .5, linetype = 3) +
  facet_grid(Site ~ Sex) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  theme_bw() +
  ylab("GDD") +
  xlab("Source MAT") +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 8),
    legend.position = "bottom"
  ) +
  labs(fill = "95% HDPI", colour = "95% HDPI")


### DoY #####

doy_annual_pp_sum <- readRDS("objects/doy_annual_pp_sum.rds")

widedoypporchsum <- doy_annual_pp_sum %>%
  tidyr::pivot_wider(
    id_cols = c(MAT, Year, Site, Sex),
    names_from = event,
    values_from = c(DoY, .lower, .upper),
    names_sep = "."
  )
widedoypporchsum$MAT_label <- paste("MAT:", widedoypporchsum$MAT)

phenf_orchplot <- readRDS("objects/phenf.rds") %>%
  filter(Event_Obs %in% c(2,3)) %>%
  select(-MAT) %>%
  mutate(Site = forcats::fct_relevel(Site, factororder_site_so), Year = as.numeric(Year))

# 2000 draws, 1945-2011 model preds. grey ribbon shows median start to median end, blue and pink ribbons show uncertainty for start and end. Used coldest and warmest source MAT for contrast. Vertical black lines show range of flowering observations in data.
doy_orch <- ggplot() +
  geom_line(data = phenf_orchplot, aes(x = Year, y = DoY, group = Year)) +
  geom_ribbon(data = doy_annual_pp_sum, aes(x = Year, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  labs(fill = "95% HDPI", colour = "95% HDPI") +
  geom_ribbon(data = widedoypporchsum, aes(x = Year, ymin = DoY.begin, ymax = DoY.end), alpha = 0.5) +
  theme_bw() +
  xlab("Year") +
  ylab("Date") +
  facet_grid(Site ~ MAT_label + Sex) +
  scale_y_continuous(
    breaks = seq(1, 365, by = 14),  # Breaks every 2 weeks
    labels = format(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "2 weeks"), "%b %d")) +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

gdd_orch + doy_orch +
  plot_layout(widths = c(1,2)) +
  plot_annotation(tag_levels = 'A')
ggsave("../flowering-cline/figures/orchpred.png", width = 9, height = 6.5)


### For Both Outputs





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

