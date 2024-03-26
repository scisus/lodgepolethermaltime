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
library(ggalt)

theme_set(theme_dark())
factororder <- readRDS("objects/factororder.rds")
factororder_site_so <- factororder$site[-c(1,2)]
shortsites <- c("PGTIS", "KettleRiver", "Sorrento", "Kalamalka")

# forcing and climate ##########
typical_year_forc <- read.csv("data/forcing/typical_year_forc.csv") %>% # from temp mean at each site across 1945-2012
  mutate(Date = as.Date(Date_scale)) %>%
  select(-Date_scale) %>%
  mutate(Site = factor(Site, levels = factororder$site)) %>%
  mutate(Site_type = case_when(Site %in% c("Border", "Trench") ~ "comparison",
                               !Site %in% c("Border", "Trench") ~ "orchard")) %>%
  mutate(Site_type = fct_rev(Site_type)) %>%
  filter(DoY < 180)

meantempplot <- ggplot(typical_year_forc, aes(x = Date, y = mean_temp, color = Site, linetype = Site_type)) +
  geom_hline(yintercept = 5, colour = "dark grey") +
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

# MAT (and replication) #####
sites <- read.csv("../lodgepole_climate/data/climateBC/climatebc_locs_Normal_1961_1990Y.csv") %>%
  filter(id == "site") %>%
  select(Site, MAT) %>%
  mutate(Site = forcats::fct_rev(forcats::fct_reorder(Site, MAT)))
sites$`Site Type` <- c(rep("Seed Orchard", 7), rep("Comparison", 2))

replication_points <- readRDS("objects/replication_points.rds")

provs <- readRDS("objects/phenf.rds") %>%
  select(Tree, MAT, Site, Genotype) %>%
  distinct() %>%
  left_join(replication_points) %>%
  rename('Within Sites' = treestf, 'Across Sites' = sitestf, 'Across Years' = yearstf, Replicated = replicated) %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site))

siteplot <- ggplot(data=sites) +
  geom_point(aes(x = "Sites", y = MAT, shape = `Site Type`)) +
  geom_text_repel(aes(x = "Sites", y = MAT, label = Site), size = 2, point.padding = 0.05, min.segment.length = 0.16) +
  xlab("") +
  ylab("Mean Annual Temperature (\u00B0C)") +
  scale_y_continuous(limits = c(min(sites$MAT), max(sites$MAT))) +
  scale_shape_manual(values = c('Comparison' = 17, 'Seed Orchard' = 16)) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Site MATs") +
  guides(shape = guide_legend(nrow = 2, title.position = 'top'))


provplot <- ggplot(provs, aes(x = Site, y = MAT,
                  shape = `Genotype replication`,
                  fill = `Within Sites`,
                  color = `Across Sites`,
                  group = `Genotype replication`)) +
  geom_quasirandom(varwidth = TRUE, alpha = 0.5) +
  scale_shape_manual(values = c("Not replicated" = 4,
                                "Within sites only" = 17,
                                "Across years only" = 21,
                                "Across years and sites" = 21,
                                "Across years and within sites" = 21,
                                "Across years and sites and within sites" = 21)) +
  scale_fill_manual(values = c("TRUE" = "grey28", "FALSE" = "transparent"),
                    guide = FALSE) +  # Hide separate fill legend
  scale_colour_manual(values = c("TRUE" = "#DF536B", "FALSE" = 'black'),
                      guide = FALSE) +  # Hide separate color legend
  scale_y_continuous(limits = c(min(provs$MAT), max(provs$MAT)), position = "right") +
  ylab("Mean Annual Temperature (\u00B0C)") +
  theme(legend.position = "bottom") +
  ggtitle("Provenance MATs") +
  guides(shape = guide_legend(override.aes = list(fill = c(rep(c("transparent", "grey28"),3)),
                                                  color = c(rep("black", 4 ), rep("#DF536B", 2))),
                              title.position = 'top',
                              nrow = 3))



siteplot + provplot + patchwork::plot_layout(widths = c(1,4)) + patchwork::plot_annotation(tag_levels = 'A')

ggsave("../flowering-cline/figures/MAT.png", width = 7, height = 5, scale = 1.1)


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
  stat_halfeye(position = "dodge", point_interval = "mean_qi", .width = c(0.50, 0.90)) +
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
# ggplot(fepred_allprovs, aes(x = MAT, y = .epred)) +
#   stat_lineribbon(aes(y = .epred, linetype = event), .width = c(.95, .5), show.legend = FALSE) +
#   scale_fill_brewer() +
#   theme_bw() +
#   facet_grid(. ~ Sex) +
#   theme(legend.position = "none") +
#   #ggtitle("Forcing requirements across all provenances", subtitle = "expectation (mean) predictions, caption = "6000 draws from the posterior") +
#   ylab("Accumulated forcing (Growing Degree Days)") +
#   xlab("Mean Annual Temperature (\u00B0C)")

slopes <- readRDS(file = "objects/slopes.rds")

slopesummary <- slopes %>%
  group_by(Sex, event) %>%
  mean_hdci(.value) %>%
  mutate(
    Value_CI = paste0(round(.value, 2), " (", round(.lower, 2), "-", round(.upper, 2), ")")
  ) %>%
  select(-starts_with(".")) %>%
  pivot_wider(names_from = Sex, values_from = Value_CI)
slopesummary
saveRDS(slopesummary, file = "../flowering-cline/tables/slopesummary.rds")

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
# using 95% HDCI, median posterior prediction for each site for the full range of provenances (MATs) using an average year, Genotype, and tree (i.e. using estimated gaussian prior to generate those random effects), but using site specific effects estimated from the model (delta offset). Posterior predictions contain full range of uncertainty because I want the orchard managers to know what to actually expect

fpred_orch_summary <- readRDS("objects/fpred_orch_summary.rds")

# widefpredorchsum <- fpred_orch_summary %>%
#   tidyr::pivot_wider(
#     id_cols = c(MAT, Year, Tree, Genotype, Site, Sex),
#     names_from = event,
#     values_from = c(.prediction, .lower, .upper),
#     names_sep = "."
#   )

ggplot(fpred_orch_summary) +
  # colored ribbons for start and end
  geom_ribbon(aes(x = MAT, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(aes(x = MAT, y = .prediction, colour = event)) +
  # solid ribbon for median flowering period
  #geom_ribbon(data = widefpredorchsum, aes(x = MAT, ymin = .prediction.begin, ymax = .prediction.end), alpha = 0.7) +
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
  )
  #labs(fill = "95% HDPI", colour = "95% HDPI")
ggsave("../flowering-cline/figures/orchpred_gdd.png", width = 3, height = 4)


### DoY #####

doy_annual_pp_sum <- readRDS("objects/doy_annual_pp_sum.rds")
doy_annual_pp_sum$MAT_label <- paste("MAT:", doy_annual_pp_sum$MAT)

# widedoypporchsum <- doy_annual_pp_sum %>%
#   tidyr::pivot_wider(
#     id_cols = c(MAT, Year, Site, Sex),
#     names_from = event,
#     values_from = c(DoY, .lower, .upper),
#     names_sep = "."
#   )
# widedoypporchsum$MAT_label <- paste("MAT:", widedoypporchsum$MAT)

phenf_orchplot <- readRDS("objects/phenf.rds") %>%
  filter(Event_Obs %in% c(2,3), Site %in% shortsites) %>%
  select(-MAT) %>%
  mutate(Site = forcats::fct_relevel(Site, shortsites), Year = as.numeric(Year))

# 2000 draws, 1945-2011 model preds. grey ribbon shows median start to median end, blue and pink ribbons show uncertainty for start and end. Used coldest and warmest source MAT for contrast. Vertical black lines show range of flowering observations in data.
ggplot() +
  geom_line(data = phenf_orchplot, aes(x = Year, y = DoY, group = Year), alpha = 0.9) +
  geom_ribbon(data = doy_annual_pp_sum, aes(x = Year, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(data = doy_annual_pp_sum, aes(x = Year, y = DoY, colour = event)) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  #labs(fill = "95% HDPI", colour = "95% HDPI") +
  #geom_ribbon(data = widedoypporchsum, aes(x = Year, ymin = DoY.begin, ymax = DoY.end), alpha = 0.5) +
  theme_bw() +
  xlab("Year") +
  ylab("Date") +
  facet_grid(Site ~ Sex + MAT_label) +
  scale_y_continuous(
    breaks = seq(1, 365, by = 14),  # Breaks every 2 weeks
    labels = format(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "2 weeks"), "%b %d")) +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 8),
    legend.position = "bottom"
  )
ggsave("../flowering-cline/figures/orchpred_doy.png", width = 8, height = 6)

gdd_orch + doy_orch +
  plot_layout(widths = c(1,2)) +
  plot_annotation(tag_levels = 'A')
ggsave("../flowering-cline/figures/orchpred.png", width = 8, height = 5.5)


### For Both Outputs


# predictions ####

## grand mean posterior predictions ####
fepred <- readRDS("objects/fepred.rds")
fpred <- readRDS("objects/fpred.rds")
# fepred_cenew <- readRDS("objects/fepred_cenew.rds")
# fepred_ceold <- readRDS("objects/fepred_ceold.rds")
#fepred_newsites <- readRDS("objects/fepred_newsites.rds")

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



# fepred_cenew <- readRDS("objects/fepred_cenew.rds")
# fepred_ceold <- readRDS("objects/fepred_ceold.rds")
# fepred_ce <- full_join(fepred_cenew, fepred_ceold)
#
#
# fepred_ceold <- mutate(fepred_ceold, Site = forcats::fct_relevel(Site, factororder_site_so))
# # fepred_ce %>%
# #   sample_frac(0.25) %>%
# #   mutate(Site = forcats::fct_relevel(Site, c(factororder_site_so, "new_Site")))
#
# ggplot(fepred_cenew,
#        aes(x = .epred, y = Sex, fill = Sex)) +
#   stat_halfeye(alpha = 0.7) +
#   scale_fill_okabe_ito() +
#   theme_clean() +
#   facet_grid(. ~ event)

## Home vs away #######
## graphs from lab meeting presentation
doy_typical_home <- readRDS("objects/doy_typical_home.rds")


#home
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

doy_typical_all_at_PGTIS <- readRDS("objects/doy_typical_all_at_PGTIS.rds")
#away
ggplot(doy_typical_all_at_PGTIS, aes(x = intercept, xend = DoY, y=MAT, shape = Sex)) +
  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#0e668b",
    size = 3
  ) +
  facet_grid(Sex ~ event) +
  xlab("Day of Year") +
  ggtitle("Change in flowering day of year expectation with MAT effect", subtitle = "typical year, trees grown at PGTIS") +
  geom_vline(data = doy_typical_all_at_PGTIS, aes(xintercept = intercept)) +
  theme(legend.position = "top")
# When all sources are grown at the same Site (PGTIS), MAT effect reduces overlap

## year to year variation ####
doy_annual_plotting <- readRDS('objects/doy_annual_plotting.rds')
dplot <- readRDS("objects/dplot2.rds")

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


summary_doy_annual <- readRDS("objects/summary_doy_annual.rds")
ggplot(summary_doy_annual, aes(x = Site, y = sd_DoY, color = normal_period)) +
  geom_point() +
  facet_grid(Sex ~ event) +
  labs(title = "Year-to-year variation in flowering phenology at 9 Sites", subtitle = "over two 30-year climate normal periods") +
  theme(legend.position = "bottom") +
  theme_bw()
ggsave("../flowering-cline/figures/year2yearvar.png", width = 10, height = 6, units = "in")


## prediction climate change ####
doy_normal_plotting.rds <- readRDS("objects/doy_normal_plotting.rds")
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

