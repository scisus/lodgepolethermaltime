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
library(scales)

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
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8))

forcplot <- ggplot(typical_year_forc, aes(x = Date, y = forcing, color = Site, linetype = Site_type)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  ggtitle("Daily forcing") +
  scale_color_viridis_d() +
  theme_bw() +
  ylab("Growing Degree Days (\u00B0C)") + xlab("") +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 8))

sumforcplot <- ggplot(typical_year_forc, aes(x = Date, y = sum_forcing, color = Site, linetype = Site_type)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  ggtitle("Forcing accumulation") +
  scale_color_viridis_d() +
  theme_bw() +
  ylab("Growing Degree Days (\u00B0C)") + xlab("") +
  labs(linetype = "Site type", color = "Site") +
  guides(
    color = guide_legend(ncol = 1),        # Color guide with 3 columns
    linetype = guide_legend(ncol = 1)      # Linetype guide with 1 column (stacked vertically)
  ) +
  # Move the legend inside the top-left corner of the plot
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = alpha("white", 0.5)),  # Semi-transparent background

    # Decrease the font size of the legend text and title
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  )
sumforcplot

siteclimplot <- meantempplot / forcplot / sumforcplot +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect')
siteclimplot
ggsave("../flowering-cline/figures/siteclimplot.png", width = 5, height = 7.5)

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

# parameters ####
## means ####
# plot of population means using means from modelparameters.R - better as a table?
intercepts <- readRDS("objects/intercepts.rds")
interceptplot <- ggplot(intercepts, aes(y = fct_rev(event), x = .value, colour = Sex, shape = event)) +
  stat_halfeye(position = "dodge", point_interval = "mean_qi", .width = c(0.50, 0.90)) +
  scale_colour_discrete_c4a_div(palette = "acadia") +
  #labs(title = expression(paste("Mean forcing requirement at 0 \u00B0C (", mu, ")")), caption = "6000 draws from the posterior") +
  ylab("") +
  xlab(expression(paste("Mean forcing accumulation ", mu, " (GDD)"))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
  scale_y_discrete(expand = expansion(add = c(0, .75))) +
  theme_bw(base_size = 9) +
  theme(legend.position = "top") +
  labs(shape = "Event")
interceptplot
#smallmeans <- filter(means, .draw %in% sample(unique(means$.draw), size = 200))
ggsave("../flowering-cline/figures/intercepts.png", width = 6, height = 6)

interceptsummary <- intercepts %>%
  group_by(Sex, event) %>%
  mean_hdci(.value) %>%
  mutate(
    Value_CI = paste0(round(.value), " (", round(.lower), "-", round(.upper), ")")
  ) %>%
  select(-starts_with(".")) %>%
  pivot_wider(names_from = Sex, values_from = Value_CI)

saveRDS(interceptsummary, file = "../flowering-cline/tables/interceptsummary.rds")

### slopes
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
#saveRDS(slopesummary, file = "../flowering-cline/tables/slopesummary.rds")

slopeplot <- ggplot(slopes, aes(y = fct_rev(event), x = .value, colour = Sex, shape = event)) +
  stat_halfeye(position = "dodge", point_interval = "mean_qi", .width = c(0.50, 0.90)) +
  scale_colour_discrete_c4a_div(palette = "acadia") +
  #labs(title = expression(paste("Mean forcing requirement at 0 \u00B0C (", mu, ")")), caption = "6000 draws from the posterior") +
  ylab("") +
  xlab(expression(paste("Provenance MAT effect ", beta, " (GDD/\u00B0C)"))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
  scale_y_discrete(expand = expansion(add = c(0, .75))) +
  theme_bw(base_size = 9) +
  theme(legend.position = "none")

interceptplot + slopeplot +
  plot_layout(widths = c(2,1)) +
  plot_annotation(tag_levels = 'A')
ggsave("../flowering-cline/figures/slopeinterceptgraph.png", width = 7, height = 3)

slopeintercepttable <- cbind(interceptsummary, slopesummary[, c(-1)])
saveRDS(slopeintercepttable, "../flowering-cline/tables/slopeintercepttable.rds")

## sd ####
# plot sd parameters using variation from modelparameters.R
variation <- readRDS("objects/variation.rds") %>% ungroup()
#varplot <- ggplot(variation, aes(y = forcats::fct_rev(event), x = .value, colour = forcats::fct_rev(Sex), shape = event)) +
varplot <- ggplot(variation, aes(y = forcats::fct_rev(event), x = .value, colour = Sex, shape = event)) +
  stat_pointinterval(position = "dodge") +
  #scale_colour_viridis_d(limits = c("FEMALE", "MALE")) +
 # labs(title = "Standard deviation of pop mean & offsets", caption = "6000 draws from the posterior") +
  ylab("") +
  xlab("Standard deviation (GDD)") +
  facet_grid(.variable ~ ., scales = "free_y") +
  guides(shape = "none", colour = guide_legend(nrow=2)) +
  scale_colour_discrete_c4a_div(palette = "acadia") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", legend.justification = "right") +
  geom_vline(xintercept = 0, linetype = 3, colour = 'darkgray')
 # labs(colour = "Sex", shape = "Event")
varplot
#ggsave("../flowering-cline/figures/sd.png", width = 5, height = 4.5)

## offset_medians ####
# plot medians of offset parameters in point clouds (like beeswarm) 6000 draws
offsets_summary <- readRDS("objects/offsets_summary.rds") %>%
  mutate(model = factor(model, levels = c("mb", "fb", "me", "fe")))
offsetplot <- offsets_summary %>%
  select(model, Sex, event, factor, level, .value, .point) %>% distinct() %>%
  ggplot(aes(y=forcats::fct_rev(event), x = .value, colour = Sex, shape = event, group = model)) +
  geom_quasirandom(dodge.width = 1) +
  facet_wrap("factor") +
  scale_colour_discrete_c4a_div(palette = "acadia", guide = "none") +
  ylab("") +
  xlab("Offset median (GDD)") +
  theme_bw(base_size = 10) +
  geom_vline(xintercept = 0, linetype = 3, colour = 'darkgray') +
  theme(axis.ticks.y = element_blank(), legend.position = "bottom", legend.justification = "left") +
  guides(shape = guide_legend(nrow=2)) +
  labs(shape = "Event")
offsetplot
#ggsave("../flowering-cline/figures/offsets_medians.png", width = 6, height = 5)


# interceptplot +
#   (varplot + offsetplot + plot_layout(ncol = 2, widths = c(1,2.5))) +
#   plot_layout(nrow = 2, heights = c(1, 1.5)) +
#   plot_annotation(tag_levels = 'A')

varplot + offsetplot +
  plot_layout(widths = c(1,2.5)) +
  plot_annotation(tag_levels = 'A')

ggsave("../flowering-cline/figures/varoffsets.png", width = 6.5, height = 5)

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

# independent data comparison ###########

indpredsummary <- readRDS('objects/indpredsummary.rds')

## table

# indpredsummary %>%
#   select(Site, provenance, MAT, ssp.) %>%
#   rename(Provenance = provenance, `Provenance MAT` = MAT, subspecies = ssp.) %>%
#   distinct() %>%
#   arrange(`Provenance MAT`)

## graph

# Position the contorta label
label_x <- 240  # Position the label slightly outside the plot
label_y <- 175  # Vertical position near the contorta points

# Subset the data to only include facets with "contorta" points
contorta_data <- subset(indpredsummary, ssp. == "contorta")

ggplot(indpredsummary, aes(x = sum_forcing, y = .prediction)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper, colour = as.character(.width)),
                width = 0.1, alpha = 0.5) +
  geom_point(aes(fill = MAT, shape = Site), size = 2) +
  geom_abline(colour = "darkgrey") +
  facet_grid(Sex ~ event) +
  scale_color_manual(values = c("0.5" = "black", "0.95" = "grey"), name = "HDI") +  # For error bars
  scale_fill_viridis_c(option = "D", name = "Provenance MAT") +  # For points
  scale_shape_manual(values = c("Central BC" = 21, "Central Sweden" = 24)) +  # Custom shapes for ssp.
  theme_bw() +
  xlab("Measured forcing (GDD)") +
  ylab("Predicted forcing (GDD)") +
  # Add segments and label only in facets with contorta points
  geom_segment(data = contorta_data,
               aes(xend = label_x, yend = label_y),
               linetype = "dashed", color = "darkgrey") +
  geom_label(data = contorta_data,
             aes(x = label_x, y = label_y, label = "contorta"),
             size = 2, fill = "white", color = "black", hjust = 0) +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5),  # Correct guide for continuous fill (MAT)
         shape = guide_legend(title.position = "top", title.hjust = 0.5),  # Correct guide for shape
         colour = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 2)))  # Correct guide for colour
ggsave("../flowering-cline/figures/independent_data_comp.png", width = 7, height = 5)

# retrodictions ####
#
## Observed vs retrodicted ##############
fretro_summary <- readRDS("objects/fretro_summary.rds")
censorpal <- c4a("icefire", 3)
ggplot(fretro_summary, aes(x = sum_forcing, y = .prediction, color = censored)) +
  geom_point(alpha = .5, shape = 3) +
  facet_grid(Sex ~ event) +
  geom_abline(color = "grey20") +
  xlab("Observed accumulated forcing (GDD)") +
  ylab("Median retrodicted accumulated forcing (GDD)") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_colour_manual(values = c(censorpal[2], censorpal[3], censorpal[1])) +
  theme(legend.position = "bottom")
ggsave("../flowering-cline/figures/obsvsretro.png", width = 6, height = 5)

# build a diagram to explain table
overlapex <- data.frame(data = c("observation", "model", "model", "model"),
                        label = c("observation interval", "mean in interval", "one sigma overlap", "no overlap"),
                        overlap = c(NA, TRUE, TRUE, FALSE),
                        mean = c(15, 13, 3, 25),
                        .lower = c(5, 8, -2, 20),
                        .upper = c(15, 18, 8, 30))
overlapex$label <- factor(overlapex$label, levels = unique(overlapex$label))

ggplot(overlapex, aes(x = mean, y = forcats::fct_rev(label), xmin = .lower, xmax = .upper, colour = overlap, )) +
  geom_pointinterval(size = 5, linewidth = 3) +
  ylab("") +
  facet_grid(forcats::fct_rev(data) ~ ., scales = "free_y") +
  scale_color_manual(values = c("TRUE" = "#1B9E77", "FALSE" = "#D95F02")) +
  geom_vline(xintercept = c(5,15), colour = "grey", linetype = 2) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom") +
  theme(axis.title.x = element_blank(), # Removes the x-axis title
                axis.text.x = element_blank(),  # Removes the x-axis tick labels
                axis.ticks.x = element_blank()) # Removes the x-axis ticks)

ggsave("../flowering-cline/figures/obsvsretroconceptual.png", width = 4, height = 3)


## GDD predictions for a generic site, year, tree, etc. #################

fpred_orch_avg_summary <- readRDS("objects/fpred_orch_avg_summary.rds")
phenf_orchplot_prov <- readRDS("objects/phenf.rds") %>%
  filter(Event_Obs %in% c(2,3)) %>%
  mutate(Site = forcats::fct_relevel(Site, factororder$site))

ggplot(fpred_orch_avg_summary) +
  # colored ribbons for start and end
  geom_ribbon(aes(x = MAT, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(aes(x = MAT, y = .prediction, colour = event)) +
  #geom_line(data = phenf_orchplot_prov, aes(x = MAT, y = sum_forcing, group = MAT), alpha = 0.9) +
  facet_grid(. ~ Sex) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  theme_bw() +
  ylab("Accumulated forcing (\u00B0C)") +
  xlab("Provenance Mean Annual Temperature (\u00B0C)") +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 8),
    legend.position = "bottom"
  )
ggsave("../flowering-cline/figures/genpred_gdd.png", width = 6, height = 4)


# specific orchard predictions ############
## GDD ##########
###

# using 95% HDCI, median posterior prediction for each site for the full range of provenances (MATs) using an average year, Genotype, and tree (i.e. using estimated gaussian prior to generate those random effects), but using site specific effects estimated from the model (delta offset). Posterior predictions contain full range of uncertainty because I want the orchard managers to know what to actually expect

fpred_orch_summary <- readRDS("objects/fpred_orch_summary.rds")

ggplot(fpred_orch_summary) +
  # colored ribbons for start and end
  geom_ribbon(aes(x = MAT, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(aes(x = MAT, y = .prediction, colour = event)) +
  geom_line(data = phenf_orchplot_prov, aes(x = MAT, y = sum_forcing, group = MAT), alpha = 0.8, linewidth = 0.25) +
  # solid ribbon for median flowering period
  #geom_ribbon(data = widefpredorchsum, aes(x = MAT, ymin = .prediction.begin, ymax = .prediction.end), alpha = 0.7) +
  # outlines of start and end
  #geom_ribbon(data=fpred_orch_summary, aes(x = MAT, ymin = .lower, ymax = .upper, colour = event), fill = "transparent", size = .5, linetype = 3) +
  facet_grid(forcats::fct_rev(Site) ~ Sex) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  theme_bw() +
  ylab("GDD") +
  xlab("Source MAT") +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 7),
    legend.position = "bottom"
  )
  #labs(fill = "95% HDPI", colour = "95% HDPI")
ggsave("../flowering-cline/figures/orchpred_gdd.png", width = 6, height = 7)

ggplot(fpred_orch_summary) +
  # colored ribbons for start and end
  geom_ribbon(aes(x = MAT, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(aes(x = MAT, y = .prediction, colour = event)) +
  geom_line(data = phenf_orchplot_prov, aes(x = MAT, y = sum_forcing, group = MAT), alpha = 0.8, linewidth = 0.25) +
  # solid ribbon for median flowering period
  #geom_ribbon(data = widefpredorchsum, aes(x = MAT, ymin = .prediction.begin, ymax = .prediction.end), alpha = 0.7) +
  # outlines of start and end
  #geom_ribbon(data=fpred_orch_summary, aes(x = MAT, ymin = .lower, ymax = .upper, colour = event), fill = "transparent", size = .5, linetype = 3) +
  facet_grid(forcats::fct_rev(Site) ~ Sex) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  theme_bw() +
  ylab("GDD") +
  xlab("Source MAT") +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 7),
    legend.position = "bottom"
  )
#labs(fill = "95% HDPI", colour = "95% HDPI")

###

## DoY #####

doy_annual_avg_pp_sum <- readRDS("objects/doy_annual_avg_pp_sum.rds") %>%
  filter(.width == 0.95)
doy_annual_avg_pp_sum$MAT_label <- paste("Provenance MAT:", doy_annual_avg_pp_sum$MAT, "\u00B0C")

phenf_orchplot <- readRDS("objects/phenf.rds") %>%
  select(-MAT) %>%
  filter(Event_Obs %in% c(2,3)) %>%
  mutate(Year = as.numeric(Year), Site = forcats::fct_relevel(Site, factororder$site))

# male only
ggplot() +
  geom_line(data = filter(phenf_orchplot, Sex == "MALE"), aes(x = Year, y = DoY, group = Year), alpha = 0.9) +
  geom_ribbon(data = filter(doy_annual_avg_pp_sum, Sex == "MALE"), aes(x = Year, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(data = filter(doy_annual_avg_pp_sum, Sex == "MALE"), aes(x = Year, y = DoY, colour = event)) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  theme_bw() +
  xlab("Year") +
  ylab("Date") +
  ggtitle("Pollen shed (MALE)") +
  facet_grid(forcats::fct_rev(Site) ~ MAT_label) +
  scale_y_continuous(
    breaks = seq(1, 365, by = 14),  # Breaks every 2 weeks
    labels = format(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "2 weeks"), "%b %d")) +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

ggsave("../flowering-cline/figures/orchpred_doy_male.png", width = 6, height = 10)
#

# female only
ggplot() +
  geom_line(data = filter(phenf_orchplot, Sex == "FEMALE"), aes(x = Year, y = DoY, group = Year), alpha = 0.9) +
  geom_ribbon(data = filter(doy_annual_avg_pp_sum, Sex == "FEMALE"), aes(x = Year, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(data = filter(doy_annual_avg_pp_sum, Sex == "FEMALE"), aes(x = Year, y = DoY, colour = event)) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  #labs(fill = "95% HDPI", colour = "95% HDPI") +
  #geom_ribbon(data = widedoypporchsum, aes(x = Year, ymin = DoY.begin, ymax = DoY.end), alpha = 0.5) +
  theme_bw() +
  xlab("Year") +
  ylab("Date") +
  ggtitle("Receptivity (FEMALE)") +
  facet_grid(forcats::fct_rev(Site) ~ MAT_label) +
  scale_y_continuous(
    breaks = seq(1, 365, by = 14),  # Breaks every 2 weeks
    labels = format(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "2 weeks"), "%b %d")) +
  theme(
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(size = 8),
    legend.position = "bottom"
  )
ggsave("../flowering-cline/figures/orchpred_doy_female.png", width = 6, height = 10)

# for paper, do separate graphs for PGTIS and Kalamalka with faceting MAT x Sex

pgtisorch <- ggplot() +
  geom_line(data = filter(phenf_orchplot, Site == "PGTIS"), aes(x = Year, y = DoY, group = Year), alpha = 0.9) +
  geom_ribbon(data = filter(doy_annual_avg_pp_sum, Site == "PGTIS"), aes(x = Year, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(data = filter(doy_annual_avg_pp_sum, Site == "PGTIS"), aes(x = Year, y = DoY, colour = event)) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  theme_bw(base_size = 8) +
  xlab("Year") +
  ylab("Date") +
  ggtitle("PGTIS", subtitle = "1961-1990 normal MAT: 3.9 \u00B0C") +
  facet_grid(Sex ~ MAT_label) +
  scale_y_continuous(
    breaks = seq(1, 365, by = 14),  # Breaks every 2 weeks
    labels = format(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "2 weeks"), "%b %d")) +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text.y = element_text(size = 9),
    legend.position = "bottom"
  )

kalorch <- ggplot() +
  geom_line(data = filter(phenf_orchplot, Site == "Kalamalka"), aes(x = Year, y = DoY, group = Year), alpha = 0.9) +
  geom_ribbon(data = filter(doy_annual_avg_pp_sum, Site == "Kalamalka"), aes(x = Year, ymin = .lower, ymax = .upper, group = event, fill = event), alpha = 0.3) +
  geom_line(data = filter(doy_annual_avg_pp_sum, Site == "Kalamalka"), aes(x = Year, y = DoY, colour = event)) +
  scale_fill_discrete_c4a_div(palette = "icefire") +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  theme_bw(base_size = 8) +
  xlab("Year") +
  ylab("Date") +
  ggtitle("Kalamalka", subtitle = "1961-1990 normal MAT: 8.0 \u00B0C") +
  facet_grid(Sex ~ MAT_label) +
  scale_y_continuous(
    breaks = seq(1, 365, by = 14),  # Breaks every 2 weeks
    labels = format(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "2 weeks"), "%b %d")) +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text.y = element_text(size = 9),
    legend.position = "bottom"
  )

kalorch / pgtisorch +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("../flowering-cline/figures/orchpred_doy.png", width = 6, height = 7)
# gdd_orch + doy_orch +
#   plot_layout(widths = c(1,2)) +
#   plot_annotation(tag_levels = 'A')
# ggsave("../flowering-cline/figures/orchpred.png", width = 8, height = 5.5)


# differences between provenances in flowering at different sites #mean difference between coldest and warmest provenance
warmvscold <- readRDS('objects/warmvscold.rds')
ggplot(warmvscold, aes(x = Site, y = mean_doy_diff, colour = Sex, shape = event)) +
  geom_point(position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = mean_doy_diff - sd_diff, ymax = mean_doy_diff + sd_diff),
                width = 0.2,   # Controls the width of the horizontal lines at the top and bottom of the error bars
                position = position_dodge(0.4)) +
  scale_colour_discrete_c4a_div(palette = "acadia") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=40, vjust=1, hjust=1)) +
  ylab("Mean difference (days)") +
  theme(legend.position = "bottom")
ggsave("../flowering-cline/figures/provdiffdoy.png", width = 5.5, height = 3.25)

# similar patterns of flowering across sites
rank_correlation_wdist <- readRDS('objects/rank_correlation_wdist.rds')
# ggplot(filter(rank_correlation_wdist, Correlation < 1), aes(colour = MAT, x = Distance, y = Correlation)) +
#   geom_smooth(method = "lm", se = TRUE, aes(group = interaction(MAT, Sex, event))) +
#   geom_point(alpha = 0.7) +
#   #ggtitle("Correlation by distance") +
#   facet_grid(Sex ~ event) +
#   scale_colour_discrete_c4a_div(palette = "icefire") +
#   xlab("Distance (km)") +
#   ylab(expression("Correlation (Kendall's" ~ tau ~")")) +
#   theme_bw() +
#   theme(legend.position = "top")
#
ggplot(filter(rank_correlation_wdist, Correlation < 1), aes(x = Distance, y = Correlation)) +
geom_smooth(method = "lm", se = TRUE, color = "grey25", fill = "grey") +
  geom_point(alpha = 0.5, pch = 1) +
  #ggtitle("Correlation by distance") +
  facet_grid(Sex ~ event) +
  scale_colour_discrete_c4a_div(palette = "icefire") +
  xlab("Distance (km)") +
  ylab(expression("Correlation (Kendall's" ~ tau ~")")) +
  theme_bw() +
  theme(legend.position = "top")
ggsave("../flowering-cline/figures/distrankcorr.png", width = 5, height = 4)

corr_model_results <- readRDS('objects/corr_model_results.rds')

#distance (slope) only
corr_model_table <- corr_model_results %>%
  filter(term == "Distance") %>%
  select(-term, -p.value) %>%
  mutate(across(c(r.squared:std.error), \(x) signif(x, digits = 2)))
saveRDS(corr_model_table, '../flowering-cline/tables/corr_model_table.rds')

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
# doy_typical <- readRDS("objects/doy_typical.rds") %>%
#   filter(! Site %in% c("Sorrento", "Tolko", "PRT", "Vernon"))
# expdoy <- ggplot(doy_typical, aes(x = DoY, y = forcats::fct_rev(Sex), color = Sex, shape = event)) +
#   stat_pointinterval() +
#   facet_grid(forcats::fct_rev(Site) ~ .) +
#  # labs(caption = "typical year based on mean daily heat sum accumulation at 7 sites between 1945 and 2012") +
#   xlab("Day of Year") +
#   scale_color_viridis_d() +
#   theme_dark() +
#   theme(strip.text.y.right = element_text(angle = 0),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = "bottom")


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
doy_typical_home <- readRDS("objects/doy_typical_home.rds") %>%
  pivot_longer(cols = c(intercept, DoY), names_to = "proveffect", values_to = "DoY")



#home
# "Change in flowering day of year expectation with MAT effect" typical year, trees grown at home
# ggplot(doy_typical_home, aes(x = intercept, xend = DoY, y=MAT)) +
#   geom_dumbbell(
#     colour = "#a3c4dc",
#     colour_xend = "#0e668b",
#     size = 3
#   ) +
#   facet_grid(Sex ~ event) +
#   xlab("Day of Year") +
#   ggtitle("Home") +
#   theme(legend.position = "top")

homeplot <- ggplot(doy_typical_home, aes(x = DoY, y = MAT, colour = proveffect, group = MAT)) +
  geom_point() +
  geom_line(colour = "darkgrey") +
  facet_grid(Sex ~ event) +
  labs(x = "Day of Year",
       y = "Site and provenance MAT (\u00B0C)",
       title = "Home",
       colour = "") +  # Removes the title of the colour legend
  scale_colour_manual(values = c("#1B9E77", "darkgrey"),
                     labels = c("With provenance effect", "No provenance effect")) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom") +
  coord_flip()


#away
#Change in flowering day of year expectation with MAT effect, typical year, trees grown at PGTIS
doy_typical_all_at_PGTIS <- readRDS("objects/doy_typical_all_at_PGTIS.rds") %>%
  pivot_longer(cols = c(intercept, DoY), names_to = "proveffect", values_to = "DoY")

pgtis_intercepts <- doy_typical_all_at_PGTIS %>%
  filter(proveffect == "intercept") %>%
  select(Sex, event, DoY) %>%
  distinct()

awayplot <- ggplot(doy_typical_all_at_PGTIS, aes(x = DoY, y=MAT, colour = proveffect, group = MAT)) +
  geom_point() +
  geom_vline(data = pgtis_intercepts, aes(xintercept = DoY), colour = "darkgrey", linetype = 3) +
  geom_line(colour = 'darkgrey') +
  facet_grid(Sex ~ event) +
  labs(x = "Day of Year",
       y = "Provenance MAT (\u00B0C)",
       #title = "Away",
       colour = "") +  # Removes the title of the colour legend
  scale_colour_manual(values = c("#1B9E77", "darkgrey"),
                      labels = c("With provenance effect", "No provenance effect")) +
  theme_bw() +
  theme(legend.position = "bottom")
awayplot
ggsave("../flowering-cline/figures/away.png", width = 5, height = 4)

# When all sources are grown at the same Site (PGTIS), MAT effect reduces overlap, increases differences between provenances

homeplot / awayplot +
  patchwork::plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("../flowering-cline/figures/homeaway.png", width = 5, height = 7)

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


## climate change prediction ####
doy_normal_plotting <- readRDS("objects/doy_normal_plotting.rds")
doy_normal_plotting$MATlabel <- paste(doy_normal_plotting$Site, " (", doy_normal_plotting$MAT, "\u00B0C", ")", sep = "")
# ggplot(filter(doy_normal_plotting, event == "begin"), aes(x = scenario, y = DoY, colour = MATlabel, shape = Sex)) +
#   stat_pointinterval(position = "dodge") +
#   stat_pointinterval(data = filter(doy_normal_plotting, event == "end"), position = "dodge") +
#   #scale_y_date(date_breaks = "1 month", date_labels =  "%b") +
#   facet_wrap("period", scales = "free_x", nrow = 1) +
#   theme_bw() +
#   theme(legend.position = "bottom")  +
#   labs(title = "Expectation for flowering period start and end", subtitle = "1951-2100 for two Shared Socioeconomic Pathways", colour = "Site") +
#   xlab("Shared Socioeconomic Pathway") +
#   ylab("Day of Year")

historicalonly <- doy_normal_plotting %>% filter(scenario == "historical") %>%
  rename(type = scenario) %>%
  merge(data.frame(scenario = c("ssp245", "ssp585")))
doy_normal_plotting2 <- doy_normal_plotting %>%
  filter(scenario != "historical") %>%
  mutate(type = "future") %>%
  full_join(historicalonly) %>%
  arrange(desc(MAT)) %>%
  mutate(MATlabel = factor(MATlabel, levels = unique(MATlabel), ordered = TRUE))

#convert DoY to date labels
doy_to_date <- function(doy) {
  # Assuming January 1st is Day 1
  date_labels <- format(as.Date(doy - 1, origin = "2024-01-01"), "%b %d")
  return(date_labels)
}

# 50 and 95% HDPI with 1961-1991 MAT normal
ggplot(filter(doy_normal_plotting2, event == "begin"), aes(x = period, y = DoY, colour = Sex, shape = Sex)) +
  stat_pointinterval(position = "dodge", alpha = 0.5, .width = c(0.50, 0.95)) +
  stat_pointinterval(data = filter(doy_normal_plotting2, event == "end"), position = "dodge", alpha = 0.5, .width = c(0.50, 0.95)) +
  facet_grid(scenario ~ MATlabel, labeller = labeller(scenario = c(ssp245 = "SSP2 4.5 W/m²", ssp585 = "SSP5 8.5 W/m²"))) +
  #scale_color_manual(values = c("historical" = "grey50", "future" = "black"), guide = "none") +
  scale_colour_discrete_c4a_div(palette = "acadia") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_text(vjust = -1))+
  labs(title = "Expectation of flowering period start and end", subtitle = "1951-2100 for two Shared Socioeconomic Pathways") +
  xlab("Normal period") +
  ylab("Day of Year")


ggplot(filter(doy_normal_plotting2, event == "begin"), aes(x = period, y = DoY, colour = Sex, shape = Sex)) +
  stat_pointinterval(position = position_dodge(width = 1), alpha = 0.5, .width = c(0.50, 0.95)) +
  stat_pointinterval(data = filter(doy_normal_plotting2, event == "end"), position = position_dodge(width = 1), alpha = 0.5, .width = c(0.50, 0.95)) +
  facet_grid(scenario ~ MATlabel, labeller = labeller(scenario = c(ssp245 = "SSP2 4.5 W/m²", ssp585 = "SSP5 8.5 W/m²"))) +
  scale_colour_discrete_c4a_div(palette = "acadia") +
  scale_y_continuous(labels = doy_to_date, breaks = seq(100, 200, by = 14)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_text(vjust = -1)) +
  ylab(NULL) +
  xlab("Normal period")

ggsave("../flowering-cline/figures/normal_predictions.png", width = 9, height = 6, units = "in")

## climate change uncertainty
doy_normal_plotting %>%
  group_by(index, Site, event, Sex, period) %>%
  median_qi(DoY, .width = c(.50, 0.95))


