# retrodictions: what does the model say our observations should have been? There are 4 separate models - one each for male and female begin and end of flowering
# this script uses *a lot* of ram. Can reduce by running for each dataset and model separately.

library(dplyr)
library(rstan)
library(tidybayes)
library(magrittr)

source('phenology_functions.R')
source('retrodiction_functions.R')

# Read in Data #############
# climate data
clim <- read.csv("data/all_clim_PCIC.csv") %>% # read in climate data
  dplyr::filter(forcing_type == "ristos")

# phenology data
phenbe <- filter_start_end() # reduce to first and last day of observations

# separate observations for each of the four models
fbdat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 
fbdat$i <- 1:nrow(fbdat)

fedat <- select_data(phenbe, "FEMALE", "end", keep_day = TRUE)
fedat$i <- 1:nrow(fedat)

mbdat <- select_data(phenbe, "MALE", "begin", keep_day = TRUE)
mbdat$i <- 1:nrow(mbdat)

medat <- select_data(phenbe, "MALE", "end", keep_day = TRUE)
medat$i <- 1:nrow(medat)

# Retrodictions ###########
# create a dataframe of retrodictions with associated Site, Provenance, Clone, and Year information

retro.fb <-retrodict(modelfile = '2021-01-19FEMALE_begin.rds', dat=fbdat, climate=clim)
retro.fe <- retrodict(modelfile = '2021-01-19FEMALE_end.rds', dat=fedat, climate=clim)
retro.mb <- retrodict(modelfile = '2021-01-19MALE_begin.rds', dat=mbdat, climate=clim)
retro.me <- retrodict(modelfile = '2021-01-19MALE_end.rds', dat=medat, climate=clim)


# HPDI Intervals ###########

# calculate HPDIs for each group, determine Day of Year associated with HPDIs, and calculate whether group observations are within intervals for day and sum_forcing

interval.fb <- intervalate(retrodictions = retro.fb, climate = clim, dat = fbdat)
interval.fe <- intervalate(retro.fe, clim, fedat)
interval.mb <- intervalate(retro.mb, clim, mbdat)
interval.me <- intervalate(retro.me, clim, medat)


# Retrodiction performance ##########
# 
# Plots and tables
# tableS


tab.fb <- retrotable(interval.fb)
knitr::kable(tab.fb, caption = "FEMALE begin", digits = 2)
tab.fe <- retrotable(interval.fe)
knitr::kable(tab.fe, caption = "FEMALE end", digits = 2)
tab.mb <- retrotable(interval.mb)
knitr::kable(tab.mb, caption = "MALE begin", digits = 2)
tab.me <- retrotable(interval.me)
knitr::kable(tab.me, caption = "MALE end", digits = 2)


########### PLOT RETRODICTIONS ##################

retrodf <- list("FEMALE begin" = retro.fb, "FEMALE end" = retro.fe, "MALE begin" = retro.mb, "MALE end" = retro.me) %>%
  dplyr::bind_rows(.id = ".id") %>%
  tidyr::separate(.id, into=c("Sex", "event"))

datdf <- list("FEMALE begin" = fbdat, "FEMALE end" = fedat, "MALE begin" = mbdat, "MALE end" = medat) %>%
  dplyr::bind_rows(.id = ".id") %>%
  tidyr::separate(.id, into=c("Sex", "event"))

lildf <- retrodf %>% 
  group_by(Sex, event) %>%
  sample_n(size = 1e4)

# Forcing PAPER
ggplot(lildf, aes(x=sum_forcing_rep, colour = "Model", fill="Model")) +
  stat_slab(alpha =0.5) +
  facet_grid(event ~ Sex) +
  geom_dots(data=datdf, aes(x=sum_forcing, color = "Observations"), alpha = 0.3, pch = 1) +
  stat_pointinterval(point_interval = median_hdi, .width = c(.5, .9)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  theme_dark() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Accumulated forcing required to begin and end flowering", subtitle = "1000 samples from each model") +
  xlab("Accumulated forcing") +
  ylab("") 

# lilsum <- lildf %>%
#   group_by(event, Sex, Site, Year, DoY) %>%
#   summarise(count = n())
# 
# ggplot(filter(lildf, Year %in% c(2006,2011)), aes(x=doy_rep, y=interaction(event, Sex), colour = "Model")) +
#   facet_wrap(~ Site + Year, ncol = 10, scales = "free_x") +
#   geom_boxplot(fill=NA, size = 2) +
#   stat_pointinterval(data=filter(datdf, Year %in% c(2006,2011)), aes(x=DoY, y=interaction(event, Sex), color = "Observations"), .width = c(0.5, 1), alpha =0.5) +
#   scale_colour_viridis_d() +
#   scale_fill_viridis_c() +
#   theme_dark() +
#   ggtitle("Flowering Day of Year", subtitle = "Model DoY from 50 & 90% HDI accumulated forcing, median and range of obs") +
#   xlab("Day of Year") +
#   coord_flip()
# 
# ggplot(filter(lilsum, Year == 2006), aes(x = Site, y=DoY, size = count)) +
#   geom_jitter() +
#   facet_grid(event ~ Sex) + 
#   geom_jitter(datdf)

ggplot(lildf, aes(x = DoY, y = doy_rep, colour = Site, group=Year)) +
  stat_pointinterval(alpha = 0.5, position = "jitter") +
  facet_grid(event ~ Sex) +
  geom_abline(slope = 1, intercept=0) +
  scale_color_viridis_d() +
  theme_dark()






# plot median observed vs. modeled
# 
medians_only <- intervals %>%
  dplyr::filter(.width == 0.5) # no duplicate obs
  
ggplot(medians_only, aes(x = sum_forcing, y=sum_forcing_rep_median)) +
  geom_point(pch=1, alpha = 0.5) +
  ggtitle("Observed sum forcing vs. median modeled sum forcing") +
  geom_abline(slope=1, intercept = 0) +
  theme_bw()

ggplot(medians_only, aes(x = DoY, y=doy_rep_median)) +
  geom_point(pch=1, alpha = 0.5) +
  ggtitle("Observed Day of Year vs. median Day of Year") +
  geom_abline(slope=1, intercept = 0) +
  theme_bw()


# plot overall retrodiction #PAPER?
ggplot(retrodiction, aes(x=sum_forcing_rep, group = .draw, colour="Modeled")) +
  geom_line(stat="density", alpha = 0.1) +
  geom_density(aes(x = sum_forcing, color="Observed")) +
  scale_color_viridis_d() +
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing") +
  theme_dark() 

ggplot(retrodiction, aes(x="", y=sum_forcing_rep, color = "Model")) +
  ggbeeswarm::geom_quasirandom(data=fbdat, aes(x = "", y= sum_forcing, color = "Data"), pch=1, alpha = 0.75) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill=NA) +
  theme_dark() +
  scale_colour_viridis_d()

ggplot(retrodiction, aes(x=as.factor(Year), y=sum_forcing_rep, colour = "Model")) +
  ggbeeswarm::geom_quasirandom(data=fbdat, aes(x = as.factor(Year), y= sum_forcing, colour = "Data"), groupOnX = TRUE, pch=1) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill=NA) +
  theme_dark() +
  scale_colour_viridis_d() +
  # scale_fill_viridis_d(alpha = 0.5) +
  facet_wrap("Site", scales="free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

retro6 <- filter(retrodiction, Year == 2006)
dat6 <- filter(fbdat, Year == 2006)

ggplot(retro6, aes(x=Site, y=sum_forcing_rep, colour = "Model")) +
  ggbeeswarm::geom_quasirandom(data=dat6, aes(x = Site, y= sum_forcing, colour = "Data"), groupOnX = TRUE, pch=1) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill=NA) +
  theme_dark() +
  scale_colour_viridis_d() +
  # scale_fill_viridis_d(alpha = 0.5) +
  facet_wrap("Provenance", scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(retrodiction, aes(x=sum_forcing, colour = "Observed")) +
  stat_ecdf() +
  stat_pointinterval(data = retrodiction, aes(x=sum_forcing_rep, y=0.5, colour = "Modeled"), point_interval = median_hdi, .width = c(0.5, 0.75, 0.95), size = c(10, 5, 1)) +
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing") +
  theme_dark() +
  scale_colour_viridis_d() 

ggplot(retrodiction, aes(x=sum_forcing_rep, group = .draw, colour="Modeled")) +
  geom_line(stat="ecdf", alpha = 0.1) +
  stat_ecdf(aes(x = sum_forcing, color="Observed")) +
  scale_color_viridis_d() +
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing") +
  theme_dark() 

# retrodictions by site
ggplot(retrodiction, aes(x=sum_forcing_rep, group = .draw, colour="Modeled")) +
  geom_line(stat="density", alpha = 0.1) +
  geom_density(aes(x = sum_forcing, color="Observed")) +
  scale_color_viridis_d() +
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing") +
  theme_dark() +
  facet_wrap("Site")

ggplot(retrodiction, aes(x=sum_forcing_rep, group = .draw, colour="Modeled")) +
  geom_line(stat="density", alpha = 0.1) +
  geom_density(aes(x = sum_forcing, color="Observed")) +
  scale_color_viridis_d() +
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing") +
  theme_dark() +
  facet_wrap("Year")

# fstat and group means ########

facs <- c("Site", "Provenance", "Year", "Clone")

## fstats for observations ########

fstat_obs.fb <- calculate_fstat_obs(fbdat)
fstat_obs.fe <- calculate_fstat_obs(fedat)
fstat_obs.mb <- calculate_fstat_obs(mbdat)
fstat_obs.me <- calculate_fstat_obs(medat)

## fstats for models #########

options(dplyr.summarise.inform = FALSE) # prevent dplyr from printing thousands of messages

fstat_mod.fb <- calculate_fstat_mod(retro.fb)
fstat_mod.fe <- calculate_fstat_mod(retro.fe)
fstat_mod.mb <- calculate_fstat_mod(retro.mb)
fstat_mod.me <- calculate_fstat_mod(retro.me)

plot_fstats <- function(df, model) {
  
  plot <- ggplot(df, aes(x= F_statistic)) +
    geom_histogram(bins = 50) +
    facet_grid(factors ~ y, scales = "free") +
    geom_vline(data = fstat_obs, aes(xintercept = F_statistic)) +
    ggtitle(model)
  
  print(plot)
}


plot_fstats(fstat_mod.fb, "Female begin")
plot_fstats(fstat_mod.fe, "Female end")
plot_fstats(fstat_mod.mb, "Male begin")
plot_fstats(fstat_mod.me, "Male end")

# Female begin looks good, but all the rest show mismatches (though never for clone). Suggests model isn't capturing heterogenity
# 
# level means ########

# plot level means of y (an outcome) calculated from the data (observations df with factor and y column) and for each draw in the model (mcmc dataframe with .draw, factor, and y_rep column). 


#forcing

plot_factorlevel_means <- function(retrodictions, obs, y, y_rep) {
  
  siteplot <- plot_level_means(retrodictions, obs, Site, {{y}}, {{y_rep}})
  print(siteplot)
  
  provplot <- plot_level_means(retrodictions, obs, Provenance, {{y}}, {{y_rep}})
  print(provplot)
  
  yearplot <- plot_level_means(retrodictions, obs, Year, {{y}}, {{y_rep}})
  print(yearplot)
  
  clonesample <- sample(unique(obs$Clone), 20)
  cloneplot <- plot_level_means(filter(retrodictions, Clone %in% clonesample), filter(obs, Clone %in% clonesample), Clone, {{y}}, {{y_rep}})
  print(cloneplot)
  
}

# forcing
plot_factorlevel_means(retro.fb, fbdat, sum_forcing, sum_forcing_rep)
plot_factorlevel_means(retro.fe, fedat, sum_forcing, sum_forcing_rep)
plot_factorlevel_means(retro.mb, mbdat, sum_forcing, sum_forcing_rep)
plot_factorlevel_means(retro.me, medat, sum_forcing, sum_forcing_rep)

#doy
plot_factorlevel_means(retro.fb, fbdat, DoY, doy_rep)
plot_factorlevel_means(retro.fe, fedat, DoY, doy_rep)
plot_factorlevel_means(retro.mb, mbdat, DoY, doy_rep)
plot_factorlevel_means(retro.me, medat, DoY, doy_rep)
# calculate predictions #############

# superpopulations
# 
# 
#all group level parameter values (excludes superpopulation parameters mu_* and sigma_*)
fb_factors <- fbfit %>%
  tidybayes::spread_draws(mu, sigma, alpha_site[Site], alpha_prov[Provenance], alpha_year[Year], alpha_clone[Clone], n = n, seed = seed)
# 
fb_super <- fbfit %>%
tidybayes::spread_draws(mu, sigma, mu_site, mu_prov, mu_year, mu_clone, sigma_site, sigma_prov, sigma_year, sigma_clone, n = n, seed = seed)

alpha_site <- rnorm(nrow(fb_super), fb_super$mu_site, fb_super$sigma_site)
alpha_prov <- rnorm(nrow(fb_super), fb_super$mu_prov, fb_super$sigma_prov)
alpha_year <- rnorm(nrow(fb_super), fb_super$mu_year, fb_super$sigma_year)
alpha_clone <- rnorm(nrow(fb_super), fb_super$mu_clone, fb_super$sigma_clone)

sumf <- rnorm(nrow(fb_super)*30, mean = fb_super$mu + alpha_site + alpha_prov + alpha_year + alpha_clone, sd = fb_super$sigma) 
sumf <- data.frame(sum_forcing = sumf)

ggplot(fbdat, aes(x=sum_forcing, colour=Provenance)) +
  geom_dots() +
  stat_pointinterval(data=retrodiction, aes(x=sum_forcing_rep), .width = c(0.5, 0.95), inherit.aes = FALSE) +
  stat_pointinterval(data=sumf, aes(x=sum_forcing, y = -0.25), .width = c(0.5, 0.95), inherit.aes = FALSE) +
  theme_classic(base_size = 18) +
  annotate("text", label = "retrodiction", x = 200, y = 0, hjust = 0, vjust = 0.3) +
  annotate("text", label = "prediction", x = 200, y = -0.25, hjust = 0, vjust = 0.3) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Observations, retrodictions, predictions")
