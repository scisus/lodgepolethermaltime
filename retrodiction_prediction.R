# retrodictions: what does the model say our observations should have been?

library(dplyr)
library(rstan)
library(tidybayes)
library(magrittr)

source('phenology_functions.R')
source('retrodiction_functions.R')

# If you need to work with smaller sample size

# Read in Data #############
# climate data
clim <- read.csv("data/all_clim_PCIC.csv") %>% # read in climate data
  dplyr::filter(forcing_type == "ristos")

# phenology data
phenbe <- filter_start_end() # reduce to first and last day of observations

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
# 
# table
# 
retrotable <- function(intervaldf) {
  tab <- intervaldf %>%
    group_by(.width) %>%
    summarize(prop_in_forcing_int = sum(in_forcing_int)/n(), 
              prop_in_doy_int = sum(in_doy_int)/n()) %>%
    rename("HDPI width" = .width, Forcing = prop_in_forcing_int, "Day of Year" = prop_in_doy_int)
  
  return(tab) # PAPER
}


tab.fb <- retrotable(interval.fb)
knitr::kable(tab.fb, caption = "FEMALE begin")
tab.fe <- retrotable(interval.fe)
knitr::kable(tab.fe, caption = "FEMALE end")
tab.mb <- retrotable(interval.mb)
tab.me <- retrotable(interval.me)


########### STOP


retrotabler <- intervals %>%
  group_by(.width) %>%
  summarize(prop_in_forcing_int = sum(in_forcing_int)/n(), 
            prop_in_doy_int = sum(in_doy_int)/n()) %>%
  rename("HDI width" = .width, Forcing = prop_in_forcing_int, "Day of Year" = prop_in_doy_int)

knitr::kable(retrodiction_table) # PAPER




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
