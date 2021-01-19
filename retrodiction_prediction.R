# retrodictions: what does the model say our observations should have been?

library(dplyr)
library(rstan)
library(tidybayes)
library(magrittr)

source('phenology_functions.R')
source('retrodiction_functions.R')

# If you need to work with smaller sample size
n <- 500 # draw n samples from the posterior for each observation
seed <- 752

# Read in Data #############
# climate data
clim <- read.csv("data/all_clim_PCIC.csv") %>% # read in climate data
  dplyr::filter(forcing_type == "ristos")

# phenology data
phenbe <- filter_start_end() # reduce to first and last day of observations

fbdat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 
fbdat$i <- 1:nrow(fbdat)

# fedat <- select_data(phenbe, "FEMALE", "end")
# mbdat <- select_data(phenbe, "MALE", "begin")
# medat <- select_data(phenbe, "MALE", "end")

# phenology models #########
fbfit <- readRDS('2021-01-19FEMALE_begin.rds')
# fefit <- readRDS('2020-09-03FEMALE_end.rds')
# mbfit <- readRDS('2020-09-03MALE_begin.rds')
# mefit <- readRDS('2020-09-03MALE_end.rds')

# extract and format ppc ##########
#modpars <- tidybayes::get_variables(fbfit)

fbfit %<>% recover_types(fbdat)
# fefit %<>% recover_types(fedat)
# mbfit %<>% recover_types(mbdat)
# mefit %<>% recover_types(medat)


fb_rep <- fbfit %>%
  tidybayes::spread_draws(`sum_forcing_rep.*`[i], regex=TRUE, n=n, seed=seed) %>% # y_ppc generated in stan model into tidy df
  #tidybayes::spread_draws(`y_ppc.*`[i], regex=TRUE) %>% # y_ppc generated in stan model into tidy df
  dplyr::left_join(
    dplyr::select(fbdat, Site, Year, i) # add identifying information (Site, Year) from data for matching with climate
  ) 

# convert forcing units to day of year
fb_rep$doy_rep <- forcing_to_doy(a = clim, b = data.frame(fb_rep), aforce = "sum_forcing", bforce = "sum_forcing_rep") 

retrodiction <- left_join(fbdat, fb_rep) # dataframe with observed and modeled sum_forcing and DoY



# HPDI Intervals ###########3
# Calculate HPDI for predicted sum_forcing
intervals <- retrodiction %>%
  group_by(i) %>%
  median_hdi(sum_forcing_rep, .width=c(0.50, 0.75, 0.90)) %>%
  rename(sum_forcing_rep_median = sum_forcing_rep) %>%
  full_join(fbdat) 

# what DoY is associated with each forcing?
intervals$.lower_doy <- forcing_to_doy(clim, intervals, aforce = "sum_forcing", bforce = ".lower") 
intervals$.upper_doy <- forcing_to_doy(clim, intervals, aforce = "sum_forcing", bforce = ".upper") 
intervals$doy_rep_median <- forcing_to_doy(clim, intervals, aforce = "sum_forcing", bforce = "sum_forcing_rep_median") 

# What proportion of observations are within the HDPIs?
intervals <- intervals %>%
  dplyr::mutate(in_forcing_int = case_when(sum_forcing >= .lower & sum_forcing <= .upper ~ TRUE,
                           sum_forcing < .lower | sum_forcing > .upper ~ FALSE),
         in_doy_int = case_when(DoY >= .lower_doy & DoY <= .upper_doy ~ TRUE,
                                DoY < .lower_doy | DoY > .upper_doy ~ FALSE))

# Retrodiction performance ##########
# 
# table
retrodiction_table <- intervals %>%
  group_by(.width) %>%
  summarize(prop_in_forcing_int = sum(in_forcing_int)/n(), 
            prop_in_doy_int = sum(in_doy_int)/n()) %>%
  rename("HDI width" = .width, Forcing = prop_in_forcing_int, "Day of Year" = prop_in_doy_int)

knitr::kable(retrodiction_table) # PAPER

# plot overall retrodiction
ggplot(retrodiction, aes(x = sum_forcing, color="Observed")) +
  geom_density() +
  geom_density(aes(x=sum_forcing_ppc, color="Modeled")) +

ggplot(retrodiction, aes(x=sum_forcing_rep, group = .draw, colour="Modeled")) +
  geom_line(stat="density", alpha = 0.1) +
  geom_density(aes(x = sum_forcing, color="Observed")) +
  scale_color_viridis_d() +
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing") +
  theme_dark() +
  facet_grid(Year ~ Site)
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing")
  

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
  stat_pointinterval(data=retrodiction, aes(x=sum_forcing_ppc), .width = c(0.5, 0.95), inherit.aes = FALSE) +
  stat_pointinterval(data=sumf, aes(x=sum_forcing, y = -0.25), .width = c(0.5, 0.95), inherit.aes = FALSE) +
  theme_classic(base_size = 18) +
  annotate("text", label = "retrodiction", x = 200, y = 0, hjust = 0, vjust = 0.3) +
  annotate("text", label = "prediction", x = 200, y = -0.25, hjust = 0, vjust = 0.3) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Observations, retrodictions, predictions")
