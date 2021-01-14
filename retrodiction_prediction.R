# retrodictions: what does the model say our observations should have been?

library(dplyr)
library(rstan)
library(tidybayes)
library(magrittr)

source('phenology_functions.R')
source('retrodiction_functions.R')

# phenology data
phenbe <- filter_start_end()

fbdat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 
ggplot(fbdat, aes(x=sum_forcing, colour=Year)) +
  geom_density() 
fbdat$i <- 1:nrow(fbdat)

# fedat <- select_data(phenbe, "FEMALE", "end")
# mbdat <- select_data(phenbe, "MALE", "begin")
# medat <- select_data(phenbe, "MALE", "end")

# phenology models
fbfit <- readRDS('2021-01-07FEMALE_begin.rds')
# fefit <- readRDS('2020-09-03FEMALE_end.rds')
# mbfit <- readRDS('2020-09-03MALE_begin.rds')
# mefit <- readRDS('2020-09-03MALE_end.rds')

modpars <- tidybayes::get_variables(fbfit)

fbfit %<>% recover_types(fbdat)
# fefit %<>% recover_types(fedat)
# mbfit %<>% recover_types(mbdat)
# mefit %<>% recover_types(medat)


# n <- 436 # draw n samples from the posterior for each observation
# seed <- 752

# all group level parameter values (excludes superpopulation parameters mu_* and sigma_*) MOVE TO PREDICTION
# fb_factors <- fbfit %>%
#   tidybayes::spread_draws(mu, sigma, alpha_site[Site], alpha_prov[Provenance], alpha_year[Year], alpha_clone[Clone], n = n, seed = seed)


fb_yppc <- fbfit %>%
  #tidybayes::spread_draws(`y_ppc.*`[i], regex=TRUE, n=n, seed=seed) %>% # y_ppc generated in stan model into tidy df
  tidybayes::spread_draws(`y_ppc.*`[i], regex=TRUE) %>% # y_ppc generated in stan model into tidy df
  dplyr::left_join(
    dplyr::select(fbdat, Site, Year, i) # add identifying information (Site, Year) from data for matching with climate
  ) %>%
  dplyr::rename(sum_forcing_ppc = y_ppc)


# add DoY for modeled sum_forcing
clim <- read.csv("data/all_clim_PCIC.csv") %>% # read in climate data
  dplyr::filter(forcing_type == "ristos")


fb_yppc <- forcing_to_doy(a = clim, b = data.frame(fb_yppc), aforce = "sum_forcing", bforce = "sum_forcing_ppc", new_doy_col = "DoY_ppc")  

retrodiction <- left_join(fbdat, fb_yppc) # dataframe with observed and modeled sum_forcing and DoY


# Calculate HPDI for predicted sum_forcing
intervals <- retrodiction %>%
  group_by(i) %>%
  median_hdi(sum_forcing_ppc, .width=c(0.50, 0.75, 0.90)) %>%
  full_join(fbdat) 

# what DoY is associated with each forcing?
intervals <- forcing_to_doy(clim, intervals, aforce = "sum_forcing", bforce = ".lower", new_doy_col = ".lower_day")
intervals <- forcing_to_doy(clim, intervals, aforce = "sum_forcing", bforce = ".upper", new_doy_col = ".upper_day")
intervals <- forcing_to_doy(clim, intervals, aforce = "sum_forcing", bforce = "sum_forcing_ppc", new_doy_col = "doy_ppc")

# What proportion of observations are within the HDPIs?
intervals <- intervals %>%
  dplyr::mutate(in_forcing_int = case_when(sum_forcing >= .lower & sum_forcing <= .upper ~ TRUE,
                           sum_forcing < .lower | sum_forcing > .upper ~ FALSE),
         in_doy_int = case_when(DoY >= .lower_day & DoY <= .upper_day ~ TRUE,
                                DoY < .lower_day | DoY > .upper_day ~ FALSE))

  

# Add DoY estimates for forcing ppc and interval columns

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
  scale_color_viridis_d() +
  theme_dark() +
  ggtitle("Retrodictions: receptivity begin", subtitle = "Actual observations and modeled observations") +
  ylab("") +
  xlab("Accumulated Forcing")
  

# calculate predictions

# superpopulations
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
