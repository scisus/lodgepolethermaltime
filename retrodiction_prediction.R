# retrodictions

library(dplyr)
library(rstan)
library(tidybayes)
library(magrittr)

source('phenology_functions.R')

# phenology data
phenbe <- filter_start_end()

fbdat <- select_data(phenbe, "FEMALE", "begin") 
fbdat$i <- 1:nrow(fbdat)
# fedat <- select_data(phenbe, "FEMALE", "end")
# mbdat <- select_data(phenbe, "MALE", "begin")
# medat <- select_data(phenbe, "MALE", "end")

# phenology models
fbfit <- readRDS('2020-09-03FEMALE_begin.rds')
# fefit <- readRDS('2020-09-03FEMALE_end.rds')
# mbfit <- readRDS('2020-09-03MALE_begin.rds')
# mefit <- readRDS('2020-09-03MALE_end.rds')

tidybayes::get_variables(fbfit)

fbfit %<>% recover_types(fbdat)
# fefit %<>% recover_types(fedat)
# mbfit %<>% recover_types(mbdat)
# mefit %<>% recover_types(medat)

n <- 1000
seed <- 752

# all group level parameter values (excludes superpopulation parameters mu_* and sigma_*)
fb_factors <- fbfit %>%
  tidybayes::spread_draws(mu, sigma, alpha_site[Site], alpha_prov[Provenance], alpha_year[Year], alpha_clone[Clone], n = n, seed = seed)

fb_yppc <- fbfit %>%
  tidybayes::spread_draws(`y_ppc.*`[i], regex=TRUE, n=n, seed=seed)

# create a dataframe with observed forcing and modeled forcing
retrodiction <- fb_factors %>% 
  select(starts_with("."), Site, Provenance, Year, Clone) %>%
  right_join(fbdat) %>%
  right_join(fb_yppc)

# ggplot(retrodiction, aes(x=sum_forcing, y=y_ppc)) +
#   geom_point(pch=1, alpha=0.5)

# ggplot(retrodiction, aes(x=y_ppc, y=sum_forcing, group=i)) +
#   stat_interval(.width = c(.95, .8, .5), show_point=TRUE, thickness = 0.1, point_colour="black", point_size=0.5) +
#   coord_flip() +
#   geom_abline(slope=1, intercept=0)

# baseplot <- ggplot(retrodiction, aes(x=y_ppc, y=sum_forcing, group=i)) +
#   stat_pointinterval(.width = c(0.95,0.5), alpha=0.1) +
#   coord_flip() 
# 
# baseplot + 
#   geom_abline(slope=1, intercept=0) +
#   ylim(c(250,450)) +
#   xlim(c(250,450)) +
#   theme_classic() +
#   xlab("modeled forcing") +
#   ylab("observed forcing") +
#   ggtitle("Retrodictions for female begin")
  


medianppc <- retrodiction %>%
  dplyr::group_by(i, Site, Provenance, Year) %>%
  dplyr::summarise(sum_forcing = unique(sum_forcing), y_ppc_median = median(y_ppc) )

ggplot(medianppc, aes(x=sum_forcing, y=y_ppc_median)) +
  geom_point(pch=1, alpha=0.7) +
  geom_abline(slope=1, intercept=0) +
  ylim(c(250,450)) +
  xlim(c(250,450)) +
  theme_classic() +
  xlab("observed forcing") +
  ylab("modeled forcing") +
  ggtitle("Retrodictions for female begin") 

# ggplot(fbdat, aes(x=sum_forcing)) +
#   geom_dots() +
#   stat_pointinterval(data=retrodiction, aes(x=y_ppc), .width = c(0.5, 0.95)) +
#   theme_classic(base_size = 18) +
#   ggtitle("Observations and retrodictions")

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
  stat_pointinterval(data=retrodiction, aes(x=y_ppc), .width = c(0.5, 0.95), inherit.aes = FALSE) +
  stat_pointinterval(data=sumf, aes(x=sum_forcing, y = -0.25), .width = c(0.5, 0.95), inherit.aes = FALSE) +
  theme_classic(base_size = 18) +
  annotate("text", label = "retrodiction", x = 200, y = 0, hjust = 0, vjust = 0.3) +
  annotate("text", label = "prediction", x = 200, y = -0.25, hjust = 0, vjust = 0.3) +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Observations, retrodictions, predictions")
