# Plot effects
# 
library(tidybayes)
library(ggplot2)

source('phenology_functions.R')
source('retrodiction_functions.R')

# climate data
clim <- read.csv("data/all_clim_PCIC.csv") %>% # read in climate data
  dplyr::filter(forcing_type == "ristos")

matprov <- data.frame(Provenance = c("Thompson Okanagan Low", "Nelson Low", "Bulkley Valley Low", "Thompson Okanagan High", "Nelson High", "Central Plateau Low", "Prince George Low"), MAT = c(4.8, 3.7, 2.3, 2.7, 1.3, 1.3, 2.8) ) %>%
  dplyr::arrange(MAT)

matsite <- data.frame(Site = c("PGTIS", "Kalamalka", "Tolko", "PRT", "Vernon", "KettleRiver", "Sorrento"), MAT = c(4, 7.9, 7, 7.5, 7.9, 6.7, 6.8)) %>%
  dplyr::arrange(MAT)

# phenology data
phenbe <- filter_start_end() # reduce to first and last day of observations

# separate observations for each of the four models
dat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 
dat$i <- 1:nrow(fbdat)

seed = 618
n = 50

modelfile <- '2021-02-26FEMALE_begin.rds'
fit <- readRDS(modelfile)

get_variables(fit)

fit %<>% recover_types(dat)

basepars <- fit %>%
  tidybayes::spread_draws(mu, sigma, n=n, seed=seed)

pars <- fit %>%
  tidybayes::spread_draws(alpha_site[Site], alpha_prov[Provenance], alpha_year[Year], alpha_clone[Clone], n = n, seed = seed) %>% # all factor combos
  dplyr::left_join(basepars)

parsdat <- left_join(dat, pars) # only factor combos with data

ggplot(parsdat, aes(x=mu)) +
  stat_interval(.width = c(.90, 0.75, 0.5))

ggplot(pars, aes(x=mu)) +
  stat_histinterval(.width = c(0.90, 0.75, 0.5))

pars$Site <- ordered(pars$Site, levels = matsite$Site)
pars$Provenance <- ordered(pars$Provenance, levels = matprov$Provenance)

basesd <- mean(basepars$sigma)
ggplot(pars, aes(x = mu)) +
  stat_histinterval(c()) 

ggplot(pars, aes(x = alpha_site, y = Site, fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

ggplot(pars, aes(x = alpha_prov, y = Provenance, fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

ggplot(pars, aes(x = alpha_year, y = Year, fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

clonesample <- sample(unique(dat$Clone), 20)
ggplot(filter(pars, Clone %in% clonesample), aes(x = alpha_clone, y = Clone, fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

head(pars)

# calculate delta_doy
temp <- pars %>% 
  mutate(baseplussite = alpha_site + mu, baseplusprov = alpha_prov + mu)

temp <- forcing_to_doy(clim, temp, "sum_forcing", "baseplussite", "sitedoy")
temp <- forcing_to_doy(clim, temp, "sum_forcing", "baseplusprov", "provdoy")
temp <- forcing_to_doy(clim, temp, "sum_forcing", "mu", "basedoy")


temp <- temp %>%
  mutate(delta_site_doy = basedoy - sitedoy, delta_prov_doy = basedoy - provdoy)

ggplot(temp, aes(x=basedoy)) +
  stat_histinterval()

ggplot(temp, aes(x = delta_site_doy, y = Site)) +
  stat_histinterval(alpha = 0.5) +
  facet_wrap("Year")

ggplot(temp, aes(x = delta_prov_doy, y = Provenance)) +
  stat_histinterval(alpha = 0.5) +
  facet_wrap("Year")
