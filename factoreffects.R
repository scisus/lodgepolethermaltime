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

poppars <- fit %>%
  tidybayes::spread_draws(alpha_site[Site], alpha_prov[Provenance], alpha_year[Year], alpha_clone[Clone], n = n, seed = seed) %>% # all factor combos
  dplyr::left_join(basepars)

popparsdat <- left_join(dat, poppars) # only factor combos with data

ggplot(popparsdat, aes(x=mu)) +
  stat_interval(.width = c(.90, 0.75, 0.5))

ggplot(poppars, aes(x=mu)) +
  stat_histinterval(.width = c(0.90, 0.75, 0.5))

poppars$Site <- ordered(poppars$Site, levels = matsite$Site)
poppars$Provenance <- ordered(poppars$Provenance, levels = matprov$Provenance)

basesd <- mean(basepars$sigma)
ggplot(poppars, aes(x = mu)) +
  stat_histinterval(c()) 

ggplot(poppars, aes(x = alpha_site, y = forcats::fct_rev(Site), fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

ggplot(poppars, aes(x = alpha_prov, y = forcats::fct_rev(Provenance), fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

ggplot(poppars, aes(x = alpha_year, y = Year, fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

clonesample <- sample(unique(dat$Clone), 20)
ggplot(filter(poppars, Clone %in% clonesample), aes(x = alpha_clone, y = Clone, fill = stat(abs(x) < basesd))) +
  stat_histinterval() +
  geom_vline(xintercept = c(-basesd, basesd), linetype = "dashed") +
  scale_fill_manual(values = c("gray80", "skyblue"))

head(poppars)

# calculate delta_doy
daypop <- poppars %>% 
  mutate(baseplussite = alpha_site + mu, baseplusprov = alpha_prov + mu, baseplusyear = alpha_year + mu, baseplusclone = alpha_clone + mu)

daypop <- forcing_to_doy(clim, daypop, "sum_forcing", "mu", "basedoy")

#climdf must have "sum_forcing" column
add_delta_day <- function(climdf, forcingdf, baseplus, deltacol) {
  
  daypop <- forcing_to_doy(climdf, forcingdf, "sum_forcing", baseplus, "doyplus") %>% # calculate day of year
    mutate("{{deltacol}}" := doyplus - basedoy) %>% # calculate day of year difference
    dplyr::select(-{{baseplus}}, -doyplus) # drop unneeded cols
  
  return(daypop)
}

daypop <- add_delta_day(clim, daypop, "baseplussite", delta_day_site)
daypop <- add_delta_day(clim, daypop, "baseplusprov", delta_day_prov)
daypop <- add_delta_day(clim, daypop, "baseplusyear", delta_day_year)
daypop <- add_delta_day(clim, daypop, "baseplusclone", delta_day_clone)

# consider only m siteyear combinations
daypop$SiteYear <- paste0(daypop$Site, daypop$Year)

m = 30
siteyearsample <- sample(unique(daypop$SiteYear), 30)
daypop <- daypop %>%
  dplyr::filter(SiteYear %in% siteyearsample)

ggplot(daypop, aes(x=basedoy)) +
  stat_histinterval() +
  ggtitle("Baseline Day of Year")

plot_day_shift <- function(df, fac, deltacol) {
  
  p <- ggplot(df, aes(x = {{deltacol}}, y = interaction(Site, Year))) +
    stat_histinterval(alpha = 0.5) +
    facet_wrap(fac)
  print(p)
}

plot_day_shift(daypop, "Site", delta_day_site)
plot_day_shift(daypop, "Provenance", delta_day_prov)
ggplot(temp, aes(x = delta_prov_doy, y = Provenance)) +
  stat_histinterval(alpha = 0.5) +
  facet_wrap("Year")
