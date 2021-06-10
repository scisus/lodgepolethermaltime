library(flowers)
library(dplyr)
library(brms)
library(bayesplot)
library(tidyr)

source('phenology_functions.R')

# pull phenology data from package
phendat <- flowers::lodgepole_phenology_event

## 4 trees were observed by both Wagner and Walsh at PGTIS in 2006 - drop 1 copy of them (16 duplicate observations).
rmidx <- phendat %>%
  group_by(Index) %>%
  summarize(count = n()) %>%
  filter(count > 4)

#add information about censoring
phen <- phendat %>%
  filter(! (Index %in% rmidx$Index & Source == "Rita Wagner")) %>%
  add_censor_indicator() %>% # add censoring type
  mutate(censored_lronly = case_when(censored == "interval" ~ "none",
                                     censored %in% c("left", "right") ~ censored)) %>% # exclude interval censoring
  # add bound labels for interval censoring models
  mutate(bound = case_when(Event_Obs == 1 | Event_Obs == 2 & censored == "left" ~ "lower",
                           Event_Obs == 2 & censored == "interval" ~ "upper",
                           Event_Obs == 3 ~ "lower",
                           Event_Obs == 4 ~ "upper"))

# phen <- phendat %>%
#   filter(! (Index %in% rmidx$Index & Source == "Rita Wagner")) %>%
#   add_censor_indicator() %>% # add censoring type
#   mutate(censored_lronly = case_when(censored == "interval" ~ "none",
#                                      censored %in% c("left", "right") ~ censored)) %>% # exclude interval censoring
#   # add bound labels for interval censoring models
#   mutate(bound = case_when(Event_Obs == 1 ~ "lower",
#                            Event_Obs == 2 ~ "upper",
#                            Event_Obs == 3 ~ "lower",
#                            Event_Obs == 4 ~ "upper"))

# add forcing information
clim <- "data/all_clim_PCIC.csv"
forcingtype <- "ristos"

spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
  select(SPU_Name, Orchard) # provenance information for each orchard in phen
forcing <- read.csv(clim, header=TRUE, stringsAsFactors = FALSE) %>%
  filter(forcing_type==forcingtype)  # ristos consider forcing units calculated based on work of Sarvas 1972

phenf <- phen %>%
    dplyr::left_join(forcing) %>%
    dplyr::left_join(spus) %>%
    dplyr::mutate(Year = as.character(Year), Clone = as.character(Clone)) %>%
    dplyr::rename(Provenance = SPU_Name) %>%
    distinct()

factors <- c("Site", "Provenance", "Year", "Clone")

fbdat <- phenf %>% filter(Sex == "FEMALE" & Event_Obs == 2) %>%
  mutate(sum_forcing_centered = sum_forcing - mean(sum_forcing))
#
# fedat <- phenf %>% filter(Sex == "FEMALE" & Event_Obs == 3) %>%
#   mutate(sum_forcing_centered = sum_forcing - mean(sum_forcing))

# fit the simplest model possible (mean only)

mo <- brm(sum_forcing_centered ~ 0 + Intercept, data = fbdat,
          prior = c(prior("normal(0,50)", class = "b"),
                    prior("normal(0,5)", class = "sigma")),
          cores = 5)
get_prior(sum_forcing_centered ~ 0 + Intercept, data = fbdat,
    prior = c(prior(normal(0,50), class = "b"),
              prior(normal(0,5), class = "sigma")))

make_stancode(sum_forcing_centered ~ 0 + Intercept, data = fbdat,
              prior = c(prior(normal(0,50), class = "b"),
                        prior(normal(0,5), class = "sigma")))
summary(mo)
plot(mo)
mo_yrep <- posterior_predict(mo, draws = 500)

color_scheme_set("brightblue")
ppc_dens_overlay(fbdat$sum_forcing_centered, mo_yrep[1:50,])

loo_mo <- loo(mo)

# # female end
# mo_fe <- brm(sum_forcing_centered ~ 1, data = fedat)
# summary(mo_fe)
# fit a model with left censoring

moc <- brm(sum_forcing_centered | cens(censored_lronly) ~ 0 + Intercept, data = fbdat,
           priorscores = 5)
summary(moc)
plot(moc)
moc_yrep <- posterior_predict(moc, draws = 500)

color_scheme_set("purple")
ppc_dens_overlay(fbdat$sum_forcing_centered, moc_yrep[1:50,])

# compare
loo_moc <- loo(moc)

# fit a model with left and interval censoring
fbdat_mean <- mean(fbdat$sum_forcing)
fbdat_cens <- phenf %>%
  filter(Sex == "FEMALE" & Event_Obs %in% c(1,2)) %>%
  mutate(sum_forcing_centered = sum_forcing - fbdat_mean) %>%
  select(-DoY, -Date, -State, -contains("Event"), -mean_temp, -forcing, -sum_forcing) %>%
  tidyr::pivot_wider(names_from = bound, values_from = sum_forcing_centered, values_fill = 0)
#fb_y2 <- fbdat_cens$upper[which(!is.na(fbdat_cens$upper))]
#y | cens(censored, y2) ~ predictors
n_chains = 4
init_ll <- init_ll <- lapply(1:n_chains, function(id) list(sigma = abs(rnorm(1,50,10)) ))
mocf <- brm(lower | cens(censored, upper) ~ 1, data = fbdat_cens, cores = 5, chains = n_chains, inits = init_ll)

summary(mocf)
plot(mocf)
mocf_yrep <- posterior_predict(mocf, draws = 500)

color_scheme_set("purple")
ppc_dens_overlay(fbdat_cens$lower, mocf_yrep[1:50,])
ppc_dens_overlay(fbdat$sum_forcing_centered, mocf_yrep[1:50,])

# compare
loo_mocf <- loo(mocf)

# make_stancode(lower | cens(censored, upper) ~ 1, data = fbdat_cens)

# fit a female end model with right and interval censoring
# fedat_cens <- phenf %>%
#   filter(Sex == "FEMALE" & Event_Obs %in% c(3,4)) %>%
#   mutate(sum_forcing_centered = sum_forcing - mean(sum_forcing)) %>%
#   select(-DoY, -Date, -State, -contains("Event"), -mean_temp, -forcing, -sum_forcing) %>%
#   tidyr::pivot_wider(names_from = bound, values_from = sum_forcing_centered, values_fill = 0) %>%
#   select(Index, lower, upper, censored)
#
# n_chains = 4
# init_ll <- lapply(1:n_chains, function(id) list(sigma = 100) )
#
# moc_interval_female_end <- brm(lower | cens(censored, upper) ~ 1, data = fedat_cens, chains = n_chains,
#                                prior = c(
#                                          prior(exponential(1), class = sigma)),
#                                inits = init_ll)
# summary(moc_interval_female_end)


# fit a censoring ignorant model with effects
msypc <- brm(sum_forcing_centered ~ 0 + Intercept + (1|Site) + (1|Provenance) + (1|Year) + (1|Clone),
             data = fbdat, cores = 5,
             prior = c(prior(normal(0,50), class = b), # prior on population level intercept
                       prior(exponential(1), class = sigma), # prior on population level sd
                       prior(normal(0,5), class = sd))) # prior on group level sds
summary(msypc)
ranef(msypc)
psum <- posterior_summary(msypc, include = FALSE) %>% round(digits = 2) %>% data.frame()
psum$coef <- c(rownames(psum))


pairs(fbfit, pars = c("b_Intercept", "sigma", "sd"))

#plot(msypc, pars = c("sigma", "Intercept", "Site"))

loo_msypc <- loo(msypc)

# fit a model with censorship and effects
msypc_c <- brm(sum_forcing_centered | cens(censored) ~ 1 + (1|Site) + (1|Provenance) + (1|Year) + (1|Clone), data = fbdat, cores = 5)


summary(msypc_c)
msypc_c_yrep <- posterior_predict(msypc_c, draws = 500)

loo_msypc_c <- loo(msypc_c)
color_scheme_set("green")
ppc_dens_overlay(fbdat$sum_forcing_centered, msypc_c_yrep[1:50,])

# compare only models with no effects
brms::loo_compare(loo_mo, loo_moc, loo_mocf)
brms::loo_compare(loo_msypc_c, loo_msypc, loo_moc, loo_mo, loo_mocf)
