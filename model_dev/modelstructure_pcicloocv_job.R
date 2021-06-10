# from female end
library(flowers)
library(dplyr)
library(brms)
library(ggplot2)
library(bayesplot)
library(tidyr)

source('phenology_functions.R')

# fresh run?
freshrun <- FALSE

phendat <- flowers::lodgepole_phenology_event

phenf <- prepare_data(phendat)

fedat <- filter_sex_event(sex = "FEMALE", event = "end", phenf)


# MODELS ######
init_ll <- lapply(1:4, function(id) list(sigma = 30 )) # interval censored models require big sigma inits to start sampling

if (freshrun == TRUE) {
  # mean only
  mo <- brm(sum_forcing_centered ~ 0 + Intercept, data = fedat,
            prior = c(prior("normal(0,50)", class = "b"),
                      prior("normal(0,20)", class = "sigma")),
            cores = 5, inits = init_ll)

  saveRDS(mo, "model_dev/mo.rds")

  # mean only with end censoring
  moc <- brm(sum_forcing_centered | cens(censored_lronly) ~ 0 + Intercept, data = fedat,
             prior = c(prior("normal(0,50)", class = "b"),
                       prior("normal(0,20)", class = "sigma")),
             cores = 5, inits = init_ll)
  saveRDS(moc, "model_dev/moc.rds")

  # mean only model with end and interval censoring


  mocf <- brm(sum_forcing_centered | cens(censored, upper) ~ 0 + Intercept, data = fedat,
              prior = c(prior("normal(0,50)", class = "b"),
                        prior("normal(0,20)", class = "sigma")),
              cores=5,  inits = init_ll)
  saveRDS(mocf, "model_dev/mocf.rds")

  # mean + effects + end + interval censoring

  ec <- brm(sum_forcing_centered | cens(censored, upper) ~ 0 + Intercept + (1|Site) + (1|Provenance) + (1|Clone) + (1|Year), data = fedat,
            prior = c(prior("normal(0,50)", class = "b"),
                      prior("normal(0,20)", class = "sigma"),
                      prior("student_t(3,0,10)", class = "sd")),
            cores = 5, inits = init_ll,
            save_pars = save_pars(all = TRUE))
  saveRDS(ec, "model_dev/ec.rds") } else {
    mo <- readRDS("model_dev/mo.rds")
    moc <- readRDS("model_dev/moc.rds")
    modf <- readRDS("model_dev/mocf.rds")
    ec <- readRDS("model_dev/ec.rds")
  }


# ec_yrep <- posterior_predict(ec, draws = 500)
#
# color_scheme_set("green")
# status y
#     Empirical CCDF estimates of each dataset (row) in yrep are overlaid, with the Kaplan-Meier estimate (Kaplan and Meier, 1958) for y itself on top (and in a darker shade). This is a PPC suitable for right-censored y. Note that the replicated data from yrep is assumed to be uncensored.
# The status indicator for the observations from y. This must be a numeric vector of the same length as y with values in {0, 1} (0 = right censored, 1 = event).
# status <- mutate(fedat_cens, status_y = case_when(censored == "interval" ~ 1,
#                                                   censored == "right" ~ 0) )
# library(ggfortify)
# ppc_km_overlay(fedat_cens$lower, ec_yrep[1:25, ], status_y = status$status_y)
# ppc_ecdf_overlay_grouped(fedat_cens$lower, ec_yrep[1:50,], group = fedat_cens$censored)
# I think I need to calculate posterior predictions differently depending on censorship
if (freshrun == TRUE) {
  loo_mo <- loo(mo)
  loo_moc <- loo(moc)
  loo_mocf <- loo(mocf)
  loo_ec <- loo(ec, reloo = TRUE, reloo_args = list(prior = c(prior("normal(0,50)", class = "b"),
                                                              prior("normal(0,20)", class = "sigma"),
                                                              prior("student_t(3,0,10)", class = "sd")),
                                                    inits = init_ll, iter = 3000))
  #loo_ec <- loo(ec)

  saveRDS(loo_mo, "model_dev/loo_mo.rds")
  saveRDS(loo_moc, "model_dev/loo_moc.rds")
  saveRDS(loo_mocf, "model_dev/loo_mocf.rds")
  saveRDS(loo_ec, "model_dev/loo_ec.rds")
} else {
  loo_mo <- readRDS("model_dev/loo_mo.rds")
  loo_moc <- readRDS("model_dev/loo_moc.rds")
  loo_mocf <- readRDS("model_dev/loo_mocf.rds")
  loo_ec <- readRDS("model_dev/loo_ec.rds")

  loo_compare(loo_mo, loo_moc, loo_mocf, loo_ec)
}
