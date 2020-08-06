library(rstan)
library(tidybayes)
library(ggplot)
library(dplyr)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Simulate data --------------
# intercept only model of forcing units with 1 cluster (sites), but sites have very small effects

N <- 1000
dat <- list(N=N, alpha_pop=350, sigma = 5,
            siteN = 7, siteID = 1:7, alpha_site = rnorm(7, 0, 1))


simu <- rstan::stan(file='interceptgroup1_simulate.stan', iter=1, chains=1, algorithm="Fixed_param", data=dat)

simudat <- data.frame(rstan::extract(simu)$y[1,,]) %>%
    tidyr::pivot_longer(cols = contains("X"), names_to = "siteID", values_to = "y") %>%
    tidyr::extract(siteID, into = "siteID", regex = "([:digit:]{1,2})") %>%
    dplyr::mutate(siteID = as.integer(siteID))

input <- list("N"= N * 7, "siteN" = 7, "y" = simudat$y, "siteID" = simudat$siteID)

ggplot(simudat, aes(x=y, fill=as.factor(siteID))) +
    geom_density(alpha=0.5) +
    ggtitle("site effects")

# Fit model to simulated data --------------------

fitsimu <- rstan::stan(file='interceptgroup1_fit.stan', chains=4, data=input)

fitpars <- data.frame(rstan::extract(fitsimu))

a_site <- tidybayes::spread_draws(fitsimu, alpha_site[siteID])
a_pop <- tidybayes::spread_draws(fitsimu, alpha_pop)

truepars <- data.frame(siteID = dat$siteID, alpha_site_true = dat$alpha_site)
alphas <- left_join(a_site, a_pop) %>%
    left_join(truepars) %>%
    mutate(offset = alpha_pop - alpha_site)

ggplot(alphas, aes(x=offset)) +
    geom_density() +
    facet_wrap("siteID") +
    geom_vline(aes(xintercept = alpha_site_true), colour="purple") +
    ggtitle("modeled and true site effects, for site effects N(0,1)")

ggplot(alphas, aes(x=alpha_site, fill=as.factor(siteID))) +
    geom_density(alpha=0.5) +
    ggtitle("modeled alpha site")

