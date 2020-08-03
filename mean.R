library(rstan)
library(tidybayes)
library(bayesplot)

# Simulate data --------------
# intercept only model of forcing units
N <- 100
dat <- list(N=N, mu=350, sigma = 5)


simu <- rstan::stan(file='simulate.stan', iter=1, chains=1, algorithm="Fixed_param", data=dat)

simudat <- rstan::extract(simu)

input <- list("N"=N, "y" = simudat$y[1,])

hist(input$y)

# Fit model to simulated data --------------------

hist(rexp(1000, 0.2), breaks=30)
hist(rnorm(1000, 350, 30), breaks = 30)
hist(rexp(1000, 0.5), breaks=30)

fitsimu <- rstan::stan(file='fit.stan', chains=4, data=input)

fitpars <- data.frame(rstan::extract(fitsimu))

mcmc_areas(fitpars, pars="mu")
mcmc_areas(fitpars, pars="sigma")
