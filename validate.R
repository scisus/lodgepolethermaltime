# validate and compare models with loo and waic

library(rstan)
library(loo)
library(dplyr)

source('phenology_functions.R')

# phenology data
# 
phenbe <- filter_start_end()
dat <- select_data(phenbe, "FEMALE", "begin", keep_day = TRUE) 
dat$i <- 1:nrow(dat)

fbfitcombined <- readRDS('2021-03-03FEMALE_begin.rds')

llsiteyear <- loo::extract_log_lik(fbfitcombined) # pull log likelihoods for waic & loo calculations

loo::waic(llsiteyear)

loosy <- loo::loo(llsiteyear2, r_eff = relative_eff(exp(llsiteyear2)), save_psis = TRUE, cores = 10)
plot(loosy)

bad <- which(loosy$diagnostics$pareto_k >= 0.7)
ok <- which(loosy$diagnostics$pareto_k < 0.7 & loosy$diagnostics$pareto_k > 0.5)

dat$paretok <-loosy$diagnostics$pareto_k


ggplot(dat, aes(x=i, y=paretok, colour = Site)) +
  geom_point(shape = 3) +
  geom_hline(yintercept = c(0.5, 0.7), lty=2, colour = "darkgray")

ggplot(dat, aes(x=i, y=paretok, colour = Provenance)) +
  geom_point(shape = 3) +
  geom_hline(yintercept = c(0.5, 0.7), lty=2, colour = "darkgray")


