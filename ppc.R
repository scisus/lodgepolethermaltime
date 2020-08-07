# posterior predictive checks
# 
# 
library(tidybayes)
library(dplyr)

fit <- readRDS("2020-08-06sypcFEMALE.rds") %>%
  tidybayes::recover_types(phenfs)

  
ypred <- fit %>%
  spread_draws(y_ppc[i], n=100, seed=87)

ggplot(ypred, aes(x=y_ppc, color="model") ) +
  geom_density() +
  geom_density(data=phenfs, aes(x=sum_forcing, color="observations"), inherit.aes = FALSE) +
  theme_classic(base_size = 18) +
  xlab("accumulated forcing")


