# posterior predictive checks
# 
# 
library(tidybayes)

fit <- readRDS("2020-08-06sypcFEMALE.rds") %>%
  tidybayes::recover_types()


