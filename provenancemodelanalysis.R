# provenance model analysis


library(dplyr)
library(brms)
library(purrr)
library(tidybayes)
#library(shinystan)
#library(ggplot2)



clonemodells <- list(fb = readRDS("female_begin_clone.rds"),
                     mb = readRDS("male_begin_clone.rds"),
                     fe = readRDS("female_end_clone.rds"),
                     me = readRDS("male_end_clone.rds"))
saveRDS(clonemodells, "objects/clonemodells.rds")
clonedat <- readRDS("objects/clonedat.rds") %>%
  split(f = list(.$Sex, .$event))

labdf <- data.frame(Sex = c("FEMALE", "FEMALE", "MALE", "MALE"), event = c('begin', 'end', 'begin', 'end'), model = c('fb', 'fe', 'mb', 'me'))


vars <- purrr::map(clonemodells, function(x) {gather_draws(x, b_Intercept, b_MAT, sigma)}) %>%
  bind_rows(.id = "model") %>%
  left_join(labdf) # label the models for plotting

# add draws from the expectation of the posterior predictive distribution (expected value/mean of the posterior). only incorporates uncertainty in the mean while ignoring residual error

clonepred <- purrr::map2(clonedat, clonemodells, function(x,y) {add_predicted_draws(x, y)}) %>%
  bind_rows() %>%
  select(-.chain, -.iteration) %>%
  filter(!is.na(MAT)) %>%
  ungroup()
saveRDS(clonepred, "objects/clonepred.rds")


# make a table summarising the model
#
summary(clonemodells$fb)

# r2 values
bayesr2 <- purrr::map_dfr(clonemodells, function(x) brms::bayes_R2(x) %>% as.data.frame(), .id = "model")
rownames(bayesr2) <- NULL
bayesr2$Parameter = "R2"

build_summary_table <- function(model) {
  df <- rbind(summary(model)$fixed, summary(model)$spec_pars)
  df$Parameter <- rownames(df)
  rownames(df) <- NULL
  return(df)
}


model_results <- purrr::map_dfr(clonemodells, build_summary_table, .id = "model") %>%
  rename(Q2.5 = "l-95% CI", Q97.5 = "u-95% CI") %>%
  select(-Rhat, -contains("ESS")) %>%
  rbind(bayesr2) %>%
  full_join(labdf)

