library(lme4)
library(brms)

# figure out how to simulate from multiple new levels
#
model <- brm(Reaction ~ Days + (1 | Subject), data = sleepstudy, family = gaussian(), iter = 4000, warmup = 1000, chains = 4, cores = 4)

newdata <- expand_grid(Days = 0:9, Subject = c("373", "374", "375"))

epred <- add_epred_draws(object = model, newdata = newdata, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "gaussian")

ppred <- add_predicted_draws(object = model, newdata = newdata, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "gaussian")

ppred_uncertainty <- add_predicted_draws(object = model, newdata = newdata, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "uncertainty")

epred %>% group_by(Subject) %>% summarise(mpred = mean(.epred), sdpred = sd(.epred))

ppred %>% group_by(Subject) %>% summarise(mpred = mean(.prediction), sdpred = sd(.prediction))

ppred_uncertainty %>% group_by(Subject) %>% summarise(mpred = mean(.prediction), sdpred = sd(.prediction))

# new data

prior1 <- c(prior("normal(5,1)", class = "Intercept"),
            prior("normal(0,1)", class = "sigma"),
            prior("normal(0,10)", class = "sd"))

reff <- rnorm(5, 0, 10)
sigma <- rnorm(100, 0, 1)
dat <- data.frame(outcome = 5 + rep(reff, 20) + sigma, individual = 1:100, group = rep(1:5, 20))
model <- brm(outcome ~ 1 + (1|group), data = dat, prior = prior1, chains = 4, cores = 4)

ranef(model)
summary(model)

hist(rnorm(1000,0,10))

preds <- add_predicted_draws(object = model, newdata = dat, re_formula = NULL)

preds %>%
  group_by(group) %>%
  summarise(mean(.prediction), sd(.prediction))

newdata <- data.frame(individual = 101:105, group = 6:10)

newpreds <- add_predicted_draws(model, newdata = newdata, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "gaussian")

newpreds %>%
  group_by(group) %>%
  summarise(mean(.prediction), sd(.prediction))

newpreds_uncertain <- add_predicted_draws(model, newdata = newdata, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "uncertainty")

newpreds_uncertain %>%
  group_by(group) %>%
  summarise(mean(.prediction), sd(.prediction))

newpreds_oldlevels <- add_predicted_draws(model, newdata = newdata, re_formula = NULL, allow_new_levels = TRUE, sample_new_levels = "old_levels")

newpreds_oldlevels %>%
  group_by(group) %>%
  summarise(mean(.prediction), sd(.prediction))





