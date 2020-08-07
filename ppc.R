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

#################

## OFFSETS
head(phenfs)

sitedraws <- gather_draws(fit, site_offset[Site]) %>%
  rename(.label = Site)
provdraws <- gather_draws(fit, prov_offset[Provenance]) %>%
  rename(.label=Provenance)
yeardraws <- gather_draws(fit, year_offset[Year]) %>%
  rename(.label = Year)
clonedraws <- gather_draws(fit, clone_offset[Clone]) %>%
  rename(.label = Clone) 

clonesample <- sample(unique(clonedraws$.label), size=20)
clonedraws_sample <- filter(clonedraws, .label==clonesample)


offsets <- bind_rows(sitedraws, provdraws, yeardraws, clonedraws_sample)

offsets %>% 
  ggplot(aes(y=.label, x=.value)) +
  stat_pointinterval() +
  facet_wrap(".variable", scales = "free_y") +
  theme_bw(base_size=18)

## MEAN & VAR POP

vars <- tidybayes::get_variables(fit) 
drop <- stringr::str_detect(vars, "y_ppc")
vars_subset <- vars[!drop]

mus <- gather_draws(fit, `mu.*`, regex=TRUE)
sigmas <- gather_draws(fit, `sigma.*`, regex=TRUE) 

musigma <- bind_rows(mus, sigmas) %>%
  tidyr::separate(.variable, into = c("moment", "variable"))  %>%
  dplyr::mutate(variable = case_when(is.na(variable) ~ moment,
                                     !is.na(variable) ~ variable)) %>%
  mutate(variable = forcats::fct_relevel(variable, 
                          "mu", "sigma", "site", 
                          "prov", "year", "clone"))

ggplot(musigma, aes(y=forcats::fct_rev(variable), x=.value)) +
  stat_halfeye(.width = c(.90, .5)) +
  facet_wrap("moment", scales = "free_x") +
  theme_bw(base_size=18) +
  ylab("") +
  xlab("")


