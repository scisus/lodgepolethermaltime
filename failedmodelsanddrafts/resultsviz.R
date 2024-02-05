install.packages("bayestestR")
install.packages("insight")
install.packages("report")
install.packages("see")

library(bayestestR)
library(insight)
library(bayestestR)
library(report)
library(see)
library(dplyr)




model <- readRDS("ecfit.rds")
canned <- report(model) # takes a long time and isn't valid for censored models
report_model(model)
result <- estimate_density(model)

plot(result)
plot(result, stack = FALSE)
plot(result, stack = FALSE, priors = TRUE) # might need to adjust prior sample reporting in brm call

pdirplot <- p_direction(model, effects = "all", component = "conditional") %>% # probability of direction
  filter(!grepl("r_Clone", Parameter)) # drop clone intercepts
plot(pdirplot) # breaks sd names!
#plot(pdirplot, priors = TRUE) # add priors

pracplot <- p_significance(model, effects = "all", component = "all", threshold = c(-5,5)) %>% # practical significance (set a threshold)
  filter(!grepl("r_Clone", Parameter))
pracplot
plot(pracplot, n_columns = NULL)

# hdiplot <- hdi(model, ci = c(0.5, 0.75, 0.89, 0.95))
# hdiplot
#
# plot(hdiplot) + scale_fill_metro()

#plot priors and posteriors together "support interval"
pp <- si(model) # add support_only = TRUE to zoom in on support interval
pp
plot(pp) +
  scale_color_metro(palette = "ice") +
  scale_fill_metro(palette = "ice")

# region of practical equivalence
ropres <- rope(model, ci = c(0.9, 0.95), range(-5,5)) %>%
  filter(!grepl("r_Clone", Parameter))
#ropres <- rope(model, ci = c(0.9, 0.95), effects = "all", component = "all")
ropres

plot(ropres, rope_color = "grey70") +
  scale_fill_social()
plot(result, rope_color = "red") +
  scale_fill_brewer(palette = "Greens", direction = -1)
