fb <- readRDS("female_begin.rds")
fe <- readRDS("female_end.rds")
mb <- readRDS("male_begin.rds")
me <- readRDS("male_end.rds")

summary(fb)
summary(fe)
summary(mb)
summary(me)

plot(conditional_effects(fb))
plot(conditional_effects(fe))
plot(conditional_effects(mb))
plot(conditional_effects(me))
