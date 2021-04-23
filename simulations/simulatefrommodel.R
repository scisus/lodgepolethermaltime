
library(tidyr)
# get median means and sigmas

medians <- musigma %>%
  group_by(moment, variable) %>%
  summarise(median = median(.value)) %>%
  tidyr::pivot_wider(names_from = moment, values_from = median) %>%
  split(.$variable)

musd <- musigma %>%
  filter(variable == "mu") %>%
  summarise(musd = sd(.value))

reps <- 1000
mu <- rnorm(n = reps, mean = medians$mu$mu, sd = musd)
site <- rnorm(reps, mean = medians$site$mu, sd = medians$site$sigma)
prov <- rnorm(reps, medians$prov$mu, medians$prov$sigma)
year <- rnorm(reps, medians$year$mu, medians$year$sigma)
clone <- rnorm(reps, medians$clone$mu, medians$clone$sigma)

y = rnorm(1e5, mean = mu+site+prov+year+clone, sd = medians$sigma$sigma)

yframe <- data.frame(forcing = y)

ggplot(yframe, aes(x=forcing, color="model")) +
  geom_density() +
  geom_density(data=phenfs, aes(x=sum_forcing, color="observations")) +
  ggtitle("Out of sample predictions and observations", subtitle="median parameter estimates only") +
  theme_bw(base_size=18)

#############

# Use more than medians (100 samples?)

mus <- filter(musigma, moment == "mu") %>%
  pivot_wider(names_from = "variable", values_from = ".value") 
sigmas <- filter(musigma, moment == "sigma") %>%
  pivot_wider(names_from = "variable", values_from = ".value") 

effects <- select(mus, .chain, .iteration, .draw, mu)
effects$sigma <- sigmas$sigma

effects$site <- rnorm(nrow(mus), mus$site, sigmas$site)
effects$prov <- rnorm(nrow(mus), mus$prov, sigmas$prov)
effects$year <- rnorm(nrow(mus), mus$year, sigmas$year)
effects$clone <- rnorm(nrow(mus), mus$clone, sigmas$clone)

predicts <- effects %>%
  mutate(fullmean = mu + site + prov + year + clone)

forcingests1 <- rnorm(nrow(predicts), predicts$fullmean, predicts$sigma)
forcingests30 <- rnorm(nrow(predicts)*30, predicts$fullmean, predicts$sigma)

pf <- data.frame(forcing = forcingests1, n=1)
pf30 <- data.frame(forcing = forcingests30, n=30)

ggplot(data=phenfs, aes(x=sum_forcing)) +
  geom_density(linetype=2) +
  geom_density(data=pf30, aes(x=forcing, fill="model"), alpha=0.4) +
  geom_density(data=yframe, aes(x= forcing, fill= "model - medians only"), alpha=0.4) +
  theme_bw(base_size = 18)
