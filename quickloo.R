library(brms)

bformprov <- brmsformula(sum_forcing | cens(censored, upper) ~ 1 + (1|Site) + bFFP + (1|Clone) + (1|Year) + (1|Tree))


bpriorprov <- c(prior("normal(400,100)", class = "Intercept"),
                prior("normal(0,2)", class = "b"),
                prior("normal(0,15)", class = "sigma"),
                prior("normal(0,9)", class = "sd"))

fbfitprov <- readRDS("female_beginprov.rds")

initpars <- lapply(1:6, function(id) list(sigma = 30, Intercept = 300))

loofbbFFP <- loo(fbfitprov, reloo = TRUE, reloo_args = list(prior=bpriorprov),
                 cores = 20, inits = initpars, iter = 3000)

saveRDS(loofbbFFP, "loofbbFFP.rds")
