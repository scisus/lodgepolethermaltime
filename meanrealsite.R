# Fit a thermal time model to my phenology data
# Only fit the mean and sd, not considering groups

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(dplyr)
library(flowers)

phen <- flowers::phenology %>% # phenology data
    filter(Phenophase_Derived==2 & Year < 2012) %>%
    rename(state = Phenophase_Derived) # only consider days when trees are flowering

forcing <- read.csv("../phenolology/data/all_clim_PCIC.csv", header=TRUE, stringsAsFactors = FALSE) %>%
    filter(forcing_type=="ristos") # only consider forcing units calculated based on work of Sarvas 1972

spus <- read.csv("../phd/data/OrchardInfo/LodgepoleSPUs.csv") %>%
    select(SPU_Name, Orchard) # provenance information for each orchard in phen

# start and end phenology
phense <- dplyr::filter(phen, DoY == First_RF | DoY == Last_RF) %>%
    dplyr::left_join(forcing) %>%
    dplyr::left_join(spus)

# write data file that stan can actually use
prepforstan <- function(df, file) {
    N <- nrow(df)
    Nsite <- length(unique(df$SiteID))
    # Nprovenance <- length(unique(df$ProvenanceID))
    # Norchard <- length(unique(df$OrchardID))
    # Nclone <- length(unique(df$CloneID))
    # Ntree <- length(unique(df$TreeID))
    # Nyear <- length(unique(df$YearID))

    SiteID <- df$SiteID
    # ProvenanceID <- df$ProvenanceID
    # OrchardID <- df$OrchardID
    # CloneID <- df$CloneID
    # TreeID <- df$TreeID
    # YearID <- df$YearID

    forcing <- df$sum_forcing
    state <- df$Phenophase_Derived

    rstan::stan_rdump(c("N", "K", "Nsite","Nprovenance", "Nclone", "Nyear", "Ntree", "SiteID", "ProvenanceID", "CloneID", "YearID", "TreeID", "forcing", "state"), file)
}

# receptivity start
phenfs <- phense %>% dplyr::filter(Sex == "FEMALE" & DoY == First_RF) %>%
    dplyr::select(Site, sum_forcing)

# prepare data for stan

#input <- list("N" = nrow(phenfs), "forcing" = phenfs$sum_forcing)

input <- tidybayes::compose_data(phenfs)


fit <- rstan::stan(file='meanfitrealsite.stan', chains=4, data=input)

# plot modeled and true data

get_variables(fit)

ypred <- fit %>%
    recover_types(phenfs) %>%
    gather_draws(y_ppc[Site])
ypred <- tidybayes::gather_draws(model = fit, `y_ppc*`[i], regex=TRUE)
ypred <- tidybayes::gather_draws(model = fit, y_ppc[Site])

ggplot(ypred, aes(x=.value, colour=Site)) +
    geom_density() +
    geom_density(data=phenfs, aes(x=sum_forcing, fill=Site), inherit.aes = FALSE, alpha=0.5) +
    ggtitle("observed data and model output with site") +
    facet_wrap("Site")

