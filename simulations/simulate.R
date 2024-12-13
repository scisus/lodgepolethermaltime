#data for simulations

k=1000
k_Site = 7
sigma_site = 3
sigma_year = 10
sigma_prov = 5
sigma_genotype = 8
mu_site = 0
mu_year = 0
mu_prov = 0
mu_genotype = 0


dat <- list(
  k = k,
  censored = sample(c(0,1), size = k, replace = TRUE),
  censoring = abs(rnorm(k, mean = 0, sd = 5)), # k - length(censored) of these values won't be used, but simulating k censoring values to keep indexing simple in the stan model
  # mu_mean = 300,
  # mu_sigma = 5,
  k_Site = k_Site,
  k_Year = 15,
  k_Provenance = 7,
  k_Genotype = 300,
  Site = sample(1:k_Site, size = k, replace = TRUE),
  Year = sample(1:k_Year, size = k, replace = TRUE),
  Provenance = sample(1:k_Provenance, size = k, replace = TRUE),
  Genotype = sample(1:k_Genotype, size = k, replace = TRUE),
  
  mu = 330,
  sigma = 10,
  sigma_cens = 15,
  
  # sigma_site = sigma_site,
  # sigma_year = sigma_year,
  # sigma_prov = sigma_prov,
  # sigma_genotype = sigma_genotype,
  # 
  # mu_site = mu_site,
  # mu_year = mu_year,
  # mu_prov = mu_prov,
  # mu_genotype = mu_genotype,
  
  delta_site = rnorm(k_Site, mu_site, sigma_site),
  delta_year = rnorm(k_Year, mu_year, sigma_year),
  delta_prov = rnorm(k_Provenance, mu_prov, sigma_prov),
  delta_genotype = rnorm(k_Genotype, mu_genotype, sigma_genotype)
  
)