// Simulate the Bayesian Ensemble Ancestral sampling from the joint probability distribution.
data {
  int<lower=1> n;
  int<lower=0> n_Site;
  int<lower=0> n_Year;
  int<lower=0> n_Provenance;
  int<lower=0> n_Genotype;
  
}

generated quantities {
  // Simulate model configuration from prior model
  
  real y[n];
  real sum_forcing[n];
  
  real<lower=0> mu; //accumulated forcing cannot be negative
  real<lower=0> sigma; //measurement error
  
  vector[n_Site] site_offset; // site effect
  vector[n_Year] year_offset; //year effect
  vector[n_Provenance] prov_offset; //provenance effect
  vector[n_Genotype] genotype_offset; //genotype effect
  
  real sigma_site; // site effect variance
  real sigma_year; // year effect variance
  real sigma_prov; // provenance effect variance
  real sigma_genotype; //genotype effect variance
  
  real mu_site; // site effect mean
  real mu_year; // year effect mean
  real mu_prov; // provenance effect mean
  real mu_genotype; //genotype effect mean
  
  sigma = exponential_rng(1);
  sigma_site = exponential_rng(0.5);
  sigma_year = exponential_rng(0.5);
  sigma_prov = exponential_rng(0.5);
  sigma_genotype = exponential_rng(0.5);
  
  mu_site = normal_rng(0,5);
  mu_year = normal_rng(0,5);
  mu_prov = normal_rng(0,5);
  mu_genotype = normal_rng(0,5);
  
  site_offset[n_Site] = normal_rng(mu_site, sigma_site);
  year_offset[n_Year] = normal_rng(mu_year, sigma_year);
  prov_offset[n_Provenance] = normal_rng(mu_prov, sigma_prov);
  genotype_offset[n_Genotype] = normal_rng(mu_genotype, sigma_genotype);
  
  mu = normal_rng(500,150);
  
  // Simulate data from observational model

  for (i in 1:n) {
    sum_forcing[n] = normal_rng(mu + site_offset[Site] + year_offset[Year] + prov_offset[Provenance] + 
    (mu_genotype + z_genotype_offset[Genotype] * sigma_genotype), 
    sigma);
    y[n] = normal_rng(lambda);
}

