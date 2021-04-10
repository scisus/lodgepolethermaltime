data {
  int<lower=1> k; // number of observations
  vector[k] sum_forcing; // observations
  
  real<lower=0> mu_mean; // mean for prior on overall mean
  real<lower=0> mu_sigma; // sigma for prior on overall mean (measurement variability)
  
  // number of individual levels in each factor
  int<lower=1> k_Site; // number of sites
  
  // individual level for each factor from which each observation is generated 
  int<lower=1, upper=k_Site> Site[k]; // which site is associated with each observation 
  
  // indexing for non-centered and centered levels within factors
  // sites
  
  int<lower=0, upper=k_Site> k_ncp_Site;          // Number of noncentered sites
  int<lower=1, upper=k_Site> ncp_idx_Site[k_ncp_Site]; // Index of noncentered sites
  
  int<lower=0, upper=k_Site> k_cp_Site;           // Number of centered sites
  int<lower=1, upper=k_Site> cp_idx_Site[k_cp_Site];   // Index of noncentered sites
}


parameters {
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale
  
  vector[k_ncp_Site] delta_ncp_site; //non-centered site parameters
  vector[k_cp_Site] delta_cp_site; //centered site parameters
  
  real sigma_site; // site offset variance
  
  real mu_site; // site offset mean
  
}

transformed parameters {
  
  // recenter individual factor levels
  vector[k_Site] delta_site;
  
  //site
  delta_site[ncp_idx_Site] = mu_site + sigma_site * delta_ncp_site;
  delta_site[cp_idx_Site] = delta_cp_site;
  
  
}


model {
  // prior model
  
  sigma ~ exponential(1);
  // these are half normals
  sigma_site ~ normal(0, 5);
  
  mu_site ~ normal(0, 5);
  
  mu ~ normal(mu_mean, mu_sigma);
  
  sum_forcing ~ normal(mu + delta_site[Site],
  sigma);
}


// Simulate a full observation from the current value of the parameters
generated quantities {
 real sum_forcing_rep[k];

  
  //simulate
  { 
  for (i in 1:k)
  sum_forcing_rep[i] = normal_rng(mu + delta_site[Site[i]], sigma);
  }

}


