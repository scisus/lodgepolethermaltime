data {
  int<lower=1> k; // number of observations
  vector[k] sum_forcing; // observations
  
  real<lower=0> mu_mean; // mean for prior on overall mean
  real<lower=0> mu_sigma; // sigma for prior on overall mean (measurement variability)
  
  // number of individual levels in each factor
  int<lower=1> k_Site; // number of sites
  
  // individual level for each factor from which each observation is generated 
  int<lower=1, upper=k_Site> Site[k]; // which site is associated with each observation 
  
}


parameters {
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale
  
  real sigma_site; // site offset variance
  
  real mu_site; // site offset mean
  
  vector[k_Site] delta_site;
  
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
  vector[k] log_lik;
  
  //simulate y_rep
  { 
    for (i in 1:k)
    sum_forcing_rep[i] = normal_rng(mu + delta_site[Site[i]], sigma);
  }
  
  //ll
  {
    for (i in 1:k) 
    log_lik[i] = normal_lpdf(sum_forcing[i] | mu  + delta_site[Site[i]], sigma);
  }
  
  
}


