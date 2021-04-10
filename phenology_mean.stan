//
// This Stan program defines a multilevel factor model of phenology, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma' and heterogeneity from factors 
// Site, Provenance, Year, and Clone modeled as offsets `delta`.
//
// Some Site and Provenance levels are noncentered and Clone is fully non-centered.
//

// The input data is a vector 'sum_forcing' of length 'k'.


data {
  int<lower=1> k; // number of observations
  vector[k] sum_forcing; // observations
  
  real<lower=0> mu_mean; // mean for prior on overall mean
  real<lower=0> mu_sigma; // sigma for prior on overall mean (measurement variability)
}


parameters {
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale
}

model {
  // prior model
  
  sigma ~ exponential(1);
  
  mu ~ normal(mu_mean, mu_sigma);
  
  sum_forcing ~ normal(mu, sigma);
}


// Simulate a full observation from the current value of the parameters
generated quantities {
 real sum_forcing_rep[k];
  
  //simulate
  { 
  for (i in 1:k)
  sum_forcing_rep[i] = normal_rng(mu, sigma);
  }

}


