//
// This Stan program fits a likelihood, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'k'.


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
  //pars
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale
  
  // factor offsets 
  
  vector[k_Site] delta_site;

}


// Simulate the log likelihood from the current value of the parameters
generated quantities {
  // DECLARE
  
  //yrep & ll
 // real sum_forcing_rep[k];
  vector[k] log_lik;
  
  //ll
  {
    for (i in 1:k) 
    log_lik[i] = normal_lpdf(sum_forcing[i] | mu  + delta_site[Site[i]], sigma);
  }
}
