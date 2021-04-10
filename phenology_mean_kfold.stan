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

}


parameters {
  //pars
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale

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
    log_lik[i] = normal_lpdf(sum_forcing[i] | mu, sigma);
  }
}
