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
  // int<lower=1> k_Site; // number of sites
  // int<lower=1> k_Year; 
  // int<lower=1> k_Provenance;
  // int<lower=1> k_Clone;
  // 
  // // individual level for each factor from which each observation is generated 
  // int<lower=1, upper=k_Site> Site[k]; // which site is associated with each observation 
  // int<lower=1, upper=k_Year> Year[k];
  // int<lower=1, upper=k_Provenance> Provenance[k];
  // int<lower=1, upper=k_Clone> Clone[k];
  
  // indexing for non-centered and centered levels within factors
      // sites
  
  // int<lower=0, upper=k_Site> k_ncp_Site;          // Number of noncentered sites
  // int<lower=1, upper=k_Site> ncp_idx_Site[k_ncp_Site]; // Index of noncentered sites
  // 
  // int<lower=0, upper=k_Site> k_cp_Site;           // Number of centered sites
  // int<lower=1, upper=k_Site> cp_idx_Site[k_cp_Site];   // Index of noncentered sites
  // 
  //    // provenances
  // 
  // int<lower=0, upper=k_Provenance> k_ncp_Provenance;          // Number of noncentered sites
  // int<lower=1, upper=k_Provenance> ncp_idx_Provenance[k_ncp_Provenance]; // Index of noncentered Provenances
  // 
  // int<lower=0, upper=k_Provenance> k_cp_Provenance;           // Number of centered Provenances
  // int<lower=1, upper=k_Provenance> cp_idx_Provenance[k_cp_Provenance];   // Index of noncentered Provenances
  

}


parameters {
  //pars
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale
  
  // factor offsets 
  
  // vector[k_Site] delta_site;
  // vector[k_Provenance] delta_prov;
  // vector[k_Year] delta_year;
  // vector[k_Clone] delta_clone;
  
  real mu_site; // site offset mean
  real mu_year; // year offset mean
  real mu_prov; // provenance offset mean
  real mu_clone; //clone offset mean

}


// Simulate the log likelihood from the current value of the parameters
generated quantities {
  // DECLARE
  
  //yrep & ll
 // real sum_forcing_rep[k];
  //vector[k] log_lik_specific;
  vector[k] log_lik_general;
  
  //ll
  {
    for (i in 1:k) {
    //log_lik_specific[i] = normal_lpdf(sum_forcing[i] | mu  + delta_site[Site[i]] + mu_year + delta_prov[Provenance[i]] + mu_clone, sigma);
    log_lik_general[i] = normal_lpdf(sum_forcing[i] | mu  + mu_site + mu_year + mu_prov + mu_clone, sigma);
    }
    
  }
}
