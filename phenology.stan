//
// This Stan program defines  model, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'k'.

functions {
  // from example by Christiaan van Dorp [https://tbz533.blogspot.com/2019/07/neat-encoding-of-censored-data-in-stan.html]
    real censored_normal_lpdf(real x, real mu, real sigma, int cc) {
        real ll;
        if ( cc == 0 ) { // uncensored
            ll = normal_lpdf(x | mu, sigma);
        } else if ( cc == 1 ) { // left-censored
            ll = normal_lcdf(x | mu, sigma);
        } else if ( cc == 2 ) { // right-censored
            ll = normal_lccdf(x | mu, sigma);
        } else if ( cc == 3 ) { // missing data
            ll = 0.0;
        } else { // any other censoring code is invalid
            reject("invalid censoring code in censored_normal_lpdf");
        }
        return ll;
    }
}

data {
  int<lower=1> k; // number of observations
  vector[k] sum_forcing; // observations
  int<lower=0, upper=3> censorship[k]; // censorship code (0 uncensored, 1 left, 2 right, 3 no flowering obs)
  
  real<lower=0> mu_mean; // mean for prior on overall mean
  real<lower=0> mu_sigma; // sigma for prior on overall mean (measurement variability)
  
  // number of individual levels in each factor
  int<lower=1> k_Site; // number of sites
  int<lower=1> k_Year;
  int<lower=1> k_Provenance;
  int<lower=1> k_Clone;
  
  // individual level for each factor from which each observation is generated 
  int<lower=1, upper=k_Site> Site[k]; // which site is associated with each observation 
  int<lower=1, upper=k_Year> Year[k];
  int<lower=1, upper=k_Provenance> Provenance[k];
  int<lower=1, upper=k_Clone> Clone[k];
  
  // indexing for non-centered and centered levels within factors
  // sites
  
  int<lower=0, upper=k_Site> k_ncp_Site;          // Number of noncentered sites
  int<lower=1, upper=k_Site> ncp_idx_Site[k_ncp_Site]; // Index of noncentered sites
  
  int<lower=0, upper=k_Site> k_cp_Site;           // Number of centered sites
  int<lower=1, upper=k_Site> cp_idx_Site[k_cp_Site];   // Index of noncentered sites
  
  // provenances
  
  int<lower=0, upper=k_Provenance> k_ncp_Provenance;          // Number of noncentered provenances
  int<lower=1, upper=k_Provenance> ncp_idx_Provenance[k_ncp_Provenance]; // Index of noncentered Provenances
  
  int<lower=0, upper=k_Provenance> k_cp_Provenance;           // Number of centered Provenances
  int<lower=1, upper=k_Provenance> cp_idx_Provenance[k_cp_Provenance];   // Index of noncentered Provenances
}


parameters {
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale
  
  vector[k_ncp_Site] alpha_ncp_site; //non-centered site parameters
  vector[k_cp_Site] alpha_cp_site; //centered site parameters
  
  vector[k_Year] alpha_year; //year effect

  vector[k_ncp_Provenance] alpha_ncp_prov; //non-centered Provenance parameters
  vector[k_cp_Provenance] alpha_cp_prov; //centered Provenance parameters
  
  vector[k_Clone] z_alpha_clone; //clone effect
  
  real sigma_site; // site effect variance
  real sigma_year; // year effect variance
  real sigma_prov; // provenance effect variance
  real sigma_clone; //clone effect variance
  
  real mu_site; // site effect mean
  real mu_year; // year effect mean
  real mu_prov; // provenance effect mean
  real mu_clone; // clone effect mean
}

transformed parameters {
  // recenter individual parameters
  vector[k_Site] alpha_site;
  vector[k_Provenance] alpha_prov;
  
  //site
  alpha_site[ncp_idx_Site] = mu_site + sigma_site * alpha_ncp_site;
  alpha_site[cp_idx_Site] = alpha_cp_site;
  
  //provenance
  alpha_prov[ncp_idx_Provenance] = mu_prov + sigma_prov * alpha_ncp_prov;
  alpha_prov[cp_idx_Provenance] = alpha_cp_prov;

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // prior model
  vector[k] forcing_mu;
  sigma ~ exponential(1);
  // these are half normals
  sigma_site ~ normal(0, 5);
  sigma_year ~ normal(0, 5);
  sigma_prov ~ normal(0, 5);
  sigma_clone ~ exponential(0.5);
  
  mu_site ~ normal(0, 5);
  mu_year ~ normal(0, 5);
  mu_prov ~ normal(0, 5);
  mu_clone ~ normal(0, 5);
  
  alpha_ncp_site ~ normal(0,1); // non-centered hierarchical model
  alpha_cp_site ~ normal(mu_site, sigma_site); //centered hierarchical model
  
  alpha_year ~ normal(mu_year, sigma_year);
  // alpha_ncp_year ~ normal(0,1);
  // alpha_cp_year ~ normal(mu_year, sigma_year);
  //z_alpha_year ~ normal(0,1); //non-centered year
  
  // alpha_prov ~ normal(mu_prov, sigma_prov);
  alpha_ncp_prov ~ normal(0,1); // non-centered hierarchical model
  alpha_cp_prov ~ normal(mu_prov, sigma_prov); //centered hierarchical model
  
  z_alpha_clone ~ normal(0, 1); // non-centered clone
  // alpha_ncp_clone ~ normal(0,1); // non-centered hierarchical model
  // alpha_cp_clone ~ normal(mu_clone, sigma_clone); //centered hierarchical model
  
  mu ~ normal(mu_mean, mu_sigma);
  
  // sum_forcing ~ normal(mu + alpha_site[Site] + alpha_year[Year] +  alpha_prov[Provenance] +
  // (mu_clone + z_alpha_clone[Clone] * sigma_clone),
  // sigma);
  
  for (n in 1:k) {
    sftrue[n] = mu + alpha_site[Site[n]] + alpha_year[Year[n]] +  alpha_prov[Provenance[n]] +
  (mu_clone + z_alpha_clone[Clone[n]] * sigma_clone);
    sum_forcing[n] ~ censored_normal(sftrue[n], sigma, censorship[n]);
  }
  

  
  //sum_forcing ~ normal(mu + alpha_site[Site] + alpha_year[Year] + alpha_prov[Provenance] + alpha_clone[Clone], sigma);
  
//   model {
//     for ( n in 1:N ) {
//         Observations[n] ~ censored_normal(mu, sigma, CensorCodes[n]);
//     }
// }
}


// Simulate a full observation from the current value of the parameters
generated quantities {
  real sum_forcing_rep[k];
  
  // reconstruct non-centered parameters
  vector[k_Clone] alpha_clone;

  alpha_clone = mu_clone + z_alpha_clone * sigma_clone;
  
  { // Don't save tempvars
  for (i in 1:k)
  sum_forcing_rep[i] = normal_rng(mu + alpha_site[Site[i]] + alpha_year[Year[i]] + alpha_prov[Provenance[i]] + alpha_clone[Clone[i]]
  , sigma);
  }
}
