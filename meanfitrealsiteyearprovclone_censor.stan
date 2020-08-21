//
// This Stan program defines  model, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> n;
  int<lower=1> n_Site;
  int<lower=1> n_Year;
  int<lower=1> n_Provenance;
  int<lower=1> n_Clone;
  
  int Site[n];
  int Year[n];
  int Provenance[n];
  int Clone[n];
  
  vector[n] sum_forcing_obs;
}



parameters {
  
  real y_cens[n];
  real<lower=0> mu; //accumulated forcing cannot be negative
  real<lower=0> sigma_measure; //measurement error
  real<lower=0> sigma_process; //measurement error
  
  vector[n_Site] site_offset; // site effect
  vector[n_Year] year_offset; //year effect
  vector[n_Provenance] prov_offset; //provenance effect
  vector[n_Clone] z_clone_offset; //clone effect
  
  real sigma_site; // site effect variance
  real sigma_year; // year effect variance
  real sigma_prov; // provenance effect variance
  real sigma_clone; //clone effect variance
  
  real mu_site; // site effect mean
  real mu_year; // year effect mean
  real mu_prov; // provenance effect mean
  real mu_clone; //clone effect mean
  
  real<lower=0> sum_forcing_true[n];
}

transformed parameters {
  // dummy variable d that must be > 0
  // sample is rejected if d <= 0
  real<upper=0> d[n];
  
  for (i in 1:n)
  // set d to be the difference between the estimate
  // of the true value and the observed value
  // This must be <= 0 because if a tree is observed flowering,
  // we know the forcing threshold is at most the
  // forcing accumulation on the observation day. We don't know if we are observing it 
  // on the first day that it has begun flowering, however. So
  // it could have started flowering at a lower forcing accumulation
  d[i] = sum_forcing_true[i] - sum_forcing_obs[i];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sigma_measure ~ exponential(1);
  sigma_process ~ exponential(1);
  sigma_site ~ exponential(0.5);
  sigma_year ~ exponential(0.5);
  sigma_prov ~ exponential(0.5);
  sigma_clone ~ exponential(0.5);
  
  mu_site ~ normal(0,14);
  mu_year ~ normal(0,14);
  mu_prov ~ normal(0,14);
  mu_clone ~ normal(0,14);
  
  site_offset ~ normal(mu_site, sigma_site);
  year_offset ~ normal(mu_year, sigma_year);
  prov_offset ~ normal(mu_prov, sigma_prov);
  z_clone_offset ~ normal(0, 1);
  
  mu ~ normal(500, 150);
  
  sum_forcing_true ~ normal(mu + 
  site_offset[Site] + year_offset[Year] + prov_offset[Provenance] + 
  (mu_clone + z_clone_offset[Clone] * sigma_clone), 
  sigma_process);
  
  sum_forcing_obs ~ normal(sum_forcing_true, sigma_measure);
}

// Simulate a full observation from the current value of the parameters
generated quantities {
  real y_true_ppc[n];
  real y_obs_ppc[n];
  
  // reconstruct clone offset
  vector[n_Clone] clone_offset;
  
  clone_offset = mu_clone + z_clone_offset * sigma_clone;
  
  { // Don't save tempvars
  for (i in 1:n) {
    y_true_ppc[i] = normal_rng(mu + site_offset[Site[i]] + year_offset[Year[i]] + prov_offset[Provenance[i]] + clone_offset[Clone[i]], sigma_process);
    y_obs_ppc[i] = normal_rng(y_true_ppc[i], sigma_measure);
  }
  }
  
}

