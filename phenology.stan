//
// This Stan program defines a multilevel factor model of phenology, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma' and heterogeneity from factors
// Site, Provenance, Year, and Clone modeled as offsets `delta`.
//
// Some Site and Provenance levels are noncentered and Clone is fully non-centered.
//


// The input data is a vector 'y' of length 'k'.
// Heterogeneity is a function of 4 factors - Site, Provenance, Year, and Clone

// The input data is a vector 'sum_forcing' of length 'k'.



data {
  int<lower=1> k; // number of observations
  vector[k] sum_forcing; // observations

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

  int<lower=0, upper=k_Provenance> k_ncp_Provenance;          // Number of noncentered sites
  int<lower=1, upper=k_Provenance> ncp_idx_Provenance[k_ncp_Provenance]; // Index of noncentered Provenances

  int<lower=0, upper=k_Provenance> k_cp_Provenance;           // Number of centered Provenances
  int<lower=1, upper=k_Provenance> cp_idx_Provenance[k_cp_Provenance];   // Index of noncentered Provenances

  // year

  // int<lower=0, upper=k_Year> k_ncp_Year;          // Number of noncentered Years
  // int<lower=1, upper=k_Year> ncp_idx_Year[k_ncp_Year]; // Index of noncentered Years
  //
  // int<lower=0, upper=k_Year> k_cp_Year;           // Number of centered Years
  // int<lower=1, upper=k_Year> cp_idx_Year[k_cp_Year];   // Index of noncentered Years

}


parameters {
  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale

  vector[k_ncp_Site] delta_ncp_site; //non-centered site parameters
  vector[k_cp_Site] delta_cp_site; //centered site parameters

  vector[k_Year] delta_year; //year offset

  vector[k_ncp_Provenance] delta_ncp_prov; //non-centered Provenance parameters
  vector[k_cp_Provenance] delta_cp_prov; //centered Provenance parameters

  vector[k_Clone] z_delta_clone; //clone offset

  real sigma_site; // site offset variance
  real sigma_year; // year offset variance
  real sigma_prov; // provenance offset variance
  real sigma_clone; //clone offset variance

  real mu_site; // site offset mean
  real mu_year; // year offset mean
  real mu_prov; // provenance offset mean
  real mu_clone; //clone offset mean
}

transformed parameters {

  // recenter individual factor levels
  vector[k_Site] delta_site;
  vector[k_Provenance] delta_prov;

  //site
  delta_site[ncp_idx_Site] = mu_site + sigma_site * delta_ncp_site;
  delta_site[cp_idx_Site] = delta_cp_site;

  //provenance
  delta_prov[ncp_idx_Provenance] = mu_prov + sigma_prov * delta_ncp_prov;
  delta_prov[cp_idx_Provenance] = delta_cp_prov;

}


model {
  // prior model

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

  delta_ncp_site ~ normal(0,1); // non-centered hierarchical model
  delta_cp_site ~ normal(mu_site, sigma_site); //centered hierarchical model

  delta_year ~ normal(mu_year, sigma_year);

  delta_ncp_prov ~ normal(0,1); // non-centered hierarchical model
  delta_cp_prov ~ normal(mu_prov, sigma_prov); //centered hierarchical model

  z_delta_clone ~ normal(0, 1); // non-centered clone

  mu ~ normal(mu_mean, mu_sigma);

  sum_forcing ~ normal(mu + delta_site[Site] + delta_year[Year] +  delta_prov[Provenance] +
  (mu_clone + z_delta_clone[Clone] * sigma_clone),
  sigma);
}


// Simulate a full observation from the current value of the parameters
generated quantities {
 real sum_forcing_rep[k];
 real log_lik[k];

  // reconstruct non-centered parameters
  vector[k_Clone] delta_clone;
  delta_clone = mu_clone + z_delta_clone * sigma_clone;


  //simulate
  {
  for (i in 1:k)
  sum_forcing_rep[i] = normal_rng(mu + delta_site[Site[i]] + delta_year[Year[i]] + delta_prov[Provenance[i]] + delta_clone[Clone[i]]
  , sigma);
  }

}


