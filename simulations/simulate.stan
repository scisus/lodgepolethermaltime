//
  // This Stan program defines  model, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

data {
  int<lower=1> k; // total number of observations
  //vector[k] sum_forcing; // observations
  int<lower=0, upper=1> censored[k]; // censorship code (0 uncensored, 1 left, 2 right, 3 no flowering obs)
  int<lower = 0> censoring[k];

  // real<lower=0> mu_mean; // mean for prior on overall mean
  // real<lower=0> mu_sigma; // sigma for prior on overall mean (measurement variability)
  //
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

  real<lower=0> mu; //population location. accumulated forcing cannot be negative.
  real<lower=0> sigma; //population scale
  real<lower=0> sigma_cens;

  real sigma_site; // site effect variance
  real sigma_year; // year effect variance
  real sigma_prov; // provenance effect variance
  real sigma_clone; //clone effect variance

  real mu_site; // site effect mean
  real mu_year; // year effect mean
  real mu_prov; // provenance effect mean
  real mu_clone; // clone effect mean

  vector[k_Site] delta_Site;
  vector[k_Year] delta_Year;
  vector[k_Provenance] delta_Provenance;
  vector[k_Clone] delta_Clone;

}

generated quantities {
  vector[k] phi;
  vector[k] sum_forcing;

  phi = mu + delta_Site[Site] + delta_Year[Year] +  delta_Provenance[Provenance] +
    delta_Clone[Clone];

  for (n in 1:k) {
    if (censored[n] == 0) {
      sum_forcing[n] = normal_rng(phi[n], sigma);
    } else if (censored[n] == 1) {
      sum_forcing[n] = censoring[n] + normal_rng(phi[n], sigma);
    }

  }

  // sum_forcing ~ normal(mu + alpha_site[Site] + alpha_year[Year] +  alpha_prov[Provenance] +
                            // (mu_clone + z_alpha_clone[Clone] * sigma_clone),
                          // sigma);


}
