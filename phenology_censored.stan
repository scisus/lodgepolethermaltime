//
// This Stan program defines  model, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

// The input data is a vector 'y' of length 'k'.

// functions {
  //   // from example by Christiaan van Dorp [https://tbz533.blogspot.com/2019/07/neat-encoding-of-censored-data-in-stan.html]
  //     real censored_normal_lpdf(real x, real mu, real sigma, int cc) {
    //         real ll;
    //         if ( cc == 0 ) { // uncensored
    //             ll = normal_lpdf(x | mu, sigma);
    //         } else if ( cc == 1 ) { // left-censored
    //             ll = normal_lcdf(x | mu, sigma);
    //         } else if ( cc == 2 ) { // right-censored
    //             ll = normal_lccdf(x | mu, sigma);
    //         } else if ( cc == 3 ) { // missing data
    //             ll = 0.0;
    //         } else { // any other censoring code is invalid
    //             reject("invalid censoring code in censored_normal_lpdf");
    //         }
    //         return ll;
    //     }
    // }

data {
      int<lower=1> k; // total number of observations
      vector[k] sum_forcing; // observations
      int<lower=0, upper=1> censored[k]; // censorship code (0 uncensored, 1 left, 2 right, 3 no flowering obs)

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

      // years

      int<lower=0, upper=k_Year> k_ncp_Year;          // Number of noncentered years
      int<lower=1, upper=k_Year> ncp_idx_Year[k_ncp_Year]; // Index of noncentered years

      int<lower=0, upper=k_Year> k_cp_Year;           // Number of centered years
      int<lower=1, upper=k_Year> cp_idx_Year[k_cp_Year];   // Index of noncentered years
}


parameters {
      real<lower=0> mu; //population location. accumulated forcing cannot be negative.
      real<lower=0> sigma; //population scale
      //real<lower=0> sigma_cens;

      vector[k_ncp_Site] delta_ncp_site; //non-centered site parameters
      vector[k_cp_Site] delta_cp_site; //centered site parameters

      vector[k_ncp_Provenance] delta_ncp_prov; //non-centered Provenance parameters
      vector[k_cp_Provenance] delta_cp_prov; //centered Provenance parameters

      //vector[k_Year] delta_year; //year effect
      //vector[k_Year] z_delta_year;
      vector[k_ncp_Year] delta_ncp_year; //non-centered year parameters
      vector[k_cp_Year] delta_cp_year; //centered year parameters

      vector[k_Clone] z_delta_clone; //clone effect

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
      vector[k_Site] delta_site;
      vector[k_Provenance] delta_prov;
      vector[k_Year] delta_year;

      //site
      delta_site[ncp_idx_Site] = mu_site + sigma_site * delta_ncp_site;
      delta_site[cp_idx_Site] = delta_cp_site;

      //provenance
      delta_prov[ncp_idx_Provenance] = mu_prov + sigma_prov * delta_ncp_prov;
      delta_prov[cp_idx_Provenance] = delta_cp_prov;

      //year
      delta_year[ncp_idx_Year] = mu_year + sigma_year * delta_ncp_year;
      delta_year[cp_idx_Year] = delta_cp_year;

}

    // The model to be estimated. We model the output
    // 'y' to be normally distributed with mean 'mu'
    // and standard deviation 'sigma'.
model {
      // prior model
      vector[k] forcing_mu;
      sigma ~ exponential(1);
      // sigma_cens ~ exponential(1);
      // these are half normals
      sigma_site ~ normal(0, 5);
      sigma_year ~ normal(0, 5);
      sigma_prov ~ normal(0, 5);
      sigma_clone ~ exponential(0.5);

      mu_site ~ normal(0, 5);
      mu_year ~ normal(0, 5);
      mu_prov ~ normal(0, 5);
      mu_clone ~ normal(0, 5);

      delta_ncp_site ~ normal(0,1); // non-centered hierarchical model for site
      delta_cp_site ~ normal(mu_site, sigma_site); //centered hierarchical model for site

      //delta_year ~ normal(mu_year, sigma_year);

      delta_ncp_year ~ normal(0,1); // non-centered hierarchical model for site
      delta_cp_year ~ normal(mu_year, sigma_year); //centered hierarchical model for site

      // delta_prov ~ normal(mu_prov, sigma_prov);
      delta_ncp_prov ~ normal(0,1); // non-centered hierarchical model for provenance
      delta_cp_prov ~ normal(mu_prov, sigma_prov); //centered hierarchical model for provenance

      z_delta_clone ~ normal(0, 1); // non-centered clone

      mu ~ normal(mu_mean, mu_sigma);

      forcing_mu = mu + delta_site[Site] + delta_year[Year] +  delta_prov[Provenance] +
      (mu_clone + z_delta_clone[Clone] * sigma_clone);

      for (n in 1:k) {
        if (censored[n] == 0) {
           sum_forcing[n] ~ normal(forcing_mu[n], sigma);
        } else if (censored[n] == 1) {
          target += normal_lcdf(sum_forcing[n] | forcing_mu[n], sigma);
        }

      }

      // sum_forcing ~ normal(mu + delta_site[Site] + delta_year[Year] +  delta_prov[Provenance] +
      // (mu_clone + z_delta_clone[Clone] * sigma_clone),
      // sigma);


}
