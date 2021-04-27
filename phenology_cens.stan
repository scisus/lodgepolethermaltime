data {
      int<lower=1> k; // total number of observations


      // indexing for censorship
      int<lower=0, upper=k> k_uncens; // Number of uncensored obs
      int<lower=1, upper=k> idx_uncens[k_uncens]; // Index of uncensored obs

      int<lower=0, upper=k> k_cens; //Number of possibly censored obs
      int<lower=1, upper=k> idx_cens[k_cens]; //Index of censored obs

      vector[k] sum_forcing; // observations
      vector[k_cens] period_cens; // length of phenological period for potentially censored observations
      //int<lower=0, upper=1> censored[k]; // censorship code (0 uncensored, 1 possible left censorship)
      //vector[k_cens] period_cens; // length of observed phenological period after potentially censored observations

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
      //real<lower=0> sigma_cens;


      real<upper=0> beta; // censorship coefficients
      real<upper=0> alpha;


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
      // declarations
      // censorship
      vector[k] censored;
      vector[k_uncens] known_cens = rep_vector(0, k_uncens);


      // recenter individual parameters
      vector[k_Site] alpha_site;
      vector[k_Provenance] alpha_prov;

      // calculations
      // recombine censorship
      censored[idx_uncens] = known_cens;
      censored[idx_cens] = mod_cens;

      //recenter indiv params
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
      int<lower=0, upper = 1> mod_cens[k_cens]; // modelled censorship status


      //censorship
      beta ~ normal(0,1);
      alpha ~ normal(0,1);

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

      alpha_ncp_site ~ normal(0,1); // non-centered hierarchical model for site
      alpha_cp_site ~ normal(mu_site, sigma_site); //centered hierarchical model for site

      alpha_year ~ normal(mu_year, sigma_year);

      // alpha_prov ~ normal(mu_prov, sigma_prov);
      alpha_ncp_prov ~ normal(0,1); // non-centered hierarchical model for provenance
      alpha_cp_prov ~ normal(mu_prov, sigma_prov); //centered hierarchical model for provenance

      z_alpha_clone ~ normal(0, 1); // non-centered clone

      mu ~ normal(mu_mean, mu_sigma);

      forcing_mu = mu + alpha_site[Site] + alpha_year[Year] +  alpha_prov[Provenance] +
      (mu_clone + z_alpha_clone[Clone] * sigma_clone);

      mod_cens ~ bernoulli_logit(alpha + beta * period_cens);

      for (n in 1:k) {
        if (censored[n] == 0) {
          sum_forcing[n] ~ normal(forcing_mu[n], sigma);
        } else if (censored[n] == 1) {
          target += normal_lcdf(sum_forcing[n] | forcing_mu[n], sigma);
        }

      }

      // sum_forcing ~ normal(mu + alpha_site[Site] + alpha_year[Year] +  alpha_prov[Provenance] +
      // (mu_clone + z_alpha_clone[Clone] * sigma_clone),
      // sigma);


}
