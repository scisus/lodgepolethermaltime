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

    vector[n] sum_forcing;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.

  // vector[Nsite] z_site;
  // vector[Nprovenance] z_prov;
  // vector[Nclone] z_clone;
  // vector[Nyear] z_year; //uncentered
parameters {
    real<lower=0> mu; //accumulated forcing cannot be negative
    real<lower=0> sigma; //measurement error

    vector[n_Site] site_offset; // site effect
    vector[n_Year] year_offset; //year effect
    vector[n_Provenance] prov_offset; //provenance effect
    vector[n_Clone] clone_offset; //clone effect

    real sigma_site; // site effect variance
    real sigma_year; // year effect variance
    real sigma_prov; // provenance effect variance
    real sigma_clone; //clone effect variance
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    sigma ~ exponential(1);
    sigma_site ~ exponential(1);
    sigma_year ~ exponential(1);
    sigma_prov ~ exponential(1);
    sigma_clone ~ exponential(1);

    site_offset ~ normal(0, sigma_site);
    year_offset ~ normal(0, sigma_year);
    prov_offset ~ normal(0, sigma_prov);
    clone_offset ~ normal(0, sigma_clone);

    mu ~ normal(350, 20);

    sum_forcing ~ normal(mu + site_offset[Site] + year_offset[Year] + prov_offset[Provenance] + clone_offset[Clone], sigma);
}

//Simulate a full observation from the current value of the parameters
generated quantities {
    real y_ppc[n];

    { // Don't save tempvars
    for (i in 1:n)
        y_ppc[i] = normal_rng(mu + site_offset[Site[i]] + year_offset[Year[i]] + prov_offset[Provenance[i]] + clone_offset[Clone[i]], sigma);
        }
}


