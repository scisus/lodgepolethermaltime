//
// This Stan program defines  model, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

    // The input data is a vector 'y' of length 'N'.
data {
    int<lower=1> n;
    int<lower=1> n_Site;
    int<lower=1> n_SPU_Name;

    int Site[n];
    int SPU_Name[n];
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
    vector[n_SPU_Name] prov_offset; //provenance offset

    real sigma_site; // site effect variance
    real sigma_prov; //provenance effect variance
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    sigma ~ exponential(1);
    sigma_site ~ exponential(2);
    sigma_prov ~ exponential(2);

    site_offset ~ normal(0, sigma_site);
    prov_offset ~ normal(0, sigma_prov);

    mu ~ normal(350, 20);

    sum_forcing ~ normal(mu + site_offset[Site] + prov_offset[SPU_Name], sigma);
}

//Simulate a full observation from the current value of the parameters
generated quantities {
    real y_ppc[n];

    { // Don't save tempvars
    for (i in 1:n)
        y_ppc[i] = normal_rng(mu + site_offset[Site[i]] + prov_offset[SPU_Name[i]], sigma);
        }
}

