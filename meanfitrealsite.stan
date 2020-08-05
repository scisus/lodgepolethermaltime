//
// This Stan program defines  model, with a
// vector of values 'sum_forcing' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//

    // The input data is a vector 'y' of length 'N'.
data {
    int<lower=0> n;
    int<lower=0> n_Site;
    int Site[n];
    vector[n] sum_forcing;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
    real<lower=0> mu; //accumulated forcing cannot be 0
    real<lower=0> sigma; //measurement error
    vector[n_Site] site_offset; // site effect
    real sigma_site; // site effect variance
    //real<lower=0> sigma_mu; //process error
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    sigma ~ exponential(1);
    sigma_site ~ exponential(1);
    site_offset ~ normal(0, sigma_site);
    mu ~ normal(350, 20);
    sum_forcing ~ normal(mu + site_offset[Site], sigma);
}

//Simulate a full observation from the current value of the parameters
generated quantities {
    real y_ppc[n];
    { // Don't save tempvars
    for (i in 1:n)
        y_ppc[i] = normal_rng(mu + site_offset[Site[i]], sigma);
        }
}

