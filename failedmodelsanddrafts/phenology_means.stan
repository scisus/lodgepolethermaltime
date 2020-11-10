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
    
    real<lower=0> mu_mean;
    real<lower=0> mu_sigma;

    vector[n] sum_forcing;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.

parameters {
    real<lower=0> mu; //accumulated forcing cannot be negative
    real<lower=0> sigma; //measurement error

    vector[n_Site] alpha_site; // site effect
    vector[n_Year] alpha_year; //year effect
    vector[n_Provenance] alpha_prov; //provenance effect
    vector[n_Clone] z_alpha_clone; //clone effect

    real sigma_site; // site effect variance
    real sigma_year; // year effect variance
    real sigma_prov; // provenance effect variance
    real sigma_clone; //clone effect variance

    real mu_site; // site effect mean
    real mu_year; // year effect mean
    real mu_prov; // provenance effect mean
    real mu_clone; //clone effect mean
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
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

    alpha_site ~ normal(mu_site, sigma_site);
    alpha_year ~ normal(mu_year, sigma_year);
    alpha_prov ~ normal(mu_prov, sigma_prov);
    z_alpha_clone ~ normal(0, 1); // non-centered clone

    mu ~ normal(mu_mean, mu_sigma);

    sum_forcing ~ normal(mu + alpha_site[Site] + alpha_year[Year] + alpha_prov[Provenance] + 
    (mu_clone + z_alpha_clone[Clone] * sigma_clone), 
    sigma);
}


// Simulate a full observation from the current value of the parameters
generated quantities {
    real y_ppc[n];
    
    // reconstruct clone offset
    vector[n_Clone] alpha_clone;
    
    alpha_clone = mu_clone + z_alpha_clone * sigma_clone;

    { // Don't save tempvars
    for (i in 1:n)
        y_ppc[i] = normal_rng(mu + alpha_site[Site[i]] + alpha_year[Year[i]] + alpha_prov[Provenance[i]] + alpha_clone[Clone[i]]
        , sigma);
        }
}
