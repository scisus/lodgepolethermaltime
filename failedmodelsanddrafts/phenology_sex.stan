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
    int<lower=1> n_Sex;
    int<lower=1> n_Event;

    int Site[n];
    int Year[n];
    int Provenance[n];
    int Clone[n];
    int Sex[n];
    int Event[n];

    vector[n] sum_forcing;
}

parameters {
    real<lower=0> mu; //accumulated forcing cannot be negative
    real<lower=0> sigma; //measurement error

    vector[n_Site] alpha_site; // site effect
    vector[n_Year] alpha_year; //year effect
    vector[n_Provenance] alpha_prov; //provenance effect
    vector[n_Clone] z_alpha_clone; //clone effect
    vector[n_Sex] alpha_sex;
    ordered[n_Event] alpha_event; // effect of begin vs. end. Begin must occur before end.

    real sigma_site; // site effect variance
    real sigma_year; // year effect variance
    real sigma_prov; // provenance effect variance
    real sigma_clone; // clone effect variance
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    sigma ~ exponential(1);
    sigma_site ~ exponential(0.5);
    sigma_year ~ exponential(0.5);
    sigma_prov ~ exponential(0.5);
    sigma_clone ~ exponential(0.5);

    alpha_site ~ normal(0, sigma_site);
    alpha_year ~ normal(0, sigma_year);
    alpha_prov ~ normal(0, sigma_prov);
    z_alpha_clone ~ normal(0, 1);
    alpha_sex ~ normal(0, 14);
    alpha_event ~ normal(0, 50);

    mu ~ normal(500, 100);

    sum_forcing ~ normal(mu + alpha_site[Site] + alpha_year[Year] + alpha_prov[Provenance] + 
    (z_alpha_clone[Clone] * sigma_clone) + alpha_sex[Sex] + alpha_event[Event], 
    sigma);
}

// Simulate a full observation from the current value of the parameters
generated quantities {
    real y_ppc[n];
    
    // reconstruct clone offset
    vector[n_Clone] alpha_clone;
    
    alpha_clone = z_alpha_clone * sigma_clone;

    { // Don't save tempvars
    for (i in 1:n)
        y_ppc[i] = normal_rng(mu + alpha_site[Site[i]] + alpha_year[Year[i]] + alpha_prov[Provenance[i]] + alpha_clone[Clone[i]] + alpha_sex[Sex[i]] + alpha_event[Event[i]], sigma);
        }
}