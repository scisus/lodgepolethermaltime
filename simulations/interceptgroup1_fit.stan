//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N' with identifiers for the source site of each datapoint.
data {
  int<lower=1> N;
  int<lower=1> siteN;

  int<lower=1, upper=siteN> siteID[N];
  vector[N] y;

}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> alpha_pop; //accumulated forcing cannot be 0
  real<lower=0> sigma; //measurement error
  real alpha_site[siteN];
  real<lower=0> sigma_site;

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  real mu[N];

  sigma_site ~ exponential(1);
  sigma ~ exponential(1);
  alpha_pop ~ normal(350, 5);
  alpha_site[siteID] ~ normal(alpha_pop, sigma_site);

  mu = alpha_site[siteID];
  y ~ normal(mu, sigma);

}
