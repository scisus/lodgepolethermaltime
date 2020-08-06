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

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> mu; //accumulated forcing cannot be 0
  real<lower=0> sigma; //measurement error
  //real<lower=0> sigma_mu; //process error
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //sigma_mu ~ exponential(0.2);
  sigma ~ exponential(1);
  mu ~ normal(350, 0.5);
  y ~ normal(mu, sigma);
}

//Simulate a full observation from the current value of the parameters
generated quantities {
    real y_ppc[N];
    { // Don't save tempvars
    for (n in 1:N)
        y_ppc[n] = normal_rng(mu, sigma);
        }
}

