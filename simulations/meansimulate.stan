// Simulate accumulated forcing data (y) from a normal distribution with mean 'mu' and standard deviation 'sigma'


data {
    int<lower=1> N; // how many data points to simulate
    // real<lower=0> sigma_mu; // process error
    real<lower=0> sigma; //measurement error
    real<lower=0> mu; //accumulated forcing mean. can't be negative
}

generated quantities {

    // real<lower=0> sigma_mu;
    // real<lower=0> mu; // accumulated forcing units cannot be negative
    // real<lower=0> sigma;

    real<lower=0> y[N]; // simulated accumulated forcing

    // sigma_mu = exponential_rng(0.2);
    // mu = normal_rng(350, sigma_mu);
    // sigma = exponential_rng(0.5);

    for (n in 1:N) {
    y[n] = normal_rng(mu, sigma);
    }
}

