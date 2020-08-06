// Simulate accumulated forcing data (y) from a normal distribution with mean 'mu' and standard deviation 'sigma'


data {
    int<lower=1> N; // how many data points to simulate
    real<lower=0> sigma; //measurement error
    real<lower=0> alpha_pop; //accumulated forcing mean. can't be negative

    int<lower=1> siteN; // number of sites
    int<lower=1> siteID[siteN]; // site identifiers
    real alpha_site[siteN]; // effect of each site
}

generated quantities {
    real<lower=0> y[N, siteN]; // simulated accumulated forcing
    real<lower=0> mu[siteN];

    for (n in 1:N) {
        for (s in 1:siteN) {
            mu[s] = alpha_pop + alpha_site[siteID[s]];
            y[n, s] = normal_rng(mu[s], sigma);
        }
    }
}

