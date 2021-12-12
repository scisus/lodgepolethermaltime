// generated with brms 2.16.3
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  vector<lower=0>[N] se;  // known sampling error
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  vector<lower=0>[N] se2 = square(se);
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    target += normal_lpdf(Y | mu, sqrt(square(sigma) + se2));
  }
  // priors including constants
  target += normal_lpdf(b | 0,5);
  target += normal_lpdf(Intercept | 0,10);
  target += normal_lpdf(sigma | 0,9)
    - 1 * normal_lccdf(0 | 0,9);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // additionally sample draws from priors
  real prior_b = normal_rng(0,5);
  real prior_Intercept = normal_rng(0,10);
  real prior_sigma = normal_rng(0,9);
  // use rejection sampling for truncated priors
  while (prior_sigma < 0) {
    prior_sigma = normal_rng(0,9);
  }
}
