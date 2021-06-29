// generated with brms 2.15.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=-1,upper=2> cens[N];  // indicates censoring
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  int<lower=1> J_3[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_1;
  // data for group-level effects of ID 4
  int<lower=1> N_4;  // number of grouping levels
  int<lower=1> M_4;  // number of coefficients per level
  int<lower=1> J_4[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_4_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // residual SD
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  vector[N_3] z_3[M_3];  // standardized group-level effects
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations
  vector[N_4] z_4[M_4];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_3] r_3_1;  // actual group-level effects
  vector[N_4] r_4_1;  // actual group-level effects
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
  r_3_1 = (sd_3[1] * (z_3[1]));
  r_4_1 = (sd_4[1] * (z_4[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + rep_vector(0.0, N);
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n] + r_4_1[J_4[n]] * Z_4_1[n];
    }
    for (n in 1:N) {
    // special treatment of censored data
      if (cens[n] == 0) {
        target += normal_lpdf(Y[n] | mu[n], sigma);
      } else if (cens[n] == 1) {
        target += normal_lccdf(Y[n] | mu[n], sigma);
      } else if (cens[n] == -1) {
        target += normal_lcdf(Y[n] | mu[n], sigma);
      }
    }
  }
  // priors including constants
  target += normal_lpdf(Intercept | 335,40);
  target += normal_lpdf(sigma | 0,20)
    - 1 * normal_lccdf(0 | 0,20);
  target += normal_lpdf(sd_1 | 0,10)
    - 1 * normal_lccdf(0 | 0,10);
  target += std_normal_lpdf(z_1[1]);
  target += normal_lpdf(sd_2 | 0,10)
    - 1 * normal_lccdf(0 | 0,10);
  target += std_normal_lpdf(z_2[1]);
  target += normal_lpdf(sd_3 | 0,10)
    - 1 * normal_lccdf(0 | 0,10);
  target += std_normal_lpdf(z_3[1]);
  target += normal_lpdf(sd_4 | 0,10)
    - 1 * normal_lccdf(0 | 0,10);
  target += std_normal_lpdf(z_4[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  // additionally draw samples from priors
  real prior_Intercept = normal_rng(335,40);
  real prior_sigma = normal_rng(0,20);
  real prior_sd_1 = normal_rng(0,10);
  real prior_sd_2 = normal_rng(0,10);
  real prior_sd_3 = normal_rng(0,10);
  real prior_sd_4 = normal_rng(0,10);
  // use rejection sampling for truncated priors
  while (prior_sigma < 0) {
    prior_sigma = normal_rng(0,20);
  }
  while (prior_sd_1 < 0) {
    prior_sd_1 = normal_rng(0,10);
  }
  while (prior_sd_2 < 0) {
    prior_sd_2 = normal_rng(0,10);
  }
  while (prior_sd_3 < 0) {
    prior_sd_3 = normal_rng(0,10);
  }
  while (prior_sd_4 < 0) {
    prior_sd_4 = normal_rng(0,10);
  }
}
