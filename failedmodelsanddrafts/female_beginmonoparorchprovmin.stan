// generated with brms 2.16.1
functions {
  /* compute monotonic effects
   * Args:
   *   scale: a simplex parameter
   *   i: index to sum over the simplex
   * Returns:
   *   a scalar between 0 and 1
   */
  real mo(vector scale, int i) {
    if (i == 0) {
      return 0;
    } else {
      return rows(scale) * sum(scale[1:i]);
    }
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=-1,upper=2> cens[N];  // indicates censoring
  vector[N] rcens;  // right censor points for interval censoring
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Ksp;  // number of special effects terms
  int<lower=1> Imo;  // number of monotonic variables
  int<lower=1> Jmo[Imo];  // length of simplexes
  int Xmo_1[N];  // monotonic variable
  vector[Jmo[1]] con_simo_1;  // prior concentration of monotonic simplex
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
  // data for group-level effects of ID 5
  int<lower=1> N_5;  // number of grouping levels
  int<lower=1> M_5;  // number of coefficients per level
  int<lower=1> J_5[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_5_1;
  // data for group-level effects of ID 6
  int<lower=1> N_6;  // number of grouping levels
  int<lower=1> M_6;  // number of coefficients per level
  int<lower=1> J_6[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_6_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
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
  simplex[Jmo[1]] simo_1;  // monotonic simplex
  vector[Ksp] bsp;  // special effects coefficients
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  vector[N_2] z_2[M_2];  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  vector[N_3] z_3[M_3];  // standardized group-level effects
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations
  vector[N_4] z_4[M_4];  // standardized group-level effects
  vector<lower=0>[M_5] sd_5;  // group-level standard deviations
  vector[N_5] z_5[M_5];  // standardized group-level effects
  vector<lower=0>[M_6] sd_6;  // group-level standard deviations
  vector[N_6] z_6[M_6];  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_3] r_3_1;  // actual group-level effects
  vector[N_4] r_4_1;  // actual group-level effects
  vector[N_5] r_5_1;  // actual group-level effects
  vector[N_6] r_6_1;  // actual group-level effects
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
  r_3_1 = (sd_3[1] * (z_3[1]));
  r_4_1 = (sd_4[1] * (z_4[1]));
  r_5_1 = (sd_5[1] * (z_5[1]));
  r_6_1 = (sd_6[1] * (z_6[1]));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + Xc * b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += (bsp[1]) * mo(simo_1, Xmo_1[n]) + r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n] + r_4_1[J_4[n]] * Z_4_1[n] + r_5_1[J_5[n]] * Z_5_1[n] + r_6_1[J_6[n]] * Z_6_1[n];
    }
    for (n in 1:N) {
    // special treatment of censored data
      if (cens[n] == 0) {
        target += normal_lpdf(Y[n] | mu[n], sigma);
      } else if (cens[n] == 1) {
        target += normal_lccdf(Y[n] | mu[n], sigma);
      } else if (cens[n] == -1) {
        target += normal_lcdf(Y[n] | mu[n], sigma);
      } else if (cens[n] == 2) {
        target += log_diff_exp(
          normal_lcdf(rcens[n] | mu[n], sigma),
          normal_lcdf(Y[n] | mu[n], sigma)
        );
      }
    }
  }
  // priors including constants
  target += student_t_lpdf(b | 3,0,5);
  target += normal_lpdf(Intercept | 400,100);
  target += dirichlet_lpdf(simo_1 | con_simo_1);
  target += student_t_lpdf(bsp | 3,0,5);
  target += normal_lpdf(sigma | 0,15)
    - 1 * normal_lccdf(0 | 0,15);
  target += normal_lpdf(sd_1 | 0,9)
    - 1 * normal_lccdf(0 | 0,9);
  target += std_normal_lpdf(z_1[1]);
  target += normal_lpdf(sd_2 | 0,9)
    - 1 * normal_lccdf(0 | 0,9);
  target += std_normal_lpdf(z_2[1]);
  target += normal_lpdf(sd_3 | 0,9)
    - 1 * normal_lccdf(0 | 0,9);
  target += std_normal_lpdf(z_3[1]);
  target += normal_lpdf(sd_4 | 0,9)
    - 1 * normal_lccdf(0 | 0,9);
  target += std_normal_lpdf(z_4[1]);
  target += normal_lpdf(sd_5 | 0,9)
    - 1 * normal_lccdf(0 | 0,9);
  target += std_normal_lpdf(z_5[1]);
  target += normal_lpdf(sd_6 | 0,9)
    - 1 * normal_lccdf(0 | 0,9);
  target += std_normal_lpdf(z_6[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // additionally sample draws from priors
  real prior_b = student_t_rng(3,0,5);
  real prior_Intercept = normal_rng(400,100);
  simplex[Jmo[1]] prior_simo_1 = dirichlet_rng(con_simo_1);
  real prior_bsp = student_t_rng(3,0,5);
  real prior_sigma = normal_rng(0,15);
  real prior_sd_1 = normal_rng(0,9);
  real prior_sd_2 = normal_rng(0,9);
  real prior_sd_3 = normal_rng(0,9);
  real prior_sd_4 = normal_rng(0,9);
  real prior_sd_5 = normal_rng(0,9);
  real prior_sd_6 = normal_rng(0,9);
  // use rejection sampling for truncated priors
  while (prior_sigma < 0) {
    prior_sigma = normal_rng(0,15);
  }
  while (prior_sd_1 < 0) {
    prior_sd_1 = normal_rng(0,9);
  }
  while (prior_sd_2 < 0) {
    prior_sd_2 = normal_rng(0,9);
  }
  while (prior_sd_3 < 0) {
    prior_sd_3 = normal_rng(0,9);
  }
  while (prior_sd_4 < 0) {
    prior_sd_4 = normal_rng(0,9);
  }
  while (prior_sd_5 < 0) {
    prior_sd_5 = normal_rng(0,9);
  }
  while (prior_sd_6 < 0) {
    prior_sd_6 = normal_rng(0,9);
  }
}