// model censorship
data {
      int<lower=1> k_cens; // total number of potentially censored observations
      vector[k_cens] period_cens; // length of phenological period for potentially censored observations
}

parameters {
  real<upper=0> beta; // censorship coefficients
  real<upper=0> alpha;

  //vector[k_uncens] known_cens;
  real mod_cens;


}

  transformed parameters {
      // declarations
      // censorship
      vector[k] censored;
      vector[k_uncens] known_cens = rep_vector(0, k_uncens);

      // calculations
      // recombine censorship
      censored[idx_uncens] = known_cens;
      censored[idx_cens] = mod_cens;

}

model {
  alpha ~ normal(0,1);
  beta ~ normal(0,1);
  mod_cens ~ bernoulli_logit(alpha + beta * period_cens);
}
