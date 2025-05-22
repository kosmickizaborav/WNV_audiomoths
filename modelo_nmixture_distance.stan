functions {
  real marginal_likelihood_binomial(int y, real p, real lambda, real phi, int K) {
    vector[K + 1] lp;
    for (n in 0:K) {
      lp[n + 1] = neg_binomial_2_lpmf(n | lambda, phi) + binomial_lpmf(y | n, p);
    }
    return log_sum_exp(lp);
  }
}

data {
  int<lower=1> n_sites;
  int<lower=1> n_transects;
  int<lower=1> n_visits;
  int<lower=1> n_bands;
  int<lower=1> K;

  array[n_sites, n_visits, n_bands] int y;

  array[n_sites] int<lower=1, upper=n_transects> transect_id;

  int<lower=1> n_habitats;
  matrix[n_sites, n_habitats] sector_habitat;

  matrix[n_sites, n_visits] ebird_abund;
  array[n_bands] real distance_mid;
}

parameters {
  real alpha_lambda;
  vector[n_habitats] beta_sector;
  real<lower=0> sigma_transect;
  vector[n_transects] transect_eff;

  real alpha_p;
  real beta_ebird;
  real<lower=0> sigma_det;

  real<lower=0> phi;
}

transformed parameters {
  array[n_sites] real lambda;
  array[n_sites, n_visits, n_bands] real p;

  for (i in 1:n_sites) {
    real log_lambda = alpha_lambda + sector_habitat[i] * beta_sector + transect_eff[transect_id[i]];
    lambda[i] = exp(log_lambda);
    for (j in 1:n_visits) {
      real logit_p = alpha_p + beta_ebird * ebird_abund[i, j];
      for (b in 1:n_bands) {
        real sigma = exp(sigma_det);
        p[i, j, b] = inv_logit(logit_p) * exp(-pow(distance_mid[b], 2) / (2 * pow(sigma, 2)));
      }
    }
  }
}

model {
  // Priors
  alpha_lambda ~ normal(0, 2);
  beta_sector ~ normal(0, 2);
  sigma_transect ~ exponential(1);
  transect_eff ~ normal(0, sigma_transect);

  alpha_p ~ normal(0, 2);
  beta_ebird ~ normal(0, 2);
  sigma_det ~ normal(0, 1);

  phi ~ gamma(0.01, 0.01);

  // Likelihood (marginalized over N)
  for (i in 1:n_sites) {
    for (j in 1:n_visits) {
      for (b in 1:n_bands) {
        target += marginal_likelihood_binomial(y[i, j, b], p[i, j, b], lambda[i], phi, K);
      }
    }
  }
}

generated quantities {
  array[n_sites] real expected_N;
  array[n_transects] real transect_abund;

  for (i in 1:n_sites) {
    expected_N[i] = lambda[i];
  }

  for (t in 1:n_transects) {
    real sum_abund = 0;
    for (i in 1:n_sites) {
      if (transect_id[i] == t) {
        sum_abund += lambda[i];
      }
    }
    transect_abund[t] = sum_abund;
  }
}
