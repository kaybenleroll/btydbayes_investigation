functions {
  #include util_functions.stan
}


data {
  int<lower=1> n;           // number of customers
  int<lower=1> n_cohort_id; // number of customer cohorts

  array[n] int<lower=1> cohort_id; // index for the customer cohort

  vector<lower=0>[n] t_x;   // time to most recent purchase
  vector<lower=0>[n] T_cal; // total observation time
  vector<lower=0>[n] x;     // number of purchases observed

  real<lower=0> hier_lambda_mn_p1; // p1 for lambda_mn hierarchical prior
  real<lower=0> hier_lambda_mn_p2; // p2 for lambda_mn hierarchical prior

  vector<lower=0>[n_cohort_id] hier_mu_mn_p1;     // p1 for mu_mn hierarchical prior
  vector<lower=0>[n_cohort_id] hier_mu_mn_p2;     // p2 for mu_mn hierarchical prior

  real<lower=0> lambda_cv;         // prior cv   for lambda
  real<lower=0> mu_cv;             // prior mean for mu
}


transformed data {
  real<lower=0> hier_lambda_shape = 1 / (hier_lambda_mn_p2 * hier_lambda_mn_p2);
  real<lower=0> hier_lambda_rate  = 1 / (hier_lambda_mn_p2 * hier_lambda_mn_p2 * hier_lambda_mn_p1);

  vector<lower=0>[n_cohort_id] hier_mu_shape = 1 ./ (hier_mu_mn_p2 .* hier_mu_mn_p2);
  vector<lower=0>[n_cohort_id] hier_mu_rate  = 1 ./ (hier_mu_mn_p2 .* hier_mu_mn_p2 .* hier_mu_mn_p1);

  real<lower=0> r = 1 / (lambda_cv * lambda_cv);
  real<lower=0> s = 1 / (mu_cv * mu_cv);
}


parameters {
  real<lower=0> lambda_mn;

  vector<lower=0>[n_cohort_id] mu_mn;

  vector<lower=0>[n] lambda; // purchase rate
  vector<lower=0>[n] mu;     // lifetime dropout rate
}


transformed parameters {
  real<lower=0> alpha = 1 / (lambda_cv * lambda_cv * lambda_mn);

  vector<lower=0>[n_cohort_id] beta  = 1 / ((mu_cv * mu_cv) * mu_mn);
}


model {
  // creating hierarchical values
  lambda_mn ~ gamma(hier_lambda_shape, hier_lambda_rate);
  mu_mn     ~ gamma(hier_mu_shape,     hier_mu_rate);

  // setting priors
  lambda ~ gamma(r, alpha);

  for(i in 1:n) {
    mu[i] ~ gamma(s,  beta[cohort_id[i]]);
  }

  target += calculate_pnbd_loglik(n, lambda, mu, x, t_x, T_cal);
}


generated quantities {
  vector[n] p_alive;         // Probability that they are still "alive"

  p_alive = 1 ./ (1 + mu ./ (mu + lambda) .* (exp((lambda + mu) .* (T_cal - t_x)) - 1));
}
