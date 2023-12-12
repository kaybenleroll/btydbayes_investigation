functions {
  #include util_functions.stan
}

data {
  int<lower=1> n_cust;                 // number of customers
  int<lower=1> n_tnx;                  // number of transaction amounts

  int<lower=1>[n_tnx] customer_id;

  vector<lower=0>[n_cust] t_x;         // time to most recent purchase
  vector<lower=0>[n_cust] T_cal;       // total observation time
  vector<lower=0>[n_cust] x;           // number of purchases observed

  vector<lower=0>[n_tnx] amt;          // transaction amounts

  real<lower=0> lambda_mn;             // prior mean for lambda
  real<lower=0> lambda_cv;             // prior cv   for lambda

  real<lower=0> mu_mn;                 // prior mean for mu
  real<lower=0> mu_cv;                 // prior mean for mu

  real<lower=0> custhier_mn;           // prior mean for lambda
  real<lower=0> custhier_cv;           // prior cv   for lambda

  real<lower=0> tnx_cv;                // transaction amount cov
}

transformed data {
  real<lower=0> r     = 1 / (lambda_cv * lambda_cv);
  real<lower=0> alpha = 1 / (lambda_cv * lambda_cv * lambda_mn);

  real<lower=0> s    = 1 / (mu_cv * mu_cv);
  real<lower=0> beta = 1 / (mu_cv * mu_cv * mu_mn);
}


parameters {
  vector<lower=0>[n_cust] lambda; // purchase rate
  vector<lower=0>[n_cust] mu;     // lifetime dropout rate
}


model {
  // setting priors
  lambda ~ gamma(r, alpha);
  mu     ~ gamma(s,  beta);

  target += calculate_pnbd_loglik(n_cust, lambda, mu, x, t_x, T_cal);
}

generated quantities {
  vector[n_cust] p_alive;         // Probability that they are still "alive"

  p_alive = 1 ./ (1 + mu ./ (mu + lambda) .* (exp((lambda + mu) .* (T_cal - t_x)) - 1));
}
