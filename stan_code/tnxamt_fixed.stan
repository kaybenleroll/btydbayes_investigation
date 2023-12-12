functions {
  #include util_functions.stan
}

data {
  int<lower=1> n_cust;                 // number of customers
  int<lower=1> n_tnx;                  // number of transaction amounts

  array[n_tnx] int<lower=1> cust_id;

  vector<lower=0>[n_tnx] amt;          // transaction amounts

  real<lower=0> custhier_mn;           // prior mean for lambda
  real<lower=0> custhier_cv;           // prior cv   for lambda

  real<lower=0> amt_cv;                // transaction amount cov
}

transformed data {
  real<lower=0> custhier_r     = 1 / (custhier_cv * custhier_cv);
  real<lower=0> custhier_alpha = 1 / (custhier_cv * custhier_cv * custhier_mn);

  real<lower=0> amt_r = 1 / (amt_cv * amt_cv);
}


parameters {
  vector<lower=0>[n_cust] amt_mn;     // customer tnx amount mean
}


transformed parameters {
  vector<lower=0>[n_cust] amt_alpha = 1 / (amt_cv * amt_cv * amt_mn);
}


model {
  // setting priors
  amt_mn ~ gamma(custhier_r, custhier_alpha);

  for(i in 1:n_tnx) {
    amt[i] ~ gamma(amt_r, amt_alpha[cust_id[i]]);
  }
}

