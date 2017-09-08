data {
  int N;               // total studies
  int K;               // regression coefficients
  int J;               // groups, ie regions
  int r[N];            // observed incidence
  vector[N] PYO;       // PYO, for offset
  vector[N] age;
  vector[N] agesquared;
  int id[N];           // to index studies by region
}

transformed data {
  vector[N] log_PYO;
  log_PYO = log(PYO);
}

parameters {
  vector[J] beta[K];
  vector[K] mu;
  real<lower=0> sigma1;
  vector<lower=0>[K] sigma2;
  real nu;
  vector[N] eps;
  vector[J] log_nu;
}

transformed parameters {
  vector[N] alpha;
  for (i in 1:N){
    alpha[i] = log_PYO[i] + eps[i] + beta[1][id[i]] + beta[2][id[i]]*age[i] + beta[3][id[i]]*agesquared[i];
  }
}

model {
  mu ~ normal(0,10);
  sigma1 ~ gamma(1,10);
  for (i in 1:K){
    sigma2[i] ~ gamma(1,5);
  }
  //
  for (i in 1:J){
    log_nu[i] ~ normal(nu, sigma1);            // group level variance
  }
  //
  nu ~ normal(-4,1);
  //
  for (i in 1:N){
    eps[i] ~ normal(0, exp(log_nu[id[i]]));    // individual level variance
  }
  //
  for (i in 1:K){
    beta[i] ~ normal(mu[i], sigma2[i]);
  }
  //
  r ~ poisson_log(alpha);
}
