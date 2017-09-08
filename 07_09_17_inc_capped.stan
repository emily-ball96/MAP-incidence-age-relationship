data{
  int N;                 // total studies
  int K;                 // regression coefficients
  int J;                 // groups, ie regions
  int r[N];              // observed incidence
  vector[N] PYO;         // PYO, for offset
  vector[N] age;         // observed ages
  vector[N] agesquared;
  int id[N];             // to index studies by region
  //
  int N_new;             // to predict incidence for ages 0-85
  vector[N_new] a_new;   // ages to be predicted
  vector[N_new] asq_new; // new ages squared
}

transformed data{
  vector[N] log_PYO;
  log_PYO = log(PYO);
}

parameters{
  vector[J] beta1;       // regression coeffs
  vector[J] beta2;       // regression coeffs
  vector[J] beta3;       // regression coeffs
  vector[K] mu;
  real<lower=0> sigma1;
  vector<lower=0>[K] sigma2;
  real nu;
  vector[N] eps;
  vector[J] log_nu;
//  vector[N] eta;
//  real<lower=0,upper=1> p;
//  real gamma;
//  real kappa;
}

transformed parameters{
  vector[N] alpha;
//  real log_p;
//  real log_1_p;
  for (i in 1:N){
    alpha[i] = log_PYO[i] + eps[i] + beta1[id[i]] + beta2[id[i]]*age[i] + beta3[id[i]]*agesquared[i];
  }
//  log_p = log(p);
//  log_1_p = log(1-p);
}

model{
  mu ~ normal(0,10);
//  p ~ beta(3,1);
  sigma1 ~ gamma(5,500);
  sigma2 ~ gamma(1,5);
  //
//  gamma ~ normal(log(600), 0.1);
//  kappa ~ gamma(2,2);
//  for (i in 1:N){
//    eta[i] ~ normal(gamma,kappa);
//  }
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
  beta1 ~ normal(mu[1], sigma2[1]);
  beta2 ~ normal(mu[2], sigma2[2]);
  beta3 ~ normal(mu[3], sigma2[3]);
  //
  r ~ poisson_log(alpha);
//    for (i in 1:N){
//      target += log_sum_exp(log_1_p + poisson_log_lpmf(r[i]| eta[i]), log_p + poisson_log_lpmf(r[i]| alpha[i]));
//    }
}

generated quantities{
  vector[N_new] alpha_new_af;
  vector[N_new] alpha_new_am;
  vector[N_new] alpha_new_as;
  alpha_new_af = log(1000) + beta1[1] + beta2[1] * a_new + beta3[1] * asq_new;        // to predict inc
  alpha_new_am = log(1000) + beta1[2] + beta2[2] * a_new + beta3[2] * asq_new;        // to predict inc
  alpha_new_as = log(1000) + beta1[3] + beta2[3] * a_new + beta3[3] * asq_new;        // to predict inc
}

