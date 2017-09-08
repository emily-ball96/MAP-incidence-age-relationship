data {
  int<lower=0> N;                 // number of observations
  vector[N] PYO;                  // exp of offset
  vector[N] age;                  // first independent variable
  vector[N] agesquared;           // second independent variable
  int y[N];                       // vector of dependent var 
}

transformed data {
  vector[N] log_PYO;
  log_PYO = log(PYO);             // offset
}

parameters {
  vector[3] gammas;               //
  vector<lower=0>[3] sigmas;
  vector[3] betas;
}

transformed parameters {
  vector[N] alpha;
  alpha = log_PYO + betas[1] + betas[2] * age + betas[3] * agesquared;
}

model {
  // priors
  gammas ~ normal(0,1);
  sigmas ~ cauchy(0,5);
  betas ~ normal(gammas,sigmas);
  y ~ poisson_log(alpha);    
}
