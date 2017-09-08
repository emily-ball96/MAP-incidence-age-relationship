data {
  int<lower=0> N;                 // number of observations
  vector[N] PYO;                  // exp of offset
  vector[N] age;                  // first independent variable
  vector[N] agesquared;           // second independent variable
  int y[N];                      // vector of dependent var 
}

transformed data {
  vector[N] log_PYO;
  log_PYO = log(PYO);             // offset
}

parameters {
  vector[3] betas;                // vector of regression coeffs, 3 because 2 indep vars + 1 adjustment
}

transformed parameters {
  vector[N] alpha;
  alpha = log_PYO + betas[1] + betas[2] * age + betas[3] * agesquared;
}

model {
  y ~ poisson_log(alpha);    // having approximated INC to integer values
}
