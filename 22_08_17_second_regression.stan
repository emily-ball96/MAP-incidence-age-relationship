data {
  int<lower=0> N;                // number of data points, length(INC)
  int<lower=0> K;                // number of model coefficients a_i, ie INC ~ a0*age + a1*age^2
  vector[N] y;                   // each estimate of incidence
  matrix[N,K] X;                 // mean_age and mean_age^2 of each, as a matrix
  vector[N] z;                   // vector of PYO
  real<lower=0> alpha;           // prior parameters
  real<lower=0> beta;            //
}

parameters {
  vector[K] betas;               // vector of regression coefficients 
  real<lower=0> sigma;           // error scale
}

model {
  vector[N] mu;                  // vector of expected values
  mu = exp(X*betas);             // using the log link 
  betas ~ gamma(alpha,beta);     // prior dist for coeffs
  sigma ~ cauchy(0,5);           // prior dist for error scale
  y ~ gamma(log(mu), sigma);
}