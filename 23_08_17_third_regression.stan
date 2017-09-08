data {
  int<lower=0> N;                // number of data points, length(INC)
  int<lower=0> J;                // number of groups (regions??)
  int K;                         // number of columns in model matrix (2??)
  int id[N];                     // vector of group indices
  vector<lower=0>[N] y;                   // response variable, INC
  matrix[N,K] X;                 // model matrix, mean_age and mean_age^2??
  
//  vector[N] z;                   // vector of PYO
//  real<lower=0> alpha;           // prior parameters
//  real<lower=0> beta;            //
}

parameters {
  vector[K] gamma;               // vector of population-level regression coefficients
  vector<lower=0>[K] tau;                 // standard deviation of the regression coefficients
  vector[K] betas[J];            // matrix of group-level regression coefficients
  real<lower=0> sigma;           // standard deviation of the individual observations
}

model {
  vector[N] mu;                  // vector of expected values, linear predictor
  
  // priors
  gamma ~ normal(0,5);           // weakly informative prior on regression coeff
  tau ~ cauchy(0,2.5);          // weakly informative prior on standard deviation
  sigma ~ gamma(2,0.1);          // weakly informative prior on standard deviation
  for(j in 1:J){
   betas[j] ~ normal(gamma,tau); //fill the matrix of group-level regression coefficients 
  }
  for(n in 1:N){
    mu[n] = exp(X[n] * betas[id[n]]);//update lin. predictor w/grouplevel regression coeffs 
  }
  
  //likelihood
  y ~ gamma(mu,sigma);
}
