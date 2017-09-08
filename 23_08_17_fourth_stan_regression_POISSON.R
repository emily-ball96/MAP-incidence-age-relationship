rm(list = ls())
setwd("C:/Users/scat6434/Documents/LowTransmissionPfalciparum")
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PfPR2_10 < 0.025 & all.dat$SPECIES == "Pf", ]
##filt.dat <- left_join(data.frame(REGION = c("Africa+", "America", "CSE Asia")),filt.dat,by="REGION") # order rows by region

mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2
d <- filt.dat$d
PYO <- filt.dat$PYO

incidence_data <- list(
  N = length(d),                                # number of observations
  PYO = PYO,                                    # for offset
  y = d,                                        # response variable
  age = mean_age,                               # covariate
  agesquared = (mean_age)^2                     # covariate
)


library(rstan)
fit1 <- stan(
  file = "23_08_17_fourth_regression_POISSON.stan",     # Stan program
  data = incidence_data,                                # named list of data
  chains = 4,                                           # number of Markov chains
  warmup = 1000,                                        # number of warmup iterations per chain
  iter = 2000,                                          # total number of iterations per chain
  cores = 2,                                            # number of cores (using 2 just for the vignette)
  refresh = 250,                                        # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.8, max_treedepth = 10) # avoid divergent transitions
)

plot(fit1)
traceplot(fit1, pars = c("alpha[1]", "betas", "lp__"), inc_warmup = TRUE, nrow = 5)
print(fit1, pars = c("alpha[1]", "betas"))

sampler_params <- get_sampler_params(fit1, inc_warmup = TRUE)
summary(do.call(rbind, sampler_params), digits = 2)
lapply(sampler_params, summary, digits = 2)
pairs(fit1, pars = c("betas", "lp__", "alpha[1]"), las = 1)
