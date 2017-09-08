rm(list = ls())
setwd("C:/Users/scat6434/Documents/LowTransmissionPfalciparum")

data <- read.csv("PfPvAllData01042015_AgeStand.csv")
newdata <- data[data$PR_Stand < 0.025 & data$SPECIES == "Pf", ]
age <- (newdata$INC_LAR + newdata$INC_UAR)/2
d <- newdata$d
PYO <- newdata$PYO

incidence_data <- list(
  N = length(d),
  PYO = PYO,
  age = age,
  agesquared = (age)^2,
  y = d                          # 
)

library(rstan)

fit <- stan(
  file = "24_08_17_fifth_regression.stan",              # Stan program
  data = incidence_data,                                # named list of data
  chains = 4,                                           # number of Markov chains
  warmup = 1000,                                        # number of warmup iterations per chain
  iter = 2000,                                          # total number of iterations per chain
  cores = 4,                                            # number of cores (using 2 just for the vignette)
  refresh = 500,                                        # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.8, max_treedepth = 15) # avoid divergent transitions
)

plot(fit, pars = c("lp__", "betas"))
sampler_params <- get_sampler_params(fit, inc_warmup = TRUE)
summary(do.call(rbind, sampler_params), digits = 2)
lapply(sampler_params, summary, digits = 2)
traceplot(fit, pars = c("betas[1]", "lp__"), nrow = 2)
pairs(fit, pars = c("lp__", "betas"), las = 1)
print(fit, pars = c("lp__", "betas"), las = 1)
