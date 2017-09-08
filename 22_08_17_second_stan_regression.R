rm(list = ls())
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf", ]

mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2
d <- filt.dat$d
PYO <- filt.dat$PYO


incidence_data <- list(
  N = length(d),     # need to check, maybe N=PYO,
  y = d,             # need to check, vector of estimates of ?incidence?
  x = mean_age,        # standard error of estimate of incidence
  z = PYO
)


library(rstan)
fit1 <- stan(
  file = "22_08_17_second_regression.stan",  # Stan program
  data = incidence_data,                    # named list of data
  chains = 4,                               # number of Markov chains
  warmup = 100,                            # number of warmup iterations per chain
  iter = 500,                              # total number of iterations per chain
  cores = 4,                                # number of cores (using 2 just for the vignette)
  refresh = 1000,                           # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.8, max_treedepth = 15)         # avoid divergent transitions
)

plot(fit1)
traceplot(fit1, pars = c("mu", "tau", "lp__"), inc_warmup = TRUE, nrow = 3)
print(fit1, pars = c("mu", "tau"))

sampler_params <- get_sampler_params(fit1, inc_warmup = TRUE)
summary(do.call(rbind, sampler_params), digits = 2)
lapply(sampler_params, summary, digits = 2)
pairs(fit1, pars = c("mu", "tau", "lp__"), las = 1)