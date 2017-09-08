rm(list = ls())
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf", ]
filt.dat <- left_join(data.frame(REGION = c("Africa+", "America", "CSE Asia")),filt.dat,by="REGION") # order rows by region

mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2
d <- filt.dat$d
PYO <- filt.dat$PYO
regions <- summary(filt.dat$REGION)
regions2 <- as.data.frame(regions)

incidence_data <- list(
  N = length(d),                              # need to check, maybe N=PYO,
  J <- length(regions),                         # number of groups, 3 regions
  K <- 2,                                       # number of regression variables: offset?, age and age^2
  id = rep(1:J, times = regions2[,1]),          #####
  y = d,                                      # response variable
  X = as.matrix(cbind(mean_age, (mean_age)^2))  # model matrix
)


library(rstan)
fit1 <- stan(
  file = "23_08_17_third_regression.stan",  # Stan program
  data = incidence_data,                    # named list of data
  chains = 1,                               # number of Markov chains
  warmup = 100,                             # number of warmup iterations per chain
  iter = 500,                               # total number of iterations per chain
  cores = 2,                                # number of cores (using 2 just for the vignette)
  refresh = 1000,                           # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.8, max_treedepth = 10)         # avoid divergent transitions
)

plot(fit1)
traceplot(fit1, pars = c("mu", "tau", "lp__"), inc_warmup = TRUE, nrow = 3)
print(fit1, pars = c("mu", "tau"))

sampler_params <- get_sampler_params(fit1, inc_warmup = TRUE)
summary(do.call(rbind, sampler_params), digits = 2)
lapply(sampler_params, summary, digits = 2)
pairs(fit1, pars = c("gamma", "tau", "lp__"), las = 1)