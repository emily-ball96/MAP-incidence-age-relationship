### base level cut of incidence over 50

rm(list = ls())
setwd("C:/Users/scat6434/Documents/LowTransmissionPfalciparum")

library(dplyr)

data1 <- read.csv("PfPvAllData01042015_AgeStand.csv")
data2 <- data1[data1$PR_Stand < 0.025 & data1$SPECIES == "Pf" & data1$INC < 50, ]
AGE <- (data2$INC_LAR + data2$INC_UAR)/2
data2 <- cbind(data2, AGE)
data3 <- left_join(data.frame(REGION = c("Africa+", "America", "CSE Asia")), data2, by="REGION") # order rows by region

summary.regions <- summary(data3$REGION)
regions <- as.data.frame(summary.regions)
ones <- as.data.frame(rep(1,times = length(data3$d)))


incidence_data <- list(
  N = length(data3$d),                                   # no. of observations
  J <- length(summary.regions),                          # number of groups, 3 regions
  K <- 3,                                                # number of regression coeffs: intercept, age and age^2
  id = rep(1:J, times = regions[,1]),                    # indexing by group
  r = data3$d,                                           # observed incidence
  age = data3$AGE,
  agesquared = (data3$AGE)^2,
  PYO = data3$PYO,                                       # for offset
  N_new = 86,                                            # no. of ages to predict
  a_new = 0:85,                                          # vector of ages to predict
  asq_new = (0:85)^2                                     # squared predictive ages
)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

init <- function(){list("kappa" = 1, "sigma1" = 1, "sigma2" = rep(1,3), p = 0.2, beta1 = rep(0, 3), beta2 = rep(0,3), beta3 = rep(0,3), "eta" = log(data3$PYO))}

fit <- stan(
  file = "07_09_17_inc_capped.stan",                      # Stan program
  data = incidence_data,                                  # named list of data
  chains = 4,                                             # number of Markov chains
  warmup = 1000,                                          # number of warmup iterations per chain
  iter = 11000,                                           # total number of iterations per chain
  cores = 4,                                              # number of cores
  refresh = 500,                                          # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.8, max_treedepth = 15),  # avoid divergent transitions
  init = init
)


sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)

summary(do.call(rbind, sampler_params), digits = 2)
##
hist_treedepth <- function(fit){
  sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
  hist(sapply(sampler_params, function(x)c(x[,"treedepth__"]))[,1], breaks = 0:20, main = "", xlab = "Treedepth")
  abline(v = 10, col = 2, lty = 1)
}
hist_treedepth(fit)                                       ## histogram of treedepth
##
lapply(sampler_params, summary, digits = 2)

print(fit, pars = c("lp__", "nu", "eps[1]"))

traceplot(fit, pars = c("eps[1]", "nu", "lp__"), nrow = 3)

plot(fit, pars = c("mu[1]", "nu", "eps[1]"))

pairs(fit, pars = c("nu", "eps[1]", "lp__"),las = 1)

divergent <- get_sampler_params(fit, inc_warmup=FALSE)[[1]][,'divergent__']    ## get divergent transitions
params_fit <- as.data.frame(extract(fit, permuted = TRUE))
params_fit$divergent <- divergent

##
names(params_fit) <- gsub("[", ".", names(params_fit), fixed = TRUE)
names(params_fit) <- gsub("]", "", names(params_fit), fixed = TRUE)
params_fit$iter <- 1:10000
par(mar = c(4,4,0.5,0.5))
plot(params_fit$iter, params_fit$nu, col = "black", pch = 20, cex = 0.8,       ## plot gamma sampling
     xlab = "Iteration", ylab = "nu", ylim=c(5,8))
####
div_params_fit <- params_fit[params_fit$divergent == 1,]                       ## get divergent transitions
nondiv_params_fit <- params_fit[params_fit$divergent == 0,]                    ## get non divergent transitions
plot(nondiv_params_fit$mu.1,nondiv_params_fit$nu,
     col = "#8F2727", pch = 20,
     xlab = "mu.1", ylab = "nu"
)
points(div_params_fit$mu.1, div_params_fit$nu,
       col="green", pch=20, cex=0.8)
####
running_means_fit <- sapply(params_fit$iter, function(n) mean(params_fit$nu[1:n]))
running_var_fit <- sapply(params_fit$iter, function(n) var(params_fit$nu[1:n]))
par(mar = c(4, 4, 0.5, 0.5))
plot(params_fit$iter, running_means_fit, col="#8F2727", pch=20, cex=0.8,
     xlab="Iteration", ylab="MCMC mean of nu")                                 ## to see convergence of nu
points(params_fit$iter, running_var_fit, col= "#DCBCBC", pch = 20, cex=0.8)    ## variance of nu by iteration
legend(x = "bottomright", legend = c("mean of nu", "variance of nu"),
       col = c("#8F2727", "#DCBCBC"),
       pch = c(20, 20))

Nu <- extract(fit, 'nu')
Nu <- unlist(Nu, use.names = FALSE)                                    ## to see the post. density of nu
plot(density(Nu), col = grey(0,0.8),
     xlab = expression(nu),
     main = "Parameter Distribution")
