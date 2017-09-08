rm(list = ls())
setwd("C:/Users/scat6434/Documents/LowTransmissionPfalciparum")

library(dplyr)

data1 <- read.csv("PfPvAllData01042015_AgeStand.csv")
data2 <- data1[data1$PR_Stand < 0.025 & data1$SPECIES == "Pf", ]
AGE <- (data2$INC_LAR + data2$INC_UAR)/2
data2 <- cbind(data2, AGE)
data3 <- left_join(data.frame(REGION = c("Africa+", "America", "CSE Asia")), data2, by="REGION") # order rows by region

summary.regions <- summary(data3$REGION)
regions <- as.data.frame(summary.regions)
ones <- as.data.frame(rep(1,times = length(data3$d)))


incidence_data <- list(
  N = length(data3$d),                                   # no. of observations
  J <- length(summary.regions),                          # number of groups, 3 regions
  K <- 3,                                                # number of regression variables: intercept, age and age^2
  id = rep(1:J, times = regions[,1]),                    # indexing by group
  r = data3$d,                                           # observed incidence
  age = data3$AGE,
  agesquared = (data3$AGE)^2,
  PYO = data3$PYO                                        # for offset
)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit <- stan(
  file = "31_08_17_seventh_regression.stan",              # Stan program
  data = incidence_data,                                  # named list of data
  chains = 4,                                             # number of Markov chains
  warmup = 1000,                                          # number of warmup iterations per chain
  iter = 11000,                                           # total number of iterations per chain
  cores = 4,                                              # number of cores
  refresh = 500,                                          # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.95, max_treedepth = 13), # avoid divergent transitions
  init = 0
)


sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
summary(do.call(rbind, sampler_params), digits = 2)
hist_treedepth <- function(fit){
  sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
  hist(sapply(sampler_params, function(x)c(x[,"treedepth__"]))[,1], breaks = 0:20, main = "", xlab = "Treedepth")
  abline(v = 10, col = 2, lty = 1)
}

lapply(sampler_params, summary, digits = 2)
print(fit, pars = c("lp__", "nu", "sigma1"))

traceplot(fit, pars = c("nu", "sigma1", "lp__"), nrow = 3)
plot(fit, pars = c("lp__", "nu", "sigma1", "mu"))
pairs(fit, pars = c("lp__", "nu", "sigma1"),las = 1)

hist_treedepth(fit)

divergent <- get_sampler_params(fit, inc_warmup=FALSE)[[1]][,'divergent__']
params_fit <- as.data.frame(extract(fit, permuted = FALSE))
params_fit$divergent <- divergent

names(params_fit) <- gsub("chain:1.", "", names(params_fit), fixed = TRUE)
names(params_fit) <- gsub("[", ".", names(params_fit), fixed = TRUE)
names(params_fit) <- gsub("]", "", names(params_fit), fixed = TRUE)

div_params_fit <- params_fit[params_fit$divergent == 1,]
nondiv_params_fit <- params_fit[params_fit$divergent == 0,]
params_fit$iter <- 1:10000
par(mar = c(4,4,0.5,0.5))
plot(params_fit$iter, params_fit$nu, col = "black", pch = 20, cex = 0.8,
     xlab = "Iteration", ylab = "nu", ylim=c(-3,2))

plot(nondiv_params_fit$mu.1,nondiv_params_fit$nu,
     col = "#8F2727", pch = 20,
     xlab = "mu.1", ylab = "nu",
     xlim = c(-4,2), ylim = c(-4,2)
)
points(div_params_fit$mu.1, div_params_fit$nu,
       col="green", pch=20, cex=0.8)

running_means_fit <- sapply(params_fit$iter, function(n) mean(params_fit$nu[1:n]))
par(mar = c(4, 4, 0.5, 0.5))
plot(params_fit$iter, running_means_fit, col="#8F2727", pch=20, cex=0.8, ylim=c(-0.7, 0.1),
     xlab="Iteration", ylab="MCMC mean of nu")


### works
