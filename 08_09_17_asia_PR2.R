## plot predicted incidence in Asia

params_fit <- as.data.frame(extract(fit, permuted = TRUE))
names(params_fit) <- gsub("[", ".", names(params_fit), fixed = TRUE)
names(params_fit) <- gsub("]", "", names(params_fit), fixed = TRUE)
alphas <- params_fit[,614:871]                                                 # quantiles for predicted values
all_quantiles <- apply(exp(alphas), 2, quantile, probs = c(0.05, 0.5, 0.95))   # create quantiles for all params

## specific to Asia

asia_data <- data3[data3$REGION == "CSE Asia", ]                               # to plot observed incidence
asia_quantiles <- all_quantiles[,173:258]
all_q <- cbind(seq(0,85,by=1), asia_quantiles[1,], asia_quantiles[2,], asia_quantiles[3,])

####
plot(x = all_q[,1], y = all_q[,3], type = "l", col = "#B97C7C",
     ylab = "Incidence", xlab = "Age", ylim = c(0,250),
     main = "Predicted incidence vs age in CSE Asia")
lines(x = all_q[,1], y = all_q[,4], lty = 3, col = "#8F2727")
lines(x = all_q[,1], y = all_q[,2], lty = 3, col = "#DCBCBC")
points(x=asia_data$AGE, y = asia_data$INC, pch = 20, col = "lightgreen")
legend(x = "right", legend = c("95%", "Median", "5%", "Observed"),
       col = c("#8F2727", "#B97C7C", "#DCBCBC", "lightgreen"),
       lty = c(3, 1, 3, NA),
       pch = c(NA, NA, NA, 20))

## error bars
alpha <- 0.1
beta <- 0.1
c1 <- qgamma(0.05, asia_data$INC + alpha, asia_data$PYO + beta)
c2 <- qgamma(0.95, asia_data$INC + alpha, asia_data$PYO + beta)
#arrows(asia_data$AGE, asia_data$INC - c1, asia_data$AGE, asia_data$INC + c2, length = 0.01, angle = 90, code = 3)


