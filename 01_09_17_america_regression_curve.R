## plot America regression model

## idea - plot median + 95% CI for y against age

params_fit <- as.data.frame(extract(fit, permuted = TRUE))

names(params_fit) <- gsub("[", ".", names(params_fit), fixed = TRUE)
names(params_fit) <- gsub("]", "", names(params_fit), fixed = TRUE)

##ypred 295:431
all_q <- apply(params_fit,2,quantile,probs=c(0.05,0.5,0.95))
y_pred_q <- all_q[,295:431]



am_data <- data3[data3$REGION == "America", ]
q_am <- y_pred_q[,1:length(am_data$AGE)]                            # quantiles
AGE <- am_data$AGE
all_q <- cbind(AGE, am_data$d, q_am[1,], q_am[2,], q_am[3,])
AGE <- sort(AGE)
q_ordered <- semi_join(as.data.frame(all_q), data.frame(AGE), by = "AGE") # order rows by ascending age


plot(q_ordered[,4], x = q_ordered[,1], type = "l", col = "#B97C7C",
     ylab = "Incidence", xlab = "Age")                                          # median
lines(x=q_ordered[,1], y = q_ordered[,5], col = "#8F2727", lty = 3)             # upper
lines(x=q_ordered[,1], y = q_ordered[,3], col = "#DCBCBC", lty = 3)             # lower
points(x=q_ordered[,1], y = q_ordered[,2], pch = 20, col = "black")
legend(x = "right", legend = c("95%", "Median", "5%", "Observed"),
       col = c("#8F2727", "#B97C7C", "#DCBCBC", "black"),
       lty = c(3, 1, 3, NA),
       pch = c(NA, NA, NA, 20))

