#18/08/17

rm(list = ls())

all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf" & all.dat$PCD == "No",]
mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2
INC <- filt.dat$INC               # incidence
PYO <- filt.dat$PYO

alpha <- 0.01
beta <- 0.01

olr <- lm(filt.dat$INC ~ mean_age + (mean_age)^2)
c1 <- qgamma(0.025, predict(olr) + alpha, PYO + beta)        # 95% confidence intervals about each point
c2 <- qgamma(0.975, predict(olr) + alpha, PYO + beta)


plot(predict(olr), x=mean_age, pch=20,
     xlab = "Mean age",
     ylab = "Predicted Incidence",
     main = "Incidence vs age using an olr",
     col = hsv(1, filt.dat$PR_Stand * 40, 1))

arrows(mean_age, predict(olr) - c1, mean_age, predict(olr) + c2, length = 0.01, angle = 90, code = 3)
