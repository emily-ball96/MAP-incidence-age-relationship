#21/08/17

rm(list = ls())
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf", ]

mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2
d <- filt.dat$d
PYO <- filt.dat$PYO

glr <- glm(d ~ mean_age + (mean_age)^2, family = poisson(link=log), offset = log(PYO))

spline2 <- smooth.spline(x = mean_age, y = predict(glr) - log(PYO), spar=0.6, all.knots = FALSE, nknots = 20)
plot(mean_age, predict(glr) - log(PYO), pch=20,
     xlab = "mean age",
     ylab = "log(incidence)",
     main = "Age and incidence with second smoothing spline",
     col = hsv(1,filt.dat$PR_Stand*40,1))
lines(spline2)

plot(spline2, type = "l",
     xlab = "mean age",
     ylab = "log(incidence)",
     main = "Second spline smoothing curve")

plot(predict(spline2), pch = 20,
     xlab = "Mean age",
     ylab = "log(incidence)",
     main = "predicted incidence fitted via spline smoothing",
     col = hsv(1, filt.dat$PR_Stand *40, 1))