#17/08/17

rm(list = ls())
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf", ]

mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2
d <- filt.dat$d
PYO <- filt.dat$PYO

#glr <- glm(INC ~ offset(log(PYO)) + mean_age + (mean_age)^2, family = poisson(link=log))
#par(mfrow = c(2,2))
#plot(glr)

initial.spline <- smooth.spline(x = mean_age, y = d, spar=0.7)
##par(mfrow = c(1,2))
plot(mean_age, d, pch = 20,
	xlab = "mean age",
	ylab = "incidence",
	main = "Age and incidence with initial smoothing spline")
lines(initial.spline)

plot(initial.spline, type = "l",
	xlab = "mean age",
	ylab = "incidence",
	main = "Initial spline smoothing curve")