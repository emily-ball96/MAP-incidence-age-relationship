#17/08/17
rm(list = ls())
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf", ]
# & all.dat$PCD == "No",]


mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2
d <- filt.dat$d
PYO <- filt.dat$PYO
INC <- filt.dat$INC

glr1 <- glm(INC ~ mean_age + (mean_age)^2, family=poisson(link=log))
glr2 <- glm(d ~ mean_age + (mean_age)^2 + 0, family = poisson(link=log), offset = log(PYO))

## error bars
alpha <- 0.01
beta <- 0.01
c1 <- qgamma(0.025, filt.dat$d + alpha, PYO + beta)
c2 <- qgamma(0.975, filt.dat$d + alpha, PYO + beta)
##

colour <- c(0, 0.33, 0.66)

plot(predict(glr2)-log(PYO), x=mean_age, pch = 20,
	xlab = "mean age",
	col = hsv(1, (filt.dat$PR_Stand)*40, 1),
	main = "age vs incidence")

arrows(mean_age, log(exp(predict(glr2)-log(PYO)) - c1), mean_age, log(exp(predict(glr2) - log(PYO)) + c2), length = 0.01, angle = 90, code = 3)

plot(predict(glr2), x=mean_age, pch = 20,
	xlab = "mean age",
	col = hsv(colour[filt.dat$REGION], filt.dat$PR_Stand * 40, 1),
	main = "age vs incidence")

arrows(mean_age, log(exp(predict(glr2))-c1), mean_age, log(exp(predict(glr2))+c2), length = 0.01, angle = 90, code = 3)


## plot last plot to see America, until age structuring
