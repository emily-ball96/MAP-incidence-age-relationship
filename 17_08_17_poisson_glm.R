#17/08/17
rm(list = ls())
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf", ]
# & all.dat$PCD == "No",]


mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2

d <- filt.dat$d
INC <- filt.dat$INC
PYO <- filt.dat$PYO

# take out mean_age of 42.5? or fit to a popn model

glr1 <- glm(INC ~ mean_age +(mean_age)^2, family=poisson(link=log))
#par(mfrow = c(2,2))
#plot(glr)

glr2 <- glm(d ~ mean_age + (mean_age)^2 + 0, family = poisson(link=log), offset = log(PYO))


colour <- c(0, 0.33, 0.66)

plot(predict(glr2)-log(PYO), x=mean_age, pch = 20,
	xlab = "mean age",
	col = hsv(colour[filt.dat$REGION], (filt.dat$PR_Stand)*40, 1),
	main = "age vs incidence")
legend(x= "topright", legend = c("Africa+", "Americas", "CSE Asia"),
       col = c("red", "lightgreen", "purple"),
       pch = rep(20,3))


plot(predict(glr2), x=mean_age, pch = 20,
	xlab = "mean age",
	col = hsv(colour[filt.dat$REGION], filt.dat$PR_Stand * 40, 1),
	main = "age vs incidence")
legend(x= "topright", legend = c("Africa+", "Americas", "CSE Asia"),
       col = c("red", "lightgreen", "purple"),
       pch = rep(20,3))



