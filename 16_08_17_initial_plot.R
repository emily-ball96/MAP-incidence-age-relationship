
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf" & all.dat$PCD == "No",]
mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2

n <- length(filt.dat$d)
total <- sum(filt.dat$d)
alpha <- 0.1
beta <- 0.1
lower <- total/n - qgamma(0.025,total + alpha, n + beta)           # incorrect error bars here
upper <- - total/n + qgamma(0.975,total + alpha, n + beta)


plot(mean_age, filt.dat$d, pch=20)
#arrows(mean_age, INC - lower, mean_age, INC + upper, length = 0.05, angle = 90, code = 3)


#olr <- lm(INC~mean_age)
#par(mfrow = c(2,2))
#plot(olr)

#glr <- glm(INC~mean_age, family=poisson)

predict <- predict(olr)
plot(predict)

