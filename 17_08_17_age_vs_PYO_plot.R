#17/08/17

rm(list = ls())
all.dat <- read.csv("PfPvAllData01042015_AgeStand.csv")

filt.dat <- all.dat[all.dat$PR_Stand < 0.025 & all.dat$SPECIES == "Pf",]
#& all.dat$PCD == "No",]

mean_age <- (filt.dat$INC_LAR + filt.dat$INC_UAR)/2

d <- filt.dat$d

data <- cbind(mean_age, d, filt.dat$PYO)

data <- as.data.frame(data)

#data <- data[data$mean_age != 42.5, ]


plot(data$mean_age, data[,3], pch=20,
	xlab = "Mean age in INC age bin",
	ylab = "PYO of each data point",
	main = "Age vs PYO")
