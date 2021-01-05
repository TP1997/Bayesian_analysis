source("/home/tuomas/R/Projects/Bayesian analysis/Take_home_task/pg.R")

# a)
dataA <- c(12,9,12,14,13,13,15,8,15,6)
theta <- seq(7,17,length.out = 200)
thetaAPriorAlfa <- 120
thetaAPriorBeta <- 1/10
thetaAPriorDist <- dgamma(theta, thetaAPriorAlfa, scale=thetaAPriorBeta)
thetaAPostInfo <- pg.sum(dataA, thetaAPriorAlfa, 1/thetaAPriorBeta)
thetaAPostDist <- dgamma(theta, thetaAPostInfo$alfa1, rate=thetaAPostInfo$beta1)
thetaAPostMean <- thetaAPostInfo$Mean
thetaAPostVar <- thetaAPostInfo$Var
thetaAPostHDI <- pg.hdi(dataA, thetaAPriorAlfa, thetaAPriorBeta, 0.9)

plot(theta, thetaAPostDist, type="l", col="red", ylab="density")
lines(theta, thetaAPriorDist, type="l", col="green")
legend(7, 0.5, legend = c("prior", "posterior"), col = c("green", "red"), lty = 1:1, cex = 0.8)

cat("Theta A:",
    "\nPosterior mean:", thetaAPostMean,
    "\nPosterior variance:", thetaAPostVar,
    "\n90% HDI: [", thetaAPostHDI$Ala,", ", thetaAPostHDI$Yla,"]\n")
# b)
dataB <- c(11,11,10,9,9,8,7,10,6,8,8,9,7)
theta <- seq(0,20,length.out = 200)
thetaBPriorAlfa <- 12
thetaBPriorBeta <- 1
thetaBPriorDist <- dgamma(theta, thetaBPriorAlfa, thetaBPriorBeta)
thetaBPostInfo <-pg.sum(dataB, thetaBPriorAlfa, thetaBPriorBeta)
thetaBPostDist <- dgamma(theta, thetaBPostInfo$alfa1, thetaBPostInfo$beta1) #1/thetaBPostInfo$beta1
thetaBPostMean <- thetaBPostInfo$Mean
thetaBPostVar <- thetaBPostInfo$Var
thetaBPostHDI <- pg.hdi(dataA, thetaBPriorAlfa, thetaBPriorBeta, 0.9)

#plot(theta, thetaBPostDist, type="l", col="red", ylab="density")
#lines(theta, thetaBPriorDist, type="l", col="green")
#legend(0, 0.5, legend = c("prior", "posterior"), col = c("green", "red"), lty = 1:1, cex = 0.8)

cat("Theta B:",
    "\nPosterior mean:", thetaBPostMean,
    "\nPosterior variance:", thetaBPostVar,
    "\n90% HDI: [", thetaBPostHDI$Ala,", ", thetaBPostHDI$Yla,"]\n")

# c)
par(mfrow=c(2,1))
# Plotting thetaA
xaxis <- seq(7, 17, by = 1)
plot(theta, thetaAPostDist, type = "l", col = "red", ylab = "density", xlab = "thetaA",
     xlim = c(7, 17), xaxt = 'n', main = "a.)")
axis(1, at = xaxis, las = 1)
lines(theta, thetaAPriorDist, type = "l", col = "green")
legend(7, 0.48, legend = c("prior", "posterior"), col = c("green", "red"), lty = 1:1, cex = 0.8)

# Plotting thetaB
xaxis <- seq(3, 20, by = 2)
plot(theta, thetaBPostDist, type = "l", col = "red", ylab = "density", xlab = "thetaB",
     xlim = c(3, 20), xaxt = 'n', main = "b.)")
axis(1, at = xaxis, las=1)
lines(theta, thetaBPriorDist, type = "l", col = "green")
legend(3, 0.48, legend = c("prior", "posterior"), col = c("green", "red"), lty = 1:1, cex = 0.8)

# d)
dataB <- c(11,11,10,9,9,8,7,10,6,8,8,9,7)
#theta <- seq(0,20,length.out = 200)
postMeans <- data.frame(Prior_alpha=integer(0), Prior_beta=integer(0), Post_alpha=integer(0), Post_beta=double(0), Post_mean=double(0))
for(n in 1:50){
    thetaBPriorAlfa <- 12*n
    thetaBPriorBeta <- n
    thetaBPostInfo <- pg.sum(dataB, thetaBPriorAlfa, 1/thetaBPriorBeta)
    postMeans[nrow(postMeans)+1,] <- c(thetaBPriorAlfa, thetaBPriorBeta,
                                       thetaBPostInfo$alfa1, thetaBPostInfo$beta1, thetaBPostInfo$Mean)
}
postMeans
postMeans[4,]

par(mfrow=c(2,1))
# Plotting updated thetaB
theta <- seq(0,20,length.out = 200)
thetaBPostDist <- dgamma(theta, postMeans[4,]$Post_alpha, rate=postMeans[4,]$Post_beta)
thetaBPriorDist <- dgamma(theta, postMeans[4,]$Prior_alpha, scale=postMeans[4,]$Prior_beta)

xaxis <- seq(0, 20, by = 2)
plot(theta, thetaBPostDist, type = "l", col = "red", ylab = "density", xlab = "thetaB",
     main = "b.)")
axis(1, at = xaxis, las=1)
lines(theta, thetaBPriorDist, type = "l", col = "green")
legend(3, 0.48, legend = c("prior", "posterior"), col = c("green", "red"), lty = 1:1, cex = 0.8)



theta <- seq(0,20,length.out = 200)
thetaBPriorAlfa <- 12*4
thetaBPriorBeta <- 1*4
pd <- dgamma(theta, thetaBPriorAlfa, rate=thetaBPriorBeta)
plot(pd, type="l")