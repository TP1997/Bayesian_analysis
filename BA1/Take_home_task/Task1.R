source("/home/tuomas/R/Projects/Bayesian analysis/Take_home_task/bb.R")
# a) lkhd: Binom(20, theta), prior: Unif(0,1), post: Beta(13,9)
theta = seq(0,1,length.out = 100)
thetaPostDist <- dbeta(theta, 13, 9)
thetaPriorDist <- dunif(theta, 0, 1)

plot(theta, thetaPostDist, type = "l", col="red", ylab="density")
lines(theta, thetaPriorDist, type = "l", col="green")
legend(0, 3.8, legend = c("prior", "posterior"), col = c("green", "red"), lty = 1:1, cex = 0.8)

# b) lkhd: Binom(20, theta), prior: Beta(10, 2), post: Beta(22, 10)
data <- c(rep(1,12), rep(0,8))
thetaPostInfo <- bb.sum(data, 1, 10, 2)
thetaPostDist <- dbeta(theta, thetaPostInfo$alfa1, thetaPostInfo$beta1)
thetaPriorDist <- dbeta(theta, 10, 2)

plot(theta, thetaPostDist, type = "l", col="red", ylab="density")
lines(theta, thetaPriorDist, type = "l", col="green")
legend(0, 4.8, legend = c("prior", "posterior"), col = c("green", "red"), lty = 1:1, cex = 0.8)

seq(0,1,length.out = 100)
