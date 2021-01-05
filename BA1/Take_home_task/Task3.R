fn <- "/home/tuomas/R/Projects/Bayesian analysis/Take_home_task/task3.jag"
cat(
  "model
  {
    # Likelihood:
    for(i in 1:N)
    {
      x[i] ~ dnegbin(p[i],1)
      # Prior for p's
      #p[i] ~ dbeta(a, b)
    }
    for(i in 1:N){
    # Prior for p's
      p[i] ~ dbeta(a, b)
    }
    # Hyperpriors for a and b
    a ~ dgamma(1, 1)
    b ~ dgamma(1, 1)
    #b ~ dgamma(1, 0.5)
    
    theta <- a/(a+b)
  }
  ",
  file = fn )

# a)
task3.data <- list(x=c(4,0,1,7,3,2,8,0), N=8)
task3.init <- list(list(p=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), a=1, b=1111),
                   list(p=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), a=1, b=1111),
                   list(p=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), a=1, b=1111))
task3.model <- jags.model(file = fn, data = task3.data, inits = task3.init, n.chains = 3, n.adapt = 100)

task3.par <- c("p[1]")#,"p[2]","p[3]","p[4]","p[5]","p[6]","p[7]","p[8]", "theta","b"
task3.result <- coda.samples(model = task3.model, variable.names = task3.par, n.iter = 1000, thin = 1)
summary(task3.result)
plot(task3.result)
# a=11, b=1
# a=111, b=1
# a=1111, b=1
# a=1, b=11
# a=1, b=111
# a=1, b=1111
###################################
par(mfrow=c(3,1))
theta <- seq(0,10,by=0.1)

aPrior1 <- dgamma(theta, 1, 1)
bPrior1 <- dgamma(theta, 1, 1)
bPrior2 <- dgamma(theta, 1, 0.5)
plot(theta, aPrior1, type = "l", main = "alpha~Gamma(1,1)")
plot(theta, bPrior1, type = "l", main = "beta1~Gamma(1,1), beta2~Gamma(1,2)", col="green")
lines(theta, bPrior2, type = "l", col="red")
legend(6, 1, legend = c("Gamma(1,1)", "Gamma(1,2)"), col = c("green", "red"), lty = 1:1, cex = 1.5)

pdist1 <- dbeta(theta, 1, 1)
pdist2 <- dbeta(theta, 1, 2)
plot(theta, pdist1, type = "l", col="green", main = "beta testing")
lines(theta, pdist2, type="l", col="red")
legend(6, 1, legend = c("Beta(1,1)", "Beta(1,2)"), col = c("green", "red"), lty = 1:1, cex = 1.5)

par(mfrow=c(2,1))
d1 <- dgamma(theta, 1, 1)
plot(theta, d1, type = "l", main = "dd")
for (i in 2:2) {
  d2 <- dgamma(theta, 1, 1/i)
  lines(theta, d2, type="l", col=i)
}
legend(6, 1, legend = c("Gamma(1,1)", 
                        "Gamma(1,2)",
                        "Gamma(1,3)",
                        "Gamma(1,4)",
                        "Gamma(1,5)"), col = c("green", 2:5), lty = 1:1, cex = 1.5)

x=seq(0,2,by=0.1)
d1 <- dbeta(x, 1, 0.5)
plot(x, d1, type = "l", main = "ddd")
for(i in seq(0.7, 7, by=0.5)){
  d2 <- dbeta(x, 1, i)
  lines(x, d2, type="l", col=i)
}
