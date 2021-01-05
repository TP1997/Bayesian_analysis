fn <- "/home/tuomas/R/Projects/Bayesian analysis/Take_home_task/task4.jag"
cat("
  model{
  # Likelihood:
  for(i in 1:N){
    mu[i] <- b0 + b1*weight[i] + b2*sixcyl[i] + b3*eightcyl[i] + e
    mpg[i] ~ dnorm(mu[i], prec)
  }
  # Priors:
  b0 ~ dnorm(0, 0.0001) # Variance = 1000
  b1 ~ dnorm(0, 0.0001)
  b2 ~ dnorm(0, 0.0001)
  b3 ~ dnorm(0, 0.0001)
  e ~ dnorm(0, prec)
  prec ~ dgamma(0.01, 0.01) #?
  sigma2 <- 1/sqrt(prec)
  
  # Predictive distribution:
  mu.new <- b0 + b1*weight.new + e
  mpg.new ~ dnorm(mu.new, prec)
    
  }",
  file = fn )

task4.data <- list(mpg=c(21.0,21.0,22.8,21.4,18.7,18.1, 14.3, 24.4, 22.8,19.2,17.8,16.4,17.3,15.2,
                         10.4,10.4,14.7,32.4,30.4,33.9, 21.5, 15.5,15.2,13.3,19.2,27.3, 26.0,30.4,
                         15.8,19.7,15.0, 21.4),
                   weight=c(2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.440, 
                            3.440, 4.070, 3.730, 3.780, 5.250, 5.424, 5.345, 2.200, 1.615, 1.835, 
                            2.465, 3.520, 3.435, 3.840, 3.845, 1.935, 2.140, 1.513, 3.170, 2.770,
                            3.570, 2.780),
                   sixcyl=c(1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                            0, 0, 0, 0, 0, 0, 1, 0, 0),
                   eightcyl=c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1,
                              1, 1, 1, 0, 0, 0, 1, 0, 1, 0),
                   weight.new = 3.5,
                   mpg.new = NA,
                   N=32
                   )
task4.init <- list(b0=20,b1=20,b2=20,b3=20,e=0.5,prec=1)
task4.model <- jags.model(fn, data = task4.data, inits = task4.init, n.chains = 3, n.adapt = 2000)

task4.pars <- c("b0", "b1", "b2", "b3", "e")
task4.postInfo <- coda.samples(model = task4.model, variable.names = task4.pars,
                               n.iter = 10000, thin = 10)
summary(task4.postInfo)
plot(task4.postInfo)

# b)
# 95% equal tailed probability intervals for:
# b0 : [27.004, 40.890]
# b1 : [-4.858, -1.584]
# b2 : [-7.170, -1.315]
# b3 : [-9.515, -2.502]
# e  : [-5.464, 5.925]

# d)
task4d.init <- list(b0=20,b1=20,b2=20,b3=20,e=0.5,prec=1)
task4d.model <- jags.model(fn, data = task4.data, inits = task4d.init, n.chains = 3, n.adapt = 2000)

task4d.pars <- c("mpg.new")
task4d.postInfo <- coda.samples(model = task4d.model, variable.names = task4d.pars,
                               n.iter = 10000, thin = 10)
summary(task4d.postInfo)
plot(task4d.postInfo)