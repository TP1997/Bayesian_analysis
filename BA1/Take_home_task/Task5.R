fn <- "/home/tuomas/R/Projects/Bayesian analysis/Take_home_task/task5.jag"
cat("
  model{
  # Likelihood:
  for(i in 1:N){
    y1[i] ~ dnorm(mu, sigma1)
    y2[i] ~ dnorm(mu, sigma2)
  }
  # Priors:
  mu ~ dnorm(4.7, 1/v)
  sigma1 ~ dgamma(4, 1/5)
  sigma2 ~ dgamma(4, 1/5)
  tau1 <- 1/sigma1
  tau2 <- 1/sigma2
  phi <- tau2/tau1
  }",
    file = fn)

# a)
task5.data <- list(y1 = c(4.3, 4.3, 2.7, 3.6, 3.5, 4.5),
                   y2 = c(3.9, 4.0, 4.5, 2.9, 5.2, 4.8),
                   v = 0.2,
                   N = 6)
task5.init <- list(mu=4.7, sigma1=1, sigma2=1)
task5.model <- jags.model(file = fn, data = task5.data, inits = task5.init, n.chains = 3,
                          n.adapt = 2000)
  
task5.pars <- c("mu", "phi", "tau1", "tau2")
task5.postInfo <- coda.samples(model = task5.model, variable.names = task5.pars, 
                                 n.iter = 100000, thin = 10)

summary(task5.postInfo)
plot(task5.postInfo)
# Inference about wheter mu < 4.7
# Inference about posterior distribution of tau1/tau2

# b)
# Explore sensitivity of the analysis to v, on range 0.15 <= v <= inf.
vs <- c(0.15, 0.5, 1, 5, lseq(10,100000000,10))
v_effect <- data.frame(v=double(0), mu_mean=double(0))
task5.pars <- c("mu")
for(v in vs){
  task5.data$v <- v
  task5.init <- list(mu=4.7, taui1=1, taui2=1)
  task5.model <- jags.model(file = fn, data = task5.data, inits = task5.init, n.chains = 3,
                            n.adapt = 2000)
  task5.postInfo <- coda.samples(model = task5.model, variable.names = task5.pars, 
                                 n.iter = 100000, thin = 10)
  mu_mean <- summary(task5.postInfo)$statistics[1]
  #phi_mean <- summary(task5.postInfo)$statistics[2]
  v_effect[nrow(v_effect)+1,] <- c(v, mu_mean)
}
v_effect

v<-100000
x <- seq(-v*2,v*2,by=1)
f <- dnorm(x, 4.7, v)
plot(x,f,type = "l")

y<-c(y1,y2)