fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/ItWrjags2.jag"
cat(
  "model{
  # Likelihood:
  for(i in 1:N)
  {
    #devs[i] ~ dt(mu, sigma, 4)
    devs[i] ~ dnorm(mu, sigma)
  }
  # Prior for mu:
  mu ~ dnorm(0, 1/1000000)
  # Prior for sigma:
  sigma ~ dgamma(0.0001,0.0001)
  tau <- 1/sigma
  
  # Prediction
  devs.new ~ dt(mu, sigma, 4)
  }",
  file=fn
)
data <- list(devs=c(28,26,33,24,34,-44,27,16,40,-2, 29,22,24,21,25,30,23,29,31,19,
                    24,20,36,32,36,28,25,21,28,29, 37,25,28,26,30,32,36,26,30,22,
                    36,23,27,27,28,27,31,27,26,33, 26,32,32,24,39,28,24,25,32,25,
                    29,27,28,29,16,23),
             N=66)
inits <- list(mu=10, sigma=1)
model <- jags.model(fn, data, inits = inits, n.chains = 3, n.adapt = 2000)

par <- c("mu", "tau", "devs.new")
res <- coda.samples(model, var=par, n.iter = 100000, thin = 10)
summary(res)
plot(res)