fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/wb2.jag"

cat( 
  "model{
  # Likelihood
  for(i in 1:n)
  {
    x[i] ~ dt(mu, tau, 4)
  }
  # Prior
  mu ~ dnorm(0, 1.0E-6)
  sigma ~ dgamma(1.0E-3, 1.0E-3)
  tau <- 1/sigma
 }",
  file=fn)

wb2.data <- list(x=c(28,26,33,24,34,-44,27,16,40,-2, 29,22,24,21,25,30,23,29,31,19,
                     24,20,36,32,36,28,25,21,28,29, 37,25,28,26,30,32,36,26,30,22,
                     36,23,27,27,28,27,31,27,26,33, 26,32,32,24,39,28,24,25,32,25,
                     29,27,28,29,16,23),
                 n=66)
wb2.init <- list(mu=10, sigma=1)
wb2.m <- jags.model(file=fn, data=wb2.data, inits=wb2.init, n.chains=3, n.adapt=2000)

par <- c("mu", "sigma")
wb2.res <- coda.samples(wb2.m, var=par, n.iter = 10000, thin = 10)
summary(wb2.res)
plot(wb2.res)

## b
fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/wb22.jag"

cat( 
  "model{
  # Likelihood
  for(i in 1:n)
  {
    x[i] ~ dnorm(mu, sigma)
  }
  # Prior
  mu ~ dnorm(0, 1.0E-6)
  sigma ~ dgamma(1.0E-3, 1.0E-3)
  tau <- 1/sigma
 }",
  file=fn)

wb22.m <- jags.model(file=fn, data=wb2.data, inits=wb2.init, n.chains=3, n.adapt=2000)
par <- c("mu", "sigma")
wb22.res <- coda.samples(wb22.m, var=par, n.iter = 10000, thin = 10)
summary(wb22.res)
plot(wb22.res)