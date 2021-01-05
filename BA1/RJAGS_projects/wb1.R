fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/wb1.jag"

cat( 
  "model{
  # Likelihood
  x ~ dbin(pi, n)
  # Prior
  pi ~ dbeta(0.01, 0.01)    # same specification as in R
 }",
  file=fn)

wb1.data <- list(x=150, n=751)
wb1.init <- list(pi=0.15) #list(list(pi=0.25),list(pi=0.5),list(pi=0.75))
wb1.m <- jags.model(file=fn, data=wb1.data, inits=wb1.init, n.chains=3, n.adapt=2000)

par <- c("pi")
wb1.res <- coda.samples(wb1.m, var=par, n.iter = 10000, thin = 10)
summary(wb1.res)
plot(wb1.res)

#b
fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/wb1b.jag"

cat( 
  "model{
  # Likelihood
  x ~ dbin(pi, n)
  # Prior
  pi1 ~ dbeta(0.01, 0.01)
  pi2 ~ dbeta(1, 1)
  pi3 ~ dbeta(0.5, 0.5)
 }",
  file=fn)
wb1.data <- list(x=0, n=8)
wb1.init <- list(list())