gibbs = function(niter, mu=0, tau=1){
  y.vec = rnorm(15, mean=3, sd=5)
  cat(mean(y.vec))
  n = length(y.vec)
  mu.samples = NULL
  tau.samples = NULL
  for(i in 1:niter){
    mu = rnorm(1, mean(y.vec), sd=sqrt(tau/n))  # Full conditional for mu
    tau = (sum((y.vec - mu)**2)) / rchisq(1, n)     # Full conditional for tau
    mu.samples = append(mu.samples, mu)
    tau.samples = append(tau.samples, tau)
  }
  
  return(list(mus=mu.samples, taus=tau.samples))
}

# Get samples from joint posterior
gibbs.samples = gibbs(10000)
mu.gibbs = gibbs.samples$mus
tau.gibbs = gibbs.samples$taus

# Plot the mariginal samples
par(mfrow=c(2,1))
hist(mu.gibbs, main="Gibbs sampling distribution for mu", nclass=50)
hist(tau.gibbs, main="Gibbs sampling distribution for tau", nclass=50)








