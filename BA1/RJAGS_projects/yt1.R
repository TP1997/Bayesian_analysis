fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/yt1.jag"
cat( 
  "model{
  theta ~ dbeta(22.28,27.23)
  # Prior 1
  x ~ dbin(theta, N)

  # Prior 2
  pi ~ dunif(0,1)
  # Predictive distribution
  x.new ~ dbin(theta, N)
  
  theta.new = x.new/N

 }",
file=fn)

data <- list(x=34, N=70)
init <- list( list(theta=0.1),list(theta=0.5),list(theta=0.9) )
model <- jags.model(file = fn, data = data, n.chains = 3,
                        inits = init, n.adapt = 2000)

res <- coda.samples(model, var="theta", n.iter = 10000, thin = 10)
class( res )   
summary(res)