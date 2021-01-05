fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/caven.jag"
cat( 
"model
{
 # likelihood
 for (i in 1:N){
  x[i]~ dnorm(mu, sigma2)
  }
 # Prior
  mu~dnorm(5,0.25)
  sigma2~dgamma(0.025,0.05)
 # Variance
  tau <- 1/sigma2
}",
 file=fn )

caven.data =list( x=c(5.36, 5.29, 5.58, 5.65, 5.57, 5.53, 5.62, 5.29, 5.44, 5.34, 
                      5.79, 5.10, 5.27, 5.39, 5.42, 5.47, 5.63, 5.34, 5.46, 5.30, 
                      5.78, 5.68, 5.85), N=23)
caven.inits= list(mu=2, sigma2=1)
caven.inits=list(  list(mu=2, sigma2=1), list(mu=1, sigma2=0.1), list(mu=2, sigma2=0.01)  )

caven.m <- jags.model( file = fn, data = caven.data, n.chains = 3, 
                       inits = caven.inits, n.adapt = 2000 )

caven.par <- c("mu", "tau")
res <- coda.samples( caven.m, var = caven.par, n.iter = 10000, thin = 10)
#class( res )   # [1] "mcmc.list"
summary(res)







