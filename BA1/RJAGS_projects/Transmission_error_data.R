# Recall that   X | lambda  ~ Poisson (lambda) ,  lambda ~ Gamma(2, 1/2.1)
# Posterior dist. :  lambda| X ~ Gamma(7, 1/8.1)
fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/m1.jag"
cat( 
"model{
 # Likelihood
  for( i in 1:n)
   {
     x[i] ~ dpois(lambda)
   }
 # Prior
  lambda ~ dgamma(2,2.1)    # same specification as in R
 }",
file=fn)

tran.dat <- list(x=c(1, 0, 1, 2, 1, 0), n=6 )
tran.ini <- list( list( lambda=3 ), list( lambda=10 ), list( lambda=20 ) )
tran.m <- jags.model( file = fn, data = tran.dat,  n.chains = 3, 
                      inits = tran.ini, n.adapt = 2000 )

tran.par <- c("lambda")
tran.res <- coda.samples( tran.m, var = tran.par,  n.iter = 10000, thin = 10 )
summary(tran.res)
plot(tran.res)

# True quantiles:
qgamma(.025, 7, 8.1)
qgamma(.75, 7, 8.1)