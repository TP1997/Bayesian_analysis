fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/yt2.jag"
airline.dat = read.table(file = "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/airline.txt",
                         header=T, sep="") 
cat( 
  "model{
  # Likelihood
  for( i in 1:N)
   {
     fatal[i] ~ dpois(lambda)
   }
  # Prior
  lambda ~ dgamma(0.1, 0.1)    # same specification as in R
 }",
  file=fn)

yt2.data <- list(fatal=c(airline.dat$fatal, NA), N=27)
yt2.ini <- list(list(lambda=15),list(lambda=23),list(lambda=30))
yt2.m <- jags.model(file = fn, data = yt2.data, n.chains = 3,
                    inits = yt2.ini, n.adapt = 2000)

air.res <- coda.samples(yt2.m, var="lambda", n.iter = 10000, thin = 10)
summary(air.res)
plot(air.res)

a1.par <- c("lambda","fatal[27]")  
yt2.res <- coda.samples(yt2.m, var=a1.par, n.iter = 10000, thin = 10)
summary(yt2.res)
plot(yt2.res)


