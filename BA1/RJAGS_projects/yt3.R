fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/yt3.jag"

cat( 
  "model{
  # Likelihood
  for( i in 1:n)
   {
     mu[i] <- alpha + b.gen*gender[i]+b.age*age[i]
     bp[i] ~ dnorm(mu[i], psi)
   }
   
  # Priors
  alpha ~ dnorm(0.0, 1.0E-4) #variance=10000
  b.gen ~ dnorm(0.0, 1.0E-4)
  b.age ~ dnorm(0.0, 1.0E-4)
  psi ~dgamma(1.0E-3, 1.0E-3)
  sigma <- 1.0/sqrt(psi)
  
  #Prediction
  mu.new <- alpha + b.gen*gen.new + b.age*age.new
  bp.new ~ dnorm(mu.new, psi)
 }",
  file=fn)

yt3.data <- list(gender=c(0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0,
                          0, 1, 1, 1, 1),
                 age=c(59, 52, 37, 40, 67, 43, 61, 34, 51, 58, 54, 31, 49, 45, 66, 48, 41, 47, 53,
                       62, 60, 33, 44, 70, 56, 69, 35, 36, 68, 38),
                 bp=c(143, 132, 88, 98, 177, 102, 154, 83, 131, 150, 131, 69, 111, 114, 170, 117,
                      96, 116, 131, 158, 156, 75, 111, 184, 141, 182, 74, 87, 183, 89),
                 n=30, 
                 gen.new=1, 
                 age.new=50)
yt3.init <- list(alpha=50, b.gen=1, b.age=4, psi=1)
yt3.model <- jags.model(fn, data=yt3.data, inits=yt3.init, n.chains=3, n.adapt=2000)

# Get information from:
par <- c("alpha", "b.age", "b.gen", "mu[1]", "sigma", "bp.new")
yt3.res <- coda.samples(yt3.model, var=par, n.iter=10000, thin=10)
summary(yt3.res)
plot(yt3.res)