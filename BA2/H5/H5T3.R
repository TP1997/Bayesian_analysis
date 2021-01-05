setwd('/home/tuomas/R/Projects/DATA.STAT.730/H5')
fn = "H5T2a.jag"

cat("
  model{
  for(i in 1:I){
    for(j in 1:n[i]){
      y[i,j] ~ dnorm(mu[i], tauinv)
    }
    mu[i] ~ dnorm(phi, mu.sd)
  }
  # Priors
  tauinv ~ dgamma(102, 202)
  gaminv ~ dgamma(6, 20)
  tau = 1/tauinv
  gam = 1/gaminv
  
  # Hyperparameters
  phi = 60
  mu.sd = 1
  }",
  file=fn)

# Coagulation time in seconds
Data = c(62,60,63,59,NA,NA,NA,NA,         #diet1 n=4
         63,67,71,64,65,66,NA,NA,         #diet2 n=4
         68,66,71,67,68,68,NA,NA,         #diet3 n=6     
         52,62,60,61,63,64,63,59)         #diet4 n=8
# Convert to matrix form
Data = matrix(Data, nrow=4, byrow=T)
# Sample sizes for each treatment
n = c(4,6,6,8)

diet.data = list(y=Data, n=n, I=4)
diet.j = jags.model(file=fn, data=diet.data, n.chains=3, n.adapt=2000)
diet.par = c("tau","gam")

diet.res = coda.samples(model=diet.j, variable.names=diet.par, n.iter=10000, thin=10)
summary(diet.res)


# b)
fn = "H5T2b.jag"

cat("
  model{
  for(i in 1:I){
    for(j in 1:n[i]){
      y[i,j] ~ dnorm(mu[i], tauinv)
    }
    mu[i] ~ dnorm(phi, gaminv[i])
    gaminv[i] ~ dgamma(0.01, 0.01)
    gam[i] = 1/gaminv[i]
  }
  # Priors
  #phi ~ dnorm(60, 0.01)    # Variance=100
  tauinv ~ dgamma(0.01, 0.01)
  tau = 1/tauinv
  # Hyperparameters
  phi = 60
  }",
    file=fn)

diet.data = list(y=Data, n=n, I=4)
diet.j = jags.model(file=fn, data=diet.data, n.chains=3, n.adapt=2000)
diet.par = c("tau","gam[1]","gam[2]","gam[3]","gam[4]")

diet.res = coda.samples(model=diet.j, variable.names=diet.par, n.iter=10000, thin=10)
summary(diet.res)