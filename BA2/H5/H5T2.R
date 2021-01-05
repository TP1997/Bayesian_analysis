setwd('/home/tuomas/R/Projects/DATA.STAT.730/H5')
fn = "H5T2a.jag"

cat("
  model{
    for(i in 1:n){
      y[i] ~ dbern(p[i])
      logit(p[i]) = b1 + b2*t[i]
    }
    # Priors
    b1 ~ dnorm(0, 0.01) # Variance = 100
    b2 ~ dnorm(0, 0.01)
  }",
  file=fn)


ts = c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76, 78,79,81)
ys = c(1,1,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,0,0)
# a)
launch.data = list(t=ts, y=ys, n=23)
launch.init = list(list(b1=0, b2=0),list(b1=5, b2=5), list(b1=10, b2=10))
launch.j = jags.model(file=fn, data=launch.data, n.chains=3, n.adapt=2000)
launch.par = c("b1","b2")

launch.res = coda.samples(model=launch.j, variable.names=launch.par, n.iter=10000, thin=10)
summary(launch.res)

# b)
fn = "H5T2b.jag"
cat("
  model{
    for(i in 1:n){
      y[i] ~ dbern(p[i])
      logit(p[i]) = b1 + b2*(t[i]-mean(t[]))
    }
    # Priors
    b1 ~ dnorm(0, 0.01) # Variance = 100
    b2 ~ dnorm(0, 0.01)
    LD50 = mean(t[])-(b1/b2)
  }",
    file=fn)

launch.data = list(t=ts, y=ys, n=23)
launch.init = list(b1=1, b2=1)
launch.j = jags.model(file=fn, data=launch.data, inits=launch.init, n.chains=3, n.adapt=2000)
launch.par = c("LD50", "b1", "b2")

launch.res = coda.samples(model=launch.j, variable.names=launch.par, n.iter=10000, thin=10)
summary(launch.res)

# c)
fn = "H5T2c.jag"
cat("
  model{
    for(i in 1:n){
      y[i] ~ dbern(p[i])
      logit(p[i]) = b1 + b2*(t[i]-mean(t[]))
    }
    # Priors
    b1 ~ dnorm(0, 0.01) # Variance = 100
    b2 ~ dnorm(0, 0.01)
    LD50 = mean(t[])-(b1/b2)
    
    # Prediction
    for(i in 1:n){
      logit(p.new[i]) = b1 + b2*(28+2*i - mean(t[]))
    }
    
  }",
    file=fn)

launch.data = list(t=ts, y=ys, n=23)
launch.init = list(b1=1, b2=1)
launch.j = jags.model(file=fn, data=launch.data, inits=launch.init, n.chains=3, n.adapt=2000)
launch.par = c("p.new")

launch.res = coda.samples(model=launch.j, variable.names=launch.par, n.iter=10000, thin=10)
summary(launch.res)