setwd('/home/tuomas/R/Projects/DATA.STAT.730/H4')
hospital_data <- read.table("hospital.txt", header=TRUE)
ys = hospital_data$hospital
x1s = hospital_data$loginc
x2s = hospital_data$distance
x3s = hospital_data$dropout
x4s = hospital_data$college
x5s = hospital_data$group

fn = "H4T5.jag"

cat("
  model{
    for(i in 1:N){
      y[i] ~ dbern(p[i])
      logit(p[i]) = b0 + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + b5*x5[i]
    }
    # Priors
    b0 ~ dnorm(0, 0.001) # Variance = 1000
    b1 ~ dnorm(0, 0.001)
    b2 ~ dnorm(0, 0.001)
    b3 ~ dnorm(0, 0.001)
    b4 ~ dnorm(0, 0.001)
    b5 ~ dnorm(0, 0.001)
  }",
    file=fn)

hospital.data = list(y=ys, x1=x1s, x2=x2s, x3=x3s, x4=x4s, x5=x5s, N=140)
hospital.init = list(list(b0=1, b1=1, b2=1, b3=1, b4=1, b5=1),
                     list(b0=2, b1=5, b2=4, b3=1, b4=2, b5=3), 
                     list(b0=1, b1=1, b2=2, b3=1, b4=2, b5=1))
hospital.j = jags.model(file=fn, data=hospital.data, n.chains=3, n.adapt=2000)
hospital.par = c("b0","b1","b2","b3","b4","b5")

hospital.res = coda.samples(model=hospital.j, variable.names=hospital.par, n.iter=10000, thin=10)
summary(hospital.res)

plot(hospital.res)
##
x=seq(-3,3,0.1)
dist=dnorm(x, )
