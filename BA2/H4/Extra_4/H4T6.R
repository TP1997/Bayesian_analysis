
x = c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
y = c(3.39,3.3,2.81,3.03,3.44,3.07,3,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)

# b) Gibbs sampler:
gibbs = function(n_iter, x, y){
  n=length(x)
  b0 = 0
  b1 = 0
  tau = 1
  b0s = NULL
  b1s = NULL
  taus = NULL
  for(i in 1:n_iter){
    tau = rgamma(1, n/2, rate=0.5*sum((y-b0-b1*x)**2))
    b0 = rnorm(1, mean(y-b1*x), sqrt((tau*n)**-1))
    b1 = rnorm(1, sum(y*x-b0*x)/sum(x**2), sqrt((tau*sum(x**2))**-1))
    
    taus = append(taus,tau)
    b0s = append(b0s,b0)
    b1s = append(b1s,b1)
  }
  
  # Remove burn-in samples
  list(tau=taus[1001:length(taus)], b0=b0s[1001:length(b0s)], b1=b1s[1001:length(b1s)])
}

gibbs.samples = gibbs(10000, x, y)

# c) plot samples:
par(mfrow=c(3,1))
x_axis=seq(1001, 10000)
plot(x_axis, gibbs.samples$tau, type='l')
plot(x_axis, gibbs.samples$b0, type='l')
plot(x_axis, gibbs.samples$b1, type='l')

par(mfrow=c(1,3))
hist(gibbs.samples$tau)
hist(gibbs.samples$b0)
hist(gibbs.samples$b1)

par(mfrow=c(1,1))
plot(gibbs.samples$b0, gibbs.samples$b1)
