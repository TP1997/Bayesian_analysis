# a)
norm.den.kernel = function(x){exp(-0.5*x**2)}

# b)
mh = function(niter, a=1){
  theta.t = 0
  theta.samples = NULL
  for(i in 1:niter){
    # Draw a sample from proposal distribution
    theta.star = runif(1, -a, a)
    # Acceptance probability
    log.alpha = log(norm.den.kernel(theta.star)) - log(norm.den.kernel(theta.t))
    # Accept drawn sample if
    if(log(runif(1)) < log.alpha){
      theta.t = theta.star
    }
    theta.samples = append(theta.samples, theta.t)
  }
  
  return(theta.samples)
}

# c)
n=10000
a=5
theta.samples = mh(n,a)
norm.samples = rnorm(n)
par(mfrow=c(2,1))
hist(theta.samples, 
     main=sprintf("MH sampling distribution from N(0, 1) when a=%i", a), 
     nclass = 25)
hist(norm.samples, main="True sampling distribution from N(0, 1)", nclass = 25)



