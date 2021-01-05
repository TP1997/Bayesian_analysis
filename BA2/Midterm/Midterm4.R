setwd('/home/tuomas/R/Projects/DATA.STAT.730/Midterm')

# Initalize the chain
theta.t = 1
theta.samples = NULL
N = 10000 # num of iterations
for(i in 1:N){
  # Draw sample from proporsal distribution
  theta.star = rnorm(1,0,1)
  # Calculate acceptance rate
  alpha = (exp(-theta.star)*dnorm(theta.t, 0, 1)) / (exp(-theta.t)*dnorm(theta.star, 0, 1))
  # Accept if...
  if(log(runif(1)) < log(alpha)){
    theta.t = theta.star
  }
  
  theta.samples = append(theta.samples, theta.t)
}

# Plot
par(mfrow=c(1,1))
hist(theta.samples, prob=T, nclass=25)