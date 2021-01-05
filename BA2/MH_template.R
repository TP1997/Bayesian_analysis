setwd('/home/tuomas/R/Projects/DATA.STAT.730/Midterm')

# Initalize the chain
theta.t = t

theta.samples = NULL
N = 10000 # num of iterations
for(i in 1:N){
  # Draw sample from proporsal distribution
  theta.star = 
  # Calculate acceptance rate
  alpha = 
  # Accept if...
  if(log(runif(1)) < log(alpha)){
    alpha.t = alpha.star
  }
  
  theta.samples = append(theta.samples, theta.t)
}