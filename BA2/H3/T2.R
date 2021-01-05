
# Sample count
n = 10000
# Initialize theta
theta.t = 0
theta.samples = NULL
accepts = 0
for(t in 1:n){
  # Draw sample from proposal distribution
  theta.star = runif(1)
  # Acceptance probability
  alpha = dbeta(theta.star, 2.7, 6.3) / dbeta(theta.t, 2.7, 6.3)
  # Accept drawn sample if 
  if(log(runif(1)) < log(alpha)){
    theta.t = theta.star
    accepts = accepts + 1 
  }
  theta.samples = append(theta.samples, theta.t)
}

cat(sprintf("MH algorithm acceptance rate = %i / %i = %f\n", accepts, n, accepts/n))
par(mfrow=c(2,1))
hist(theta.samples, main="MH-sampling distribution from Beta(2.7, 6.3)", nclass = 25)
theta.samples2 = rbeta(n, 2.7, 6.3)
hist(theta.samples2, main="True sampling distribution from Beta(2.7, 6.3)", nclass = 25)