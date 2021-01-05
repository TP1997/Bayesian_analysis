x=20
lambda=30
a=2
b=2

theta.samples = NULL
W.samples = NULL
n.samples = NULL
# Initialize the chain
theta = 1
W = 1
iter=2000
for(i in 1:iter){
  # Draw theta from full-cond.
  theta = rbeta(1, x+a, (W+x)-x+b)
  # Draw W from full-cond.
  W = rpois(1, (1-theta)*lambda)
  # Append samples to list
  theta.samples = append(theta.samples, theta)
  n.samples = append(n.samples, W+x)
}

# Delete burn-in period
theta.samples = theta.samples[500:length(theta.samples)]
n.samples = theta.samples[500:length(n.samples)]

# Plot the distributions
par(mfrow=c(1,2))
hist(theta.samples, prob=T, nclass=25)
hist(n.samples, prob=T, nclass=25)

