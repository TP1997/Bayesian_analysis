# a)
f.inv = function(x){sqrt(1/(1-x))}

n=10000
us = runif(n, 0, 1)
samples = f.inv(us)

hist(samples, prob=T, nclass = 25)

# b)
theta.samples = NULL
n=10000
for(i in 1:n){
  # Draw theta & u
  theta = runif(1, 0, pi/2)
  u = runif(1,0,1)
  # Accept theta if...
  if(u <= sin(theta)){
    theta.samples = append(theta.samples, theta)
  }
}
hist(theta.samples, prob=T, nclass = 25)