source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/nn.R")

# Likelihood: N( mu, 0.25)
eden.dat <- c( 5.36, 5.29, 5.58, 5.65, 5.57, 5.53, 5.62, 5.29, 5.44, 5.34, 5.79,
               5.10, 5.27,  5.39, 5.42, 5.47, 5.63, 5.34, 5.46, 5.30, 5.78, 5.68, 5.85)
summary(eden.dat)

# Conjugate prior for mu:  N(5,4)
# Obtain the Posterior distribution with the mean, variance, and the mode:
# Posterior: Normal(5.4835, 0.01084)
nn.sum(eden.dat, 5,4, 0.25)

# Plot of the prior, likelihood, and the posterior distribution
mu <- seq(0,10, by=0.02)
mu.prior <- dnorm(mu,  5, 2)    # 2= std.dev :  sqrt(4)
mu.post <- dnorm(mu, 5.486, sqrt(0.01084) )
mu.lkhd <-  dnorm(mu, 5.485, sqrt(0.25/ 23 ) ) # mean=x_bar, variance=tau/n

plot(mu,  mu.prior, ylim=c(0,4), type='l', col=4)
lines(mu, mu.post, lty=2, col=2)
lines(mu, mu.lkhd,  lty=3, col=3)
legend(0, 3, c('prior','posterior','lkhd'), col=c(4,2,3),  lty=1:3 )

# Triplot of the prior, likelihood, and the posterior distribution in one figure 
# using the function 'nn.trip':
nn.trip(eden.dat, 5,4, 0.25)

# 90% Posterior highest density Interval:
nn.hdi(eden.dat, 5,4, 0.25, .90)

# Posterior  probability:
1-pnorm(5.5, 5.4835, sqrt(0.01084) )

