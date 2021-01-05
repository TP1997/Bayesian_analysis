# a)
fa = function(x){10*exp(-2*abs(x-5))}
n=10000
x = runif(n, 0, 10)

mc.estimate1 = mean(fa(x))

# b)
fb = function(x){exp(-2*abs(x-5)) / dnorm(x,5,1)}
n=10000
x = rnorm(n, 5, 1)

x.fb = fb(x)
x.fb = x.fb[x.fb>0 & x.fb<10]

mc.estimate2 = mean(x.fb)

# c)
cat("MC-etimate using uniform dist:", mc.estimate1,
    "\nMC-etimate using normal dist:", mc.estimate2)

# The true value of integral is approximately 0.99995
# Therefore the normal dist. estimation of this integral is closer to the true value