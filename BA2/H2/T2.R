
# b
# Implement the algorithm in R and make a histogram with the samples drawn to
# esimate the density of X.
bound = function(x){(1/2.1)*(exp(-1) + (exp(1) - 1)*exp(x*(1 - exp(1))))}

N=10000
x = rexp(N*10)
u = runif(N*10)
x_ = sapply(x, bound)

x_ar = x[u <= x_][0:N]

par(mfrow=c(1,1))
hist(x_ar, main="Accept-Reject sampling dist. of X", nclass = 25)