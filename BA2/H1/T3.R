# Exercises 1, Task 3
# i)
par(mfrow = c(2,1))

f_exp = function(u, lambda) {-lambda*log(u)}
x_unif = runif(100000)
x_exp = sapply(x_unif, f_exp, lambda=2.0)
hist(x_exp, main = "Sampling distribution of exp(0.5)", nclass = 25)
# True distribution
x = seq(0,5,0.01)
y = dexp(x, rate = 0.5)
plot(x, y, type = "l", main = "exp(0.5) distribution")


# ii) Consider the integral :   Int (from 0 to infinity)  x^2 sin (pi x) exp(-x/2) dx
f1 = function(x){(x**2) * sin(pi*x) * exp(-x/2)}
f2 = function(x){(2*x**2) * sin(pi*x)}
x_exp2 = rexp(100000, rate = 0.5)

mc_est = mean(f2(x_exp2))
cat("Monte carlo integral approximation:", mc_est)
cat("True value of the integral:", integrate(f1,lower=0, upper=Inf)$value)


# iii) Approximate the integral using the uniform variables drawn in i)
mc_est = mean(f2(x_exp))
cat("Monte carlo integral approximation based on i):", mc_est)
cat("True value of the integral:", integrate(f1,lower=0, upper=Inf)$value)