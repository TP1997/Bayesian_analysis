# Exercise 1, Task 4

# i) Estimate the normalizing constant using the exponential (0.1) random variable.
f_original = function(x){(2+sin(x)^2)*exp(-(3+cos(x)^3)*x)}
k_original = integrate(f_original, lower=0, upper=Inf)$value

f2 = function(x){2*(2+sin(x)^2)*exp(-x*cos(x)^3)*exp(-2.5*x)}
x = rexp(100000, 0.5)

k2 = mean(f2(x))
cat("MC-etimate for normalizing constant using Exp(0.5):", 1/k2,
    "\nTrue normalizing constant:", 1/k_original)


# ii)
f1 = function(x){1/10*(2+sin(x)^2)*exp(-x*cos(x)^3)*exp(7*x)}
x = rexp(100000, 10)

k1 = mean(f1(x))
cat("MC-etimate for normalizing constant using Exp(10):", 1/k1,
    "\nTrue normalizing constant:", 1/k_original)
