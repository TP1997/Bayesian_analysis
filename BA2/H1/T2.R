# Exercises 1, Task 2
# i) Compute the probability : P(Z =<3)- P(Z<1), where Z ~N(0,1).
p = pnorm(3) - pnorm(1)
cat("True probability P(Z =<3)- P(Z<1):", p)

# ii)
x_norm = rnorm(10000)
n_le3 = length(x_norm[x_norm<=3])
n_l1 = length(x_norm[x_norm<1])
mc_est_norm = (n_le3 - n_l1) / length(x)
cat("MC-estimetion of probability P(Z =<3)- P(Z<1) with N(0,1) sampling:", mc_est_norm,
    "\nQuality of the approximation (sample variance, less is better):", var(x_norm))

# iii)
f = function(x){2/sqrt(2*pi) * exp(-0.5*x**2)}

x_unif = runif(10000, 1, 3)
#mc_est_unif = mean(f(x_unif))
mc_est_unif = mean( 2* dnorm(x_unif ) )
cat("MC-estimetion of probability P(Z =<3)- P(Z<1) with U(1,3) sampling:", mc_est_unif,
    "\nQuality of the approximation (sample variance, less is better):", var(x_unif))