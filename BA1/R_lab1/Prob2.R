source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/pg.R")
source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/eg.R")
"
2. The following data are failure times measured in CPU seconds for a real-time command
and control system. That is, the first failure occurred 3 seconds after the start of the
test, the second 30 seconds later at 33 seconds, ... the last occurred at 759 seconds.
The data consists of the following number of seconds between errors:

   3, 30, 113, 81, 115, 9, 2, 91, 112, 15, 138, 50

Assume that the times between failures are independent and distributed exponentially
with a constant failure rate. Assume a Gamma(1,1) prior distribution.
"
# i) Note that the Gamma distribution is the conjugate prior for the exponential
# distribution of the unknown failure rate. First, obtain the analytic form of the
# posterior distribution using conjugacy by hand. (Refer to lab 2 problem 5)

# p(x_i|theta) = theta* exp(-x_i *theta),    X_1,...X_n |theta ~ Exp(1/theta)

# theta|x_1,...,x_n ~ Gamma(13, 760)
# theta|x_1,...,x_n ~ Gamma(13, 1/760) !!

# ii) Obtain the posterior distribution using the code given above and fill in the blanks below.
data <- c(3, 30, 113, 81, 115, 9, 2, 91, 112, 15, 138, 50)
# pg.sum(data, 1, 1)
eg.sum(data, 1, 1)
eg.trip(data,1,1)
#             Mean          Standard Deviation
# Prior       1             1
# Posterior   0.0171        0.004734

# iii)  Set the sequence of 50 values from 0 to 0.10 first.
# Obtain the probability density functions of the prior and posterior distributions (separately)
# evaluated over the above values of the sequence.
seq <- seq(0, .1, .1/49)
eg.sum(seq, 1, 1)
prior <- dgamma(seq, 1, 1)
post <- dgamma(seq, 13, 760)
lkhd <- dgamma(seq, 12, sum(data))

plot(seq, post, type="l", col="green")
lines(seq, prior, type="l", col="black")
lines(seq, lkhd, type="l", col="red")
# Prior: theta ~ Gamma(1,1)
# Posterior: theta|x_1,...,x_n ~ Gamma(51, 3.5)

# iv) Make plots of prior and posterior distributions overlaid in one figure.
# Comment on the plots, including the central tendency of each of the plotted functions and
# the amount of spread in each of the plotted function.
theta <- seq(0,0.05,length= 50)
prior.d <- dgamma(theta, 1, 1)
post.d <- dgamma(theta, 13, 760)
plot(theta, prior.d, type = "l", col="red", ylab = "y", main="Posterior (black), prior (red)", ylim=c(0,100))
lines(theta, post.d, type = "l", col="black")

th <- seq(0,0.1,length= 50)
th.post <- dgamma(th,13,760)
th.prior <- dgamma(th,1,1)
x.lkhd <- dgamma(th,12, sum(cpu.dat) ) # likelihood: Exp(θ) = Gamma (n, θ)
plot(th, th.post,type="l",col="black", ylim=c(0,90) )
lines(th, th.prior, lty=2, col="red")
lines(th, x.lkhd, lty=2, col="3")

# v) The software test continued for 91208 CPU seconds. The below shows a sequence
# of 13 additional observations taken later during the test. Use these observations to
# create a data set of 12 times between failures.

# 26, 114, 325, 55, 242, 68, 422, 180, 10, 1146, 600, 15

# Under the assumption of this problem that the times between failures are independent,
# exponentially distributed with constant failure rate, it would be appropriate to use the
# posterior distribution from i) (or ii)) as your prior distribution for this data set.

# Obtain the posterior distribution and make two plots as in iv). Comment on your results.

data <- c(26, 114, 325, 55, 242, 68, 422, 180, 10, 1146, 600, 15)
post.info <- eg.sum(data, 13, 760)
post.info

theta <- seq(0,0.1,length= 50)
prior.d <- dgamma(theta, 13, 760)
post.d <- dgamma(theta, post.info$alfa1, post.info$beta1)
plot(theta, prior.d, type = "l", col="red", ylab = "y", main="Posterior (black), prior (red)", ylim=c(0,300))
lines(theta, post.d, type = "l", col="black")

cpu2.dat = c( 26, 114, 325, 55, 242, 68, 422, 180, 10, 1146, 600, 15)
eg.sum(cpu2.dat, 13, 760) # Posterior: Gamma(25, 1/3963)
eg.trip(cpu2.dat, 13, 760)

th <- seq(0,0.1,length= 50)
th.post <- dgamma(th,25,3963) # dgamma function takes 1/beta, not beta in R
th.prior <- dgamma(th,13,760)
plot(th, th.post,type="l",col="1")
lines(th, th.prior, lty=2, col="2")