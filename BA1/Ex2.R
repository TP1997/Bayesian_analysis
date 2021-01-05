# 2. tack tossing problem
# Suppose that we wish to find out the probability of a tack (nasta) landing heads.

# a) First set the possible values of theta to be a sequence of length 50 from 0 to 1.
theta <- seq(0, 1, 1/49)
par(mfrow=c(2,2))

# b) Let Beta(a,b) be the prior distribution for the probability of a tack landing heads (theta).
# Set a=2 and b=3 to specify the prior distribution. Obtain the prior probability density function (pdf)
# and make a plot of it. Make also a title for the plot such as 'prior' on top of the plot.
prior.dist <- dbeta(theta, 2, 3)
plot(theta, prior.dist, type = "l")
mtext("prior",side=3)

# c) Suppose that we assume the total number of heads is modeled as a binomial distribution (n,theta). i.e.
# P(data|theta) ~ binom(n, theta) :  likelihood.
# Let n=10 and the number of observed heads x=7.
# Find the likelihood and plot it over theta values. Add a label  'likelihood' for the plot.
lkhd.dist <- dbinom(7, 10, theta)
plot(theta, lkhd.dist, type = "l")
mtext("likelihood",side=3)

# d) Then, multiply the prior by the likelihood and draw this joint probability over theta. Give a label to
# the plot as 'prior x lkhd'.
post_kernel.dist <- prior.dist * lkhd.dist
plot(theta, post_kernel.dist, type = "l")
mtext("prior x lkhd",side=3)

# e) The posterior distribution is found to be a beta distribution with parameters a+x and b+n-x.
# (This will be derived during the lecture) Draw the posterior density function with a title 'Posterior'.
post.dist = dbeta(theta, 2+7, 3+10-7)
plot(theta, post.dist, type = "l")
mtext("Posterior",side=3)

# f) What do you find in the shape of  those four plots above? How is the contribution of the prior and
# likelihood to the posterior density?

# g) Here, a discretized version of the posterior can be obtained as follows:
# the joint probabilities obtained in d) / (divided by)  sum of the joint probabilities
# Give a name say, 'post.disc' to this discretized version.
# With this discretized version of the posterior, note that we could also obtain  fairly good discrete
# approximation to the posterior means, variances,  and probabilities.
post.disc <- post_kernel.dist / sum(post_kernel.dist)

# h) Compare the following:
# sum of  post.disc multiplied by theta   with (a+x)/(n+a+b) = Posterior mean
# sum of post.disc where [ theta <=.4 ] with  posterior probability that theta <=.4
a <- sum(post.disc * theta)
b <- (2+7) / (10+2+3)
c <- sum(post.disc[theta <= 0.4])
d <- pbeta(0.4, 2+7, 3+10-7)   
