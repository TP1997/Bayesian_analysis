# 1. Suppose that the posterior distribution you obtain is:
#                     theta | data ~ Gamma (6,12)

# a) First, set the theta values as a sequence from 0 to 1 spaced with 0.02.
theta <- seq(0, 1, 0.02)
# b) Draw the posterior density function using the theta values given above.
plot(theta, dgamma(theta, 6, rate=12), type = "l")

# c) Find the posterior probability that theta is greater than 0.3. i.e. P(theta > 0.3 | X)=?
1-pgamma(0.3, 6, 12)

# d) A horizontal line defined by c=0.72 cuts the density function at two theta values, theta= 0.2001
#    and theta=0.7514. Find the probabilities corresponding to these two theta values.
theta1 <- pgamma(0.2001, 6, 12)
theta2 <- pgamma(0.7514, 6, 12)

# e) In c), when you subtract the smaller probability from the bigger one, this is the percent (probability)
#    coverage for the HPD interval with theta= 0.2001 and theta=0.7514.
#    What percentage HPD interval is obtained here?
hdp.int <- theta2-theta1

# f) Generate a random sample of size 100 from this posterior distribution.
sample <- rgamma(100, 6, 12)

# g) Write your own function which summarizes a data set with mean, median, standard deviation, and
#    025 and 0.975 quantiles (i.e. 95% probability interval).

sample_summary = function(sample){
  n <- length(sample)
  mean <- sum(sample)/n
  median <- median(sample)
  sd <- sqrt(var(sample))
  quantiles <- quantile(sample, probs=c(0.025, 0.975))
  cat("Summary\n")
  cat("Mean:", mean)
  cat("\nMedian:", median)
  cat("\nSd:",sd)
  cat("\nQuantiles:\n")
  print(quantiles)
}

# h) Using the function you wrote in g, give the summary of your random sample generated in f).
sample_summary(sample)

