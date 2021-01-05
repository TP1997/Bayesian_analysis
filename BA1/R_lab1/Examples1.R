source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/pg.R")
# 1. Class Example: Transmission Error Data (Lecture notes in p12)
"
Data: 6 one-hour observation periods with 1, 0, 1, 2, 1, 0 transmission errors. The data can
be modeled as a sample from a Poisson distribution with mean rate lambda.
The data on previous system established the mean error rate as 1.6 errors per hour and the new
system has a design goal of cutting this rate in half to .8 errors per hour.
An expert has a prior belief that the median of the new system should be close to .8. Consider
a gamma distribution with alpha=2 and beta=1/2.1 as the prior distribution. Does the new system
seem to be able to achieve the design goal?
"

tran.dat <- c(1,0,1,2,1,0)

# To obtain the conjugate posterior distribution, you need to specify only data set and parameters
# in the function 'pg.sum'
pg.sum( tran.dat,2,2.1)   #The second parameter in Gamma distn is specified as 1/beta in this function.

# Triplot of the prior, likelihood, and the posterior distribution in one figure:
pg.trip( tran.dat, 2, 2.1)

# 90% Posterior highest density Interval:
pg.hdi( tran.dat, 2, 2.1, .9)
