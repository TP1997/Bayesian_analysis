source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/bb.R")
# 2. Example: Clinical trial data
"
A new treatment protocol is proposed for a particular form of cancer. The measure of
success will be the proportion of patients that survive longer than six months after
diagnosis. With the present treatment, this success rate is 40%. Letting theta be the
success rate of the new treatment, a doctor assesses her prior beliefs about theta as
follows. She judges that her expectation of theta is E(theta)=0.45, and her standard
deviation is 0.07. Assume that her beliefs can be represented by a beta distribution.

A clinical trial of the new treatment protocol is carried out. Out of 70 patients
in the trial, 34 survive beyond six months from diagnosis.Should the new treatment
protocol replace the old one?
"

# Likelihood:  Binomial (n, theta)
clin.dat <- c(rep(1, 34), rep(0,36) )

# Conjugate prior for theta:  Beta(22.28, 27.23)
# Obtain the Posterior distribution with the mean, variance, and the mode:
bb.sum(clin.dat, 1, 22.28,27.23)
