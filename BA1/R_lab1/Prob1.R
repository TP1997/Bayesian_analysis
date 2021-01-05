source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/nn.R")
"
1. A teacher administers a standardized aptitude exam to a class of 20 students.
She knows that the national distribution of scores for this exam is normal with mean 65
and standard deviation 9. She is willing to assume that students in her class have
aptitude values that are normally distributed with standard deviation 9, but with
unknown mean.

Her students come from an affluent, educated population. She assesses a 95% credible
(highest density) interval for the mean of her class distribution as (60,80). She
therefore assumes a priori that the mean of her class distribution is normal with mean 70
and standard deviation 5.

When the scores are returned, she opens the first 5 envelopes and sees scores of
78,80,95,81, and 77.
"
# i) Find the posterior distribution of class mean.
data <- c(78,80,95,81,77)
nn.sum(data, 70, 5^2, 9^2)
# Post. dist: mu|x ~ N(77.40291, 9.860097)

# ii) Obtain 95% highest posterior density interval for the mean.
nn.hdi(data, 70, 5^2, 9^2, .95)

# iii) Make three plots of prior, likelihood, and posterior distributions in one figure.
# Give some comments on these plots.
nn.trip(data, 70, 5^2, 9^2)
# Plots make sense intuitively, since it can be clearly seen that posterior distribution is
# a weighted average of likelihood and prior distributions.

# iv) Give a predictive distribution for the next observation using the code 'nn.pred'.
nn.pred(data, 70, 5^2, 9^2)
