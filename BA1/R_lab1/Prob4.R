source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/bb.R")
"
4. An usual condition of pregnancy called placenta previa obstructs normal baby delivery.
An early study concerning the gender (sex) of placenta previa births in Germany found
that a total of 980 births, 437 were female.
"
data <- c(rep(1, 437),rep(0,980-437))
# bb.sum(data, 0.5, 1/12)
# bb.trip(data, 0.5, 1/12)
# bb.hdi(data, 0.5, 1/12, .95)
# i) Obtain the posterior distribution using a conjugate prior with alpha 0.5 and beta 1/12.
data <- c(rep(1, 437),rep(0,980-437))
bb.sum(data, 1, 0.5, 1/12)
bb.trip(data, 1, 0.5, 1/12)

# ii) Find the 95% highest posterior density interval for the unknown proportion of placenta
# previa births.
bb.hdi(data, 1, 0.5, 1/12, .95)

# iii) How much evidence does this provide for the claim that the proportion of female births in
# the population of placenta previa births is less than 0.485 (the currently assumed proportion
# of female births in the general population)? Compute the appropriate posterior probability.