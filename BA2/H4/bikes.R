## read in the data file
setwd('/home/tuomas/R/Projects/DATA.STAT.730/H4')
bikes <- read.table("bikes_dat.txt", header=TRUE)
y <- bikes$bikes
n <- y + bikes$other
m <- length(n)


# bikes data - data file is in 'bikes_dat.txt' /one can make a data file from this!)
#
#  bikes other
#     16    58
#      9    90
#     10    48
#     13    57
#     19   103
#     20    57
#     18    86
#     17   112
#     35   273
#     55    64


##
## part (b)
##

## load necessary libraries
library(coda)


## ab.marg.llik:
##
## function for calculating the marginal log-likelihood
## of the alpha and beta parameters to the hierarchical
## beta-binomial model

ab.marg.llik <- function(alpha, beta, y, n)
  {
    m <- length(y)
    const <- m*(lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta))
    lprod <- sum(lgamma(alpha + y) + lgamma(beta + n - y) - lgamma(alpha + beta + n))
    return(const + lprod)
  }


## ab.lprior:
##
## function for evaluating the log-prior of alpha and
## beta for the hierarchical beta-binomial model

ab.lprior <- function(alpha, beta)
  {
    return(-(5/2)*log(alpha + beta))
  }


## set up the MCMC, allocate space, etc
S <- 100000
alpha <- beta <- rep(NA, S)
alpha[1] <- 1
beta[1] <- 1 
lpost <- ab.marg.llik(alpha[1], beta[1], y, n) + ab.lprior(alpha[1], beta[1])
L <- 1
U <- 2

## MCMC rounds for alpha and beta
for(s in 2:S) {

  ## propose new alpha and calculate new log posteiror
  aprime <- runif(1, L*alpha[s-1]/U, L*alpha[s-1]*U)
  lprime <- ab.marg.llik(aprime, beta[s-1], y, n) + ab.lprior(aprime, beta[s-1])

  ## accept or reject the proposal
  if(runif(1) < exp(lprime - lpost) * alpha[s-1]/aprime) {  ## accept
    alpha[s] <- aprime
    lpost <- lprime
  } else alpha[s] <- alpha[s-1]  ## reject
  
  
  ## propose new BETA and calculate new log posteiror
  bprime <- runif(1, L*beta[s-1]/U, U*beta[s-1]*L)
  lprime <- ab.marg.llik(alpha[s], bprime, y, n) + ab.lprior(alpha[s], bprime)

  ## accept or reject the proposal
  if(runif(1) < exp(lprime - lpost) * beta[s-1]/bprime) {  ## accept
    beta[s] <- bprime
    lpost <- lprime
  } else beta[s] <- beta[s-1]  ## reject

}

##
## converting samples of alpha and beta into samples of theta
##

## allocate space for the samples
theta <- matrix(NA, nrow=S, ncol=m)

## gather the samples by direct MC
for(j in 1:m) {
  theta[,j] <- rbeta(S, alpha + y[j], beta + n[j] - y[j])
}

