## read in the data file
setwd('/home/tuomas/R/Projects/DATA.STAT.730/H4')
bikes <- read.table("bikes_dat.txt", header=TRUE)
y <- bikes$bikes
n <- y + bikes$other
m <- length(n)

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

ab.lprior <- function(alpha, beta, c, d, e, f)
{
  return((c-1)*log(alpha) - alpha*d + (e-1)*log(alpha) - beta*f)
}

## set up the MCMC, allocate space, etc
S <- 100000
c=1
d=10
e=1
f=10
alpha <- beta <- rep(NA, S)
alpha[1] <- 1
beta[1] <- 1 
lpost <- ab.marg.llik(alpha[1], beta[1], y, n) + ab.lprior(alpha[1], beta[1], c,d,e,f)

## MCMC rounds for alpha and beta
for(s in 2:S) {
  
  ## propose new alpha and calculate new log posteiror
  aprime = rnorm(1, alpha[s-1], 20)
  lprime <- ab.marg.llik(aprime, beta[s-1], y, n) + ab.lprior(aprime, beta[s-1], c,d,e,f)
  cat(lprime)
  ## accept or reject the proposal
  if(runif(1) < exp(lprime - lpost) * alpha[s-1]/aprime) {  ## accept
    alpha[s] <- aprime
    lpost <- lprime
  } else alpha[s] <- alpha[s-1]  ## reject
  
  
  ## propose new BETA and calculate new log posteiror
  bprime <- rnorm(1, beta[s-1], 20)
  lprime <- ab.marg.llik(alpha[s], bprime, y, n) + ab.lprior(alpha[s], bprime, c,d,e,f)
  
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

