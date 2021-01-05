mh_gibbs = function(niter, theta.vec, c=1, d=1, e=1, f=1){
  # Generate data
  n.vec = c(10,12,15,16,20,19,10,11,12,14)
  x.vec = rbinom(10, 150, 0.5)
  # Initialize
  theta.samples = NULL
  alpha.samples = NULL
  beta.samples = NULL
  alpha.t = 1
  beta.t = 1
  for(i in 1:niter){
    # Gibbs sampling for thetas
    teta.t.samples = NULL
    for(j in 1:10){
      theta.vec[j] = rbeta(1, x.vec[j]+alpha.t, n.vec[j]-x[j]+beta.t)
    }
    # MH sampling for alpha & beta
    # Proposal dist = dnorm()
    # For alpha
    alpha.star = rnorm(1, alpha*(i-1), 20)
    # Acceptance probability for alpha
    log.a = log(pgamma(alpha.star, c, d**(-1)) / pgamma(alpha.t, c, d**(-1))) #??? Calculate posterior
    if(log(runif(1)) < log.a){
      alpha.t = alpha.star
    }
    alpha.samples = append(alpha.samples, alpha.t)
    # For beta
    beta.star = rnorm(1, alpha*(i-1), 20)
    log.a = log(pgamma(beta.star, e, f**(-1)) / pgamma(beta.t, e, f**(-1))) #??? Calculate posterior
    if(log(runif(1)) < log.a){
      beta.t = beta.star
    }
    beta.samples = append(beta.samples, beta.t)
  }
  
  
}