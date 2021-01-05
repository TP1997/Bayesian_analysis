gibbs.n <- function( iter, mu0, tau0, X=X ) {
  mu <- mu0
  tau <- tau0
  mus <-NULL
  sds <-NULL
  taus <-NULL
  xbar <- mean(X)
  n <- length(X)
  for(i in 1: iter ) {
    mu <- rnorm(1, xbar, sqrt(tau/n) )
    tau <- sum((X-mu)^2)/rchisq(1,n)
    std <- sqrt(tau)
    mus <- append(mus,mu)
    vars<- append(taus, tau)
    sds <- append(sds, std)
  }
  list(mu=mus, tau=taus, sd=sds)
}
X <- rnorm(15,3,5) # normal sample with mean3, var 25
eg <- gibbs.n(10000,0,1, X)
hist(as.vector(eg$mu), prob=T, nclass=50)