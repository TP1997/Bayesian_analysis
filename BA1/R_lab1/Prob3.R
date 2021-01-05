source("/home/tuomas/R/Projects/Bayesian analysis/R_lab1/pg.R")
"
3. The numbers of misprints (typos) spotted on the first few pages of an early draft of a
book were

3, 4, 2, 1, 2, 3

It seems reasonable that these numbers should constitute a sample from a Poisson
distribution of unknown mean lambda. From the past experience of typing such a book,
the author would like to use a prior with a mean about 2 and variance about 4/3.
"

# i) Using the conjugate prior distribution, find the posterior distribution.
data <- c(3, 4, 2, 1, 2, 3)
prior.a <- 3
prior.b <- 3/2 
post.info <- pg.sum(data, 3, 3/2)
post.a <- post.info$alfa1
post.b <- post.info$beta1
cat("Post. dist: lambda|x_1,...,x_n ~ Gamma(", post.a, ",", 1/post.b, ")")

pg.sum(data, 3, 3/2)
pg.trip(data, 3, 3/2)
# ii) Give the plots of prior and the posterior distribution on one page.
lambda <- seq(0, 10, .1)
prior.d <- dgamma(lambda, prior.a, prior.b)
post.d <- dgamma(lambda, post.a, post.b)
plot(lambda, post.d, type = "l", col="black", ylab = "y", main="Posterior (black), prior (red)")
lines(lambda, prior.d, type = "l", col="red")

pg.trip(data, prior.a, prior.b)

lam <-seq(0, 3, length=50 )
prior.g <- dgamma(lam , 3, 3/2 )
post.g <- dgamma(lam, 18, 7.5) # for plots, 2nd parameter= 1/beta
plot(lam, post.g, type="l",col="1" )
lines(lam, prior.g, type="l", col="2")
