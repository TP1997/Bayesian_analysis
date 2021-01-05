# 3. Continuing the setup of the problem 2, consider different choices of priors as follows.
theta <- seq(0, 1, 1/49)
par(mfrow=c(2,2))
# a) Chage the prior parameters to a=2, b=2. Then, draw the prior density and the posterior density
# on one page.
a <- b <- 2
prior.dist <- dbeta(theta, a, b)
plot(theta, prior.dist, type = "l")
mtext("prior",side=3)

post.dist = dbeta(theta, a+7, b+10-7)
plot(theta, post.dist, type = "l")
mtext("Posterior",side=3)

# b) Repeat a) with a=2, b=9. What do you find from the plots?
b <- 9
prior.dist <- dbeta(theta, a, b)
plot(theta, prior.dist, type = "l")
mtext("prior",side=3)

post.dist = dbeta(theta, a+7, b+10-7)
plot(theta, post.dist, type = "l")
mtext("Posterior",side=3)