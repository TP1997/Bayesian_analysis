# a
fa = function(theta, x=2){(theta/(1+theta^2))*exp(-0.5*(x-theta)^2)}

theta = seq(-6,10,0.01)
par(mfrow=c(1,1))
plot(theta, fa(theta), type = "l", main = "Plot of integrand given in a.", xaxt="n")
axis(1, at=seq(-6,10,1), las=2)

fmca = function(theta, x=2){pi*theta*exp(-0.5*(x-theta)^2)}
theta = rcauchy(10000, location = 0, scale = 1)
mc_int_a = mean(fmca(theta))

cat("MC-estimate for integral:", mc_int_a,
    "\nTrue value of the integral:", integrate(fa, lower = -Inf, upper = Inf)$value)

# b
fb = function(theta, x=2){(1/(1+theta^2))*exp(-0.5*(x-theta)^2)}
theta = seq(-6,10,0.01)
plot(theta, fa(theta), main = "Plot of integrands given in b.", xaxt="n",
     type = "l", col="red")
axis(1, at=seq(-6,10,1), las=2)
lines(theta, fb(theta), type = "l", col="blue")
legend(-6,0.4, legend = c("fa", "fb"), col = c("red", "blue"), lty = 1:1, cex = 0.8)

fmcb = function(theta, x=2){pi*exp(-0.5*(x-theta)^2)}
theta = rcauchy(10000, location = 0, scale = 1)
mc_int_b = mean(fmcb(theta))

cat("MC-estimate for integral:", mc_int_a / mc_int_b,
    "\nTrue value of the integral:", 
    (integrate(fa, lower = -Inf, upper = Inf)$value) / (integrate(fb, lower = -Inf, upper = Inf)$value))




