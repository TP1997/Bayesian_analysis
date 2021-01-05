fn <- "/home/tuomas/R/Projects/Bayesian analysis/RJAGS_projects/ItWrjags1.jag"
cat(
  "model{
  # Prior for pi:
  pi ~ dbeta(0.01, 0.01)
  ri ~ dbinom(pi, N)
  
  ri.new ~ dbinom(pi, N)
  pi.new <- ri.new/N
  
  }",
  file=fn
)
data <- list(ri=150, N=751)

inits <- list(pi=0.5, ri.new=0)
model <- jags.model(fn, data = data, inits = inits, n.chains = 3, n.adapt = 2000)
par <- c("ri.new", "pi.new")
res <- coda.samples(model, var=par, n.iter = 10000, thin = 10)
summary(res)
plot(res)