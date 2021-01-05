source("/home/tuomas/R/Projects/Bayesian analysis/Take_home_task/pg.R")

fn <- "/home/tuomas/R/Projects/Bayesian analysis/Take_home_task/taskd.jag"
cat("
  model{

  mu ~ dnorm(4.7, 1/v)

  }",
    file = fn)

# a)
taskd.data <- list(v = 1000)
taskd.model <- jags.model(file = fn, data = taskd.data, n.chains = 3,
                          n.adapt = 2000)

taskd.pars <- c("mu")
taskd.postInfo <- coda.samples(model = taskd.model, variable.names = taskd.pars, 
                               n.iter = 100000, thin = 10)

summary(taskd.postInfo)
plot(taskd.postInfo)