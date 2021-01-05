library("R2OpenBUGS")
library(MASS) 
library(coda) 

model <- function() { 
  x ~ dbin(p,N)
  p ~ dbeta(1,1)I( 0.1, 0.6) 
}

model.file <- file.path("/home/tuomas/R/Projects/Bayesian\ analysis/BUGS_files/model.txt")
write.model(model, model.file)

tbl <- table(survey$Smoke) 
N <- as.numeric(sum(tbl)); N 
y <- N - as.numeric(tbl["Never"]); y 
data <- list("N", "y")
params <- c("p")

inits <- function() { list(p=0.5) }

out <- bugs(data, inits, params, model.file, n.iter=10000)

all(out$summary[,"Rhat"] < 1.1) 
out$mean["p"] 
out$sd["p"] 
print(out, digits=5)


out <- bugs(data, inits, params, model.file, codaPkg=TRUE, n.iter=10000) 
out.coda <- read.bugs(out) 
xyplot(out.coda)
densityplot(out.coda)
acfplot(out.coda)
