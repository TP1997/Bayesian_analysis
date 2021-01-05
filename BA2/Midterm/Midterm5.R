# a)
setwd('/home/tuomas/R/Projects/DATA.STAT.730/Midterm')
fn = "Midterm5.jag"
cat("
  model{
  # Unique linear models for each group
    for(i in 1:9){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b11 + b12*(age[i]-60)
    }
    for(i in 10:18){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b21 + b22*(age[i]-60)
    }
    for(i in 19:27){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b31 + b32*(age[i]-60)
    }
    for(i in 28:36){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b41 + b42*(age[i]-60)
    }
    # Priors
    b11 ~ dnorm(0, 0.01) # For group 1
    b12 ~ dnorm(0, 0.01)
    b21 ~ dnorm(0, 0.01) # For group 2
    b22 ~ dnorm(0, 0.01)
    b31 ~ dnorm(0, 0.01) # For group 3
    b32 ~ dnorm(0, 0.01)
    b41 ~ dnorm(0, 0.01) # For group 4
    b42 ~ dnorm(0, 0.01)
  }",
    file=fn)

deaths = c(18,22,19,55,117,170,179,120,120,2,4,3,38,113,173,212,243,253,149,169,193,
           576,1001,901,613,337,189,124,140,187,514,778,689,432,214,63)
age = c(40,45,50,55,60,65,70,75,80,40,45,50,55,60,65,70,75,80,40,45,50,55,60,65,70,75,
        80,40,45,50,55,60,65,70,75,80)
n = c(656,359,249,632,1067,897,668,361,274,145,104,98,372,846,949,824,667,537,4531,
      3030,2267,4682,6052,3880,2033,871,345,3410,2239,1851,3270,3791,2421,1195,436,113)
g = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4)

smoking.data = list(y=deaths, age=age, n=n)

smoking.j = jags.model(file=fn, data=smoking.data, n.chains=3, n.adapt=2000)
smoking.par = c("b11","b12","b21","b22","b31","b32","b41","b42")

smoking.res = coda.samples(model=smoking.j, variable.names=smoking.par, n.iter=10000, thin=10)
summary(smoking.res)

# Comments:
# It seems that the increase of age have similar effect to probability of death apart from the group.

# b) I Guess I dont get the point of this question
setwd('/home/tuomas/R/Projects/DATA.STAT.730/Midterm')
fn = "Midterm5b.jag"
cat("
  model{
  # Unique linear models for each group
    for(i in 1:9){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b11 + b12*(age[i]-60)
    }
    for(i in 10:18){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b21 + b22*(age[i]-60)
    }
    for(i in 19:27){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b31 + b32*(age[i]-60)
    }
    for(i in 28:36){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) = b41 + b42*(age[i]-60)
    }
    # Priors
    b11 ~ dnorm(mu, 0.01) # For group 1
    b12 ~ dnorm(mu, 0.01)
    b21 ~ dnorm(mu, 0.01) # For group 2
    b22 ~ dnorm(mu, 0.01)
    b31 ~ dnorm(mu, 0.01) # For group 3
    b32 ~ dnorm(mu, 0.01)
    b41 ~ dnorm(mu, 0.01) # For group 4
    b42 ~ dnorm(mu, 0.01)
    
    # Hyperpriors
    mu ~ dnorm(60, 0.01)
  }",
    file=fn)

deaths = c(18,22,19,55,117,170,179,120,120,2,4,3,38,113,173,212,243,253,149,169,193,
           576,1001,901,613,337,189,124,140,187,514,778,689,432,214,63)
age = c(40,45,50,55,60,65,70,75,80,40,45,50,55,60,65,70,75,80,40,45,50,55,60,65,70,75,
        80,40,45,50,55,60,65,70,75,80)
n = c(656,359,249,632,1067,897,668,361,274,145,104,98,372,846,949,824,667,537,4531,
      3030,2267,4682,6052,3880,2033,871,345,3410,2239,1851,3270,3791,2421,1195,436,113)
g = c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4)

smoking.data = list(y=deaths, age=age, n=n)

smoking.j = jags.model(file=fn, data=smoking.data, n.chains=3, n.adapt=2000)
smoking.par = c("b11","b12","b21","b22","b31","b32","b41","b42")

smoking.res = coda.samples(model=smoking.j, variable.names=smoking.par, n.iter=10000, thin=10)
summary(smoking.res)