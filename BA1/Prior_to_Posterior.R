# 4. Prior to Posterior : a bag with unknown proportion of red balls
#
# Suppose that the total number of balls is 100 in a bag.
# The prior distribution is set to  r(100-r)/166650
# (The number 166650 is to make the prior probabilities sum to one.)
#
# We suppose that the data comprise n=20 draws, in which s=7 red balls are observed.
#
# Then, the prior distribution looks like:
r <- seq(0,100, 1)
prior.d <- r*(100-r)/166650
plot(r, prior.d)

# For the posterior distribution, we have two parts:
# numerator(likelihood x prior) and denominator (marginal distribution)
denom <- 0
for (k in 1:100){
  temp <- k^8*(100-k)^14  /(166650*100^20)
  denom<- denom + temp
}
numer <- r^8*(100-r)^14  /(166650*100^20)
post.d <-  numer/denom

# Plot the prior and posterior
plot( r, post.d, type='l', col="red") 
lines(r, prior.d, col="green")  