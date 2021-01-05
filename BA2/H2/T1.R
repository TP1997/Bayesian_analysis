
# b
# Write a function in R to generate samples from the Logistic based on your algorithm.
# Make a histogram with the samples.

F_inv = function(u){-log(1/u - 1)}
u = runif(10000)

par(mfrow=c(2,1))
x_log = F_inv(u)
hist(x_log, main = "Inverse transformation sampling distribution of Log(0,1)", nclass = 25)

x = rlogis(10000,0,1)
hist(x, main = "Distribuion sampled directly from Log(0,1)", nclass = 25)

# c
# Use the samples to estimate P (X ∈ (2, 3)).

x_log2 = x_log[x_log <= 3]
x_log2 = x_log2[x_log2 >= 2]
p_23 = length(x_log2)/length(x_log)
cat("Estimated prob of P(X ∈ (2,3)):", p_23,
    "\nTrue prob of P(X ∈ (2,3)):", plogis(3)-plogis(2))
