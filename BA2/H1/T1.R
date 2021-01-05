# Exercises 1, Task 1
# i)
par(mfrow = c(2,1))

p = rbeta(100000, 6, 5)
hist(p, main = "Sampling distribution of Beta(6,5)", nclass = 20)

x <- seq(0,1, by = 0.01)
y = dbeta(x, 6, 5)
plot(x, y, type = "l", main = "Beta(6,5) distribution")

# ii) Empirical distribution of the odds
par(mfrow = c(1,1))

odds = function(p){p/(1-p)}
p_odds=sapply(p, odds)
hist(p_odds, main = "Sampling distribution of odds of Beta(6,5)", nclass = 25)

# iii)
odds_mean = mean(p_odds)
odds_sd = sd(p_odds)
cat("Mean of odds:", odds_mean, "\nsd of odds:", odds_sd)

# iv) Approximate the probability that odds is greater than, or equal to 1 and .5.
#prob_ge1 = length(p_odds[p_odds >= 1]) / length(p_odds)
prob_ge1 = mean(p_odds >=1)
#prob_ge.5 = length(p_odds[p_odds >= .5]) / length(p_odds)
prob_ge.5 = mean(p_odds >=.5)
  
cat("P(p_odds >= 1) =", prob_ge1, "\nP(p_odds >= .5) =", prob_ge.5)

# v) Using appropriate quantiles, give the approximate 90% quantile intervals for odds.
quantiles = quantile(p_odds, probs = c(0.1, 0.9))
q1_ = quantiles[1]
q2_ = quantiles[2]
prob_q1_q2_ = length(p_odds[p_odds > q1_][p_odds < q2_]) / length(p_odds)
cat("Approximate (2) 90% quantile intervals for odds:",
    "\nQuantile 1 =", q1_,
    "\nQuantile 2 =", q2_,
    "\nCumulative probability in the interval = ", prob_q1_q2_)









