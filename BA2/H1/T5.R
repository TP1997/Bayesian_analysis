# Exercise 1, Task 5

# i) appropriate bound (f(x)/Mg(x)): 2/5 * (2 + cos(x))
bound = function(x){(2/5)*(2+cos(x))}

# ii) Write a function that simulates N independent realizations 
#     from X by using accept-reject sampling method.
N=1000
x = rexp(N*10, 1)
u = runif(N*10, 0,1)
x_ = sapply(x, bound)

xs2 = x[u <= x_][0:N]

cat("\nMean of samples:", mean(xs2))


