# Example on p. 188 of ISLR:
n = 100

# Simulate the two financial assets x and y:
x = rnorm(n)
y = 0.5*x + rnorm(n)

# Var(x) = 1, Var(y) = 1.25, cov(x, y) = 0.5
# Check:
var(x)
var(y)
cov(x,y)

# Alpha function from lecture slides:
alpha = function(x, y) (var(y)-cov(x,y))/(var(x) + var(y) - 2*cov(x,y))
alpha(x, y)
# Note the true theoretical alpha = (1.25 - 0.5)/(1 + 1.25 - 2*0.5) = 0.6

# What is the SE of alpha?
# No theory exists about it!

# Simulate many alphas from the true population:
R = 100000
out = 1:R

for (i in 1:R) 
{
  x = rnorm(n)
  y = 0.5*x + rnorm(n)
  out[i] = alpha(x, y)
}

hist(out)
mean(out)
sd(out)
   
# Estimated SD of alpha is about 0.08 
# Again, there is no theoretical result to compare to!

# Let us now do the same with bootstrap:
# We have only one sample to work with!!
# Generate that sample:

set.seed(265)
x = rnorm(n)
y = 0.5*x + rnorm(n)

# Generate the boostrap samples:
# We will use the sample function to resample (x,y)
b.ind = sample(1:n, replace = T)
b.ind # note some indeces are repeated!

out.boot = 1:R
for (i in 1:R) 
{
  b.ind = sample(1:n, replace = T)
  out.boot[i] = (var(y[b.ind])-cov(x[b.ind],y[b.ind]))/(var(x[b.ind]) + var(y[b.ind]) - 2*cov(x[b.ind],y[b.ind]))
  
}

hist(out.boot)
mean(out.boot)
sd(out.boot)
# Quite close to the values obtained with simulating from the population!

# All this can be done automatically with library boot:

library("boot")
# Define function to estimate alpha
alpha.fcn = function(data, indices) 
{
  d = data[indices,] # allows boot to select sample
  a = (var(d[,2])-cov(d[,1],d[,2]))/(var(d[,1]) + var(d[,2]) - 2*cov(d[,1],d[,2]))
  return(a)
} 
results = boot(data=cbind(x,y), statistic = alpha.fcn, R=1000)
results
hist(results$t)
mean(results$t)
sd(results$t)

# You can also obtain CI:
boot.ci(results, type="all")

# Or manually with the correct empirical quantiles from the bootstrap samples:
quantile(results$t, c(0.025, 0.975)) # result does not match with output because boot has bias correction

# Or with SE:
mean(results$t) - 1.96*sd(results$t)
mean(results$t) + 1.96*sd(results$t)

# To check normality:
plot(results) # Looks good, so the normal CI is probably ok


# Example 2: Studying the SD of the coefficient of variation CV = sd/x.bar
x <- c(0.73, 3.62, 1.11, 1.30, 1.19, 2.74, 1.69, 2.55, 0.38, 0.45,
1.39, 0.06, 1.06, 0.07, 0.19, 0.35, 0.15, 2.84, 0.58, 0.03)

# Alternative method of specifying the function is with weights
cv <- function(x, w) 
  {
   xbar <- sum(x*w)
   sqrt((sum(w*x^2) - xbar^2)*length(x)/(length(x)-1))/xbar
  }
b <- boot(x, statistic = cv, 1000, stype = "w")
boot.ci(b, conf = 0.95)
abc.ci(x, cv, conf = 0.95)

hist(b$t)
plot(b)

cv1 <- function(x, ind) 
{
  d = x[ind]
  sd(d)/mean(d)
}
b1 <- boot(x, statistic = cv1, 1000)
boot.ci(b1, conf = 0.95)


# Exercise:
# Generate 100 observations from the N(0, 1) distribution
# Obtain an estimate of the SE of the sample variance
# Note: True theoretical SE of variance = sqrt(2*sigma^4/(n-1))