# Simulating a z CI
R = 100
n = 50 # sample size (does not affect confidence!)
m = 0 # Choose the true value of the mean
d = matrix(rnorm(R*n, mean = m), R, n) # the correct value of mu of st. normal is 0
out = matrix(0, R, 2)

plot(1, type="n", xlab="", ylab="", xlim=c(-1, 1), ylim=c(0, R), main = "100 simulated 95% CIs")
# Simulating R CIs
for (i in 1:R) 
{
  out[i, 1] = mean(d[i,]) - qnorm(0.975)/sqrt(n) # l(x)
  out[i, 2] = mean(d[i,]) + qnorm(0.975)/sqrt(n) # u(x)
  if ((out[i, 1] < 0) & (out[i, 2] > 0)) lines(c(out[i,1], out[i,2]), c(i, i))
  else lines(c(out[i,1], out[i,2]), c(i, i), col = "red")
}

abline(v = m)


# Example 2: Exponential CI

l = 10 # Choose the true value of the parameter
n = 15 # sample size
x = rexp(n, l) # simulate some data

# Compute 95% CI for lambda:
(c(qgamma(0.025, n, 1)/sum(x), qgamma(0.975, n, 1)/sum(x)))


# Example: t CI
# read in some data
x <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)

# Compute a 88% CI for the population mean
t.test(x, conf.level = 0.88)$conf.int


# Profile likelihood CI example
p0 = 0.9
n = 50  # sample siz
x = rbinom(1, n, p0)
p.hat = x/n
grid = seq(0.7, 1, len = 100)

# Define the -2loglikelihood ratio for binomial (from lecture)
G2 = function(p) 2*(x*log(p.hat/p) + (n-x)*log((1-p.hat)/(1-p)))
plot(grid, G2(grid), type = "l")
abline(h = qchisq(0.95, 1), col = "red")

library(rootSolve)
G2s = function(p) G2(p)-qchisq(0.95, 1)
uniroot.all(G2s,c(0.01,0.99))
#compare to z CI:
c(p.hat-1.96*sqrt(p.hat*(1-p.hat)/n), p.hat+1.96*sqrt(p.hat*(1-p.hat)/n))