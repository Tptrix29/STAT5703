# Binomial example simulation
R = 10000
n = 100  # sample size 
p0 = 0.9 # Null hypothesis value

x = rbinom(R, n, p0)
p.hat = x/n
G2 = 2*(x*log(p.hat/p0) + (n-x)*log((1-p.hat)/(1-p0)))

hist(G2, freq = F, breaks=15)
grid = seq(0, 15, len = 100)
lines(grid, dchisq(grid,1), col = "red") # overlapping the theoretical distribution


# Using the LRT for finding the so-called profile CIs
# Find the solution to G2 = quantile of chi-square with df = 1

x = rbinom(1, n, p0)
p.hat = x/n
grid = seq(0.5, 1, len = 100)

# Define the -2loglikelihood ratio for binomial (from lecture)
G2 = function(p) 2*(x*log(p.hat/p) + (n-x)*log((1-p.hat)/(1-p)))
plot(grid, G2(grid), type = "l", xlab = "p", ylab = "-2logLR(p)",
     main = "-2logLR vs. parameter values")
arrows(p.hat-0.1, 30, p.hat, 0, angle = 15, col = "red", length = 0.1)
text(p.hat-0.15, 32, "MLE = argmin(-2logLR)", cex = 0.5)
points(p.hat, 0, pch = 16, col = "red")
points(p0, 0, pch = 16)
arrows(p0, 30, p0, 0, angle = 15, length = 0.1)
text(p0, 32, "True value of p", cex = 0.5)
abline(h=0)

for (i in 1:10)
{
  x = rbinom(1, n, p0)
  p.hat = x/n
  points(grid, G2(grid), type = "l", col = "gray")
}

abline(h = qchisq(0.95, 1), lty = 2)
text(0.59, 9, "5% upper quantile of ChiSq", cex = 0.5)

# install.packages("rootSolve")
library(rootSolve)
G2s = function(p) G2(p)-qchisq(0.975, 1)
uniroot.all(G2s,c(0.01,0.99))

#compare to Wald z CI:
c(p.hat-1.96*sqrt(p.hat*(1-p.hat)/n), p.hat+1.96*sqrt(p.hat*(1-p.hat)/n))