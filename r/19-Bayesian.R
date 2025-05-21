# Binomial example 1:

# Define a grid of 50 points between 0 & 1
n_grid = 50
p_grid = seq(from=0 , to=1 , length.out= n_grid )

# define the uniform prior on the grid
prior = dbeta(p_grid, 1, 1) # Note that Beta(1, 1) = U(0, 1)

par(mfrow=c(1,3))
plot(p_grid , prior, type = "l", xaxt="n", xlab=expression(theta) , main="Prior density" )
axis(1, xaxp=c(0, 1, 4), las=2)

# From lecture notes we have 6 winnings out of n = 9 plays
# Compute the likelihood at each point in the grid of probability theta
likelihood = dbinom(x = 6, size = 9, prob = p_grid) 

plot(p_grid , likelihood , type = "l", xaxt="n", xlab = expression(theta) , main="Likelihood" )
axis(1, xaxp=c(0, 1, 4), las=2)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# normalize the posterior, so the area under the integral is 1 (for plotting)
posterior <- unstd.posterior / sum(unstd.posterior*(1/n_grid))
# Alternative is to normalize the posterior, so it sums to 1 (for discrete computations)

plot(p_grid , posterior , type = "l", xaxt="n",xlab= expression(theta) , main="Posterior density" )
axis(1, xaxp=c(0, 1, 4), las=2)


# Plotting exact posterior with different prior parameters a and b, and different sample sizes
n = 5
x = 1 # sample proportion is 1/5 = 0.2
a = 5
b = 4

prior = dbeta(p_grid, a, b)
posterior = dbeta(p_grid, a+x, b+n-x)
plot(p_grid , prior, type = "l", xlab= expression(theta) , ylab="prior probability" , ylim =c(0, 10))
lines(p_grid, posterior, col = "red")
abline(v = x/n, lty = 2)
mtext("a = 5, b = 4, x = 1, n = 5", cex = 0.5)
legend(0.35, 6.2, legend=c("Prior", "Posterior", "Sample Mean"), col=c("black", "red", "black"), lty= c(1,1,2), cex = 0.6)


a = 5
b = 8
prior = dbeta(p_grid, a, b)
posterior = dbeta(p_grid, a+x, b+n-x) # Compute posterior with theoretical formula
plot(p_grid , prior, type = "l", xlab= expression(theta) , ylab="prior probability" , ylim =c(0, 10))
lines(p_grid, posterior, col = "red")
abline(v = x/n, lty = 2)
mtext("a = 5, b = 8, x = 1, n = 5", cex = 0.5)
legend(0.35, 6.2, legend=c("Prior", "Posterior", "Sample Mean"), col=c("black", "red", "black"), lty= c(1,1,2), cex = 0.6)


# The effect of large sample:
n = 100; x = 20 # Note that MLE of p is still 0.2
a = 5; b = 4

prior = dbeta(p_grid, a, b)
posterior = dbeta(p_grid, a+x, b+n-x) # using the formula from lectures
plot(p_grid , prior, type = "l", xlab= expression(theta) , ylab="prior probability" , ylim =c(0, 10))
lines(p_grid, posterior, col = "red")
abline(v = x/n, lty = 2)
mtext("a = 5, b = 4, x = 20, n = 100", cex = 0.5)
legend(0.35, 6.2, legend=c("Prior", "Posterior", "Sample Mean"), col=c("black", "red", "black"), lty= c(1,1,2), cex = 0.6)


par(mfrow=c(1,1))

# HPDI example
# install.packages("HDInterval")
library(HDInterval)
hdi(qbeta, 0.95, shape1 =2, shape2 = 5)

# Notice this function is very flexible and also works on a sample from the posterior:
tst <- rbeta(1e5, 2, 5)
hdi(tst)


# Example 2: School study time

x = scan("http://www2.stat.duke.edu/~pdh10/FCBS/Exercises/school1.dat")

sigma.sq = 16

(x.bar = mean(x))
n = length(x)

prior.mean = 7.5
prior.var = 1^2

(post.var = 1/(1/prior.var + n/sigma.sq))
(post.mean = post.var*(prior.mean/prior.var + sum(x)/sigma.sq))


grid = seq(4, 12, len = 100)
hist(x, freq = F, ylim = c(0, 0.7), breaks=10, main = "Data, prior and posterior")
lines(grid, dnorm(grid, prior.mean, sqrt(prior.var)), type = "l", col = "blue", ylim = c(0, 0.7),
     xlab = "x", ylab = "Densities")

lines(grid, dnorm(grid, post.mean, sqrt(post.var)), col = "red")
legend(11, 0.6, legend=c("Prior", "Posterior"),
       col=c("blue", "red"), lty=1, cex=0.8)



# What to do when sigma is not known and we want a prior on it?
# We will learn how to estimate any posterior with RStan.


# Important!
# For next time: you need to have Rstan installed
# read https://mc-stan.org/users/interfaces/ 
# for example:
install.packages(c('devtools','coda','mvtnorm'))
library(devtools)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# Optional
install_github("rmcelreath/rethinking")

# Check:
library(rethinking)