# Example 1:
# Study time example from last time but with unknown population variance
x = scan("http://www2.stat.duke.edu/~pdh10/FCBS/Exercises/school1.dat")
(x.bar = mean(x))
n = length(x)

prior.mean = 7.5

(x.bar = mean(x))
n = length(x)

k0 =1
nu0 = 1
sigma.sq0 = 16
(post.mean = (k0*prior.mean + n*x.bar)/(k0+n))
(post.var = (nu0*sigma.sq0 + (n-1)*var(x) + k0*n*(x.bar-prior.mean)^2/(k0+n))/(nu0+n))

kn = k0 + n

# Plot the posterior distributions
library(invgamma)

# plot(s, dinvgamma(s, (nu0+n)/2, (nu0 + n)*post.var/2), type = "l", xlab = "sigma^2")

# Create double grid 
mu = seq(5, 15, len = 1000)
s = seq(0, 30, len = 1000)

# Joint posterior
post = matrix(0, 1000, 1000)
for (i in 1:1000) for (j in 1:1000)
  post[i,j] <- dnorm(mu[i], post.mean, sqrt(s[j]/kn))* dinvgamma(s[j], (nu0+n)/2, (nu0 + n)*post.var/2)
par(mfrow = c(1,1))
contour(mu, s, post, nlevels = 20)

# image(mu, s, post)
# persp(mu, s, post, theta = 60, phi = 30, expand = 0.5, col = "lightblue")

# Monte Carlo sampling
S = 10000
mu = 1:S
s2 = 1:S
for (i in 1:S)
{
  s2[i] = rinvgamma(1, (nu0+n)/2, (nu0 + n)*post.var/2)
  mu[i] = rnorm(1, post.mean, sqrt(s2[i]/kn))
}

library("scales")
points(mu, s2, col = alpha("red", 0.2), cex = 0.4)

hist(mu)

# posterior means:
mean(mu)
mean(s2)

# Posterior 95% CI 
quantile(mu,c(.025,.975))
quantile(s2,c(.025,.975))

# Now try with rstan:
write(
  "data {
  int n;
  real x[n]; # real vector of length n to store the data points
}

parameters {
  real mu;
  real<lower=0> sigma2;
}

model {
  for (i in 1:n)
   x[i] ~ normal(mu, sqrt(sigma2));
  mu ~ normal(7.5, sqrt(sigma2));
  sigma2 ~ inv_gamma(0.5, 8);
}",
"Example1.stan")

library(rstan)
model = stan_model("Example1.stan")
fit = sampling(model,list(n=n,x = x), iter= 5000, chains=4)
print(fit)
params = extract(fit)
hist(params$mu)
hist(params$sigma2)
quantile(params$mu, c(0.025, 0.975))
quantile(params$sigma2, c(0.025, 0.975))


# Example 2: Gibbs sampler

library(MASS)
# Means and standard deviations of the bivariate normal data
m1 <- 1; m2 <- 1
s1 <- 1
s2 <- 22

# Correlation between the two components
r <- 0.9

# Covariance matrix
s <- matrix(c(s1^2, r*s1*s2, r*s1*s2, s2^2),2,2)
m <- c(m1,m2)

# Generate draw from the bivariate normal
z <- mvrnorm(1000,m,s)
plot(z,col="grey",pch=20)

# Gibbs sampler to simulate this data
iter <- 1000
x <- 1:iter
y <- 1:iter
x[1] <- -1
for (i in 1:iter) # Gibbs loop to sample from the two conditional distributions
{
  y[i] <- rnorm(1, m2+s2*r*(x[i]-m1)/s1, sqrt((1-r^2)*s2^2))
  if (i < iter) x[i+1] <- rnorm(1, m1+s1*r*(y[i]-m2)/s2, sqrt((1-r^2)*s1^2))
}

# Compare with package generator:
plot(z,col="grey",pch=20, main = "r = 0.9")
points(x,y,col="red", pch=20)

ts.plot(x)

# Now change r to 0.999 and repeat!


# Gibbs example 2:
# Mixture model:
mu1 = -2
mu2 = 2
s1 = 0.6
s2 = 0.6
pd = c(0.5, 0.5)

# iid Monte Carlo sample
S = 10000
d = rbinom(S, 1, pd[1]) # simulating the group membership labels 0 and 1

x = 1:S
# simulate the different normal variables depending on d = 0 or d = 1:
for (i in 1:S) if (d[i] == 0) x[i] = rnorm(1, mu1, s1) else x[i] = rnorm(1, mu2, s2)

hist(x, freq = F, ylim = c(0,0.4))
grid = seq(-5, 5, len = 1000)
lines(grid, pd[1]*dnorm(grid, mu1, s1) + pd[2]*dnorm(grid, mu2, s2), lw = 2)

# Gibbs
d = 1:S
xGibbs = 1:S
for (i in 2:S)
{
  if (d[i-1] == 0) xGibbs[i] = rnorm(1, mu1, s1) else xGibbs[i] = rnorm(1, mu2, s2)
  d[i] = rbinom(1, 1, 1-pd[1]*dnorm(xGibbs[i], mu1, s1)/(pd[1]*dnorm(xGibbs[i], mu1, s1) + pd[2]*dnorm(xGibbs[i], mu2, s2)))
}
hist(xGibbs, freq = F, ylim = c(0,0.4))

lines(grid, pd[1]*dnorm(grid, mu1, s1) + pd[2]*dnorm(grid, mu2, s2), lw = 2)
table(d)
par(mfrow = c(1,2))
ts.plot(xGibbs)
ts.plot(x)

acf(xGibbs)
acf(x)

# coda for ESS computations
library(coda)

effectiveSize(xGibbs)
# Note we requested 10000 iterations, of which only 21 are approx. indep.
# The ratio of ESS/TSS should not be less than 10%
effectiveSize(xGibbs)/S

# Compare to the iid sampler:
effectiveSize(x)/S

# Exercise: Try the school example with a Gibbs sampler


# Extra Rstan examples (if we have time)
# library(devtools)
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install_github("rmcelreath/rethinking")

# Load library:
library(rethinking)

# Regression example: rugged terrain and GDP
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

# Laplace approximation:
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )
precis( m8.3 , depth=2 )

# Trim the dataset to only variables you need
dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer( dd$cid )
)
str(dat_slim)

# In case compiler does not work:
# rebuild_cmdstan()

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=4 , cores=4 )

# Summary with precis function:
precis( m9.1 , depth=2 )


stancode( m9.1 ) # If you want to check the Stan code

pairs( m9.1 ) # posterior histograms
traceplot( m9.1 ) # check convergence

trankplot( m9.1 ) 
# For healthy well-mixing chains, the histrograms should be uniform. 
# When there are spikes for some chains, especially in the low or high ranks, 
# this suggests problems in exploring the posterior.