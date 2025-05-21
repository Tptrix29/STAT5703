# Density curves of various univariate normal distributions:
x = seq(-5, 5, len= 1000)
plot(x, dnorm(x), type = "l", ylim = c(0, 0.6), ylab = "Normal Density")
lines(x, dnorm(x, m = -1), col = "red")
lines(x, dnorm(x, m = 1), col = "green")
lines(x, dnorm(x, sd = 1.5), col = "blue")
lines(x, dnorm(x, sd = 0.75), col = "gray54")

legend(-4.6, 0.55, legend=c("N(0,1)", "N(-1,1)", "N(1,1)", 
                           parse(text=paste("N(0, 1.5^2)")), parse(text=paste("N(0, 0.75^2)"))), 
       col=c("black", "red", "green", "blue", "gray54"), lty = 1, cex=0.6, y.intersp = 0.8)

# Density curves of various t distributions:
x <- seq(-5, 5, len= 200)
plot(x, dnorm(x), type = "l", ylim = c(0, 0.5), ylab = "t densities")
lines(x, dt(x, 3), col = "purple")
lines(x, dt(x, 11), col = "green")
lines(x, dt(x, 24), col = "blue")

legend(-4, 0.45, legend=c("N(0, 1)", "df = 3", "df = 11", "df = 24"), 
       col=c("black", "purple", "green", "blue"), lty = 1, cex=0.8)

# Density curves of various log-normal distributions:
x = seq(0, 8, len = 1000) # grid on the horizontal line
plot(x, dlnorm(x, 0, 1), type = "l", ylim = c(0, 1.5), ylab = "Log Normal densities")
lines(x, dlnorm(x, 1, 0.5), col = "purple")
lines(x, dlnorm(x, 0, 2), col = "red")
legend(4, 1, legend=c("mu = 0, sigma = 1", "mu = 1, sigma = 0.5", "mu = 0, sigma = 2"), 
       col=c("black", "purple", "red"), lty = 1, cex=0.8)


# Bivariate normal example
# Create the grid first
x <- seq(-3, 3, len = 100)
y <- seq(-3, 3, len = 100)

# Standard bivariate normal
z1 <- (1/(2*pi))*exp(-outer(x^2, y^2, "+")/2) # outer function avoids writing loops

p = par() # save graph parameters
# Make plot area square
par(pty = "s")
contour(x,y,z1)
image(x,y,z1)
persp(x, y, z1, theta = 20, phi = 40, expand = 0.5, col = "lightblue")

# Or with package rgl
# install.packages("rgl")
library("rgl")
persp3d(x,y,z1)

# Any bivariate normal (notice it is slower because of the two loops)
x <- seq(-8, 8, len = 100)
y <- seq(-4, 4, len = 100)

# S is the covariance matrix. Make sure it has a positive determinant and positive diagonal elements!
S = matrix(c(11, 4, 4, 2), 2, 2)
z2 <- matrix(rep(0, 100^2), 100, 100)
d <- det(S)
for (i in 1:100)
  for (j in 1:100)
    z2[i, j] <- (1/(2*pi*sqrt(d)))*exp(-(t(c(x[i], y[j]))%*%solve(S)%*%c(x[i], y[j]))/2)

contour(x,y,z2)
persp(x,y,z2, theta = 10, phi = 30, expand = 0.5, col = "lightblue")

library(MASS)
# Simulate bivariate normal data
mu <- c(0,0)                         # Mean
(Sigma <- S)  # Covariance matrix
contour(x,y,z2)

# Generate sample from N_2(mu, Sigma)
bivn <- mvrnorm(500, mu = mu, Sigma = Sigma )  # from Mass package
head(bivn)
points(bivn, col = "red", cex = 0.5)

library(car)
dataEllipse(bivn[,1], bivn[,2], levels=c(0.95))

# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50) 

image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = T)     # from base graphics package
contour(x,y,z2, add = T, col = "gray")
par = p # restore graph parameters
# Notice each marginal distribution is normal (as expected by the theory):
hist(bivn[,1])
hist(bivn[,2])

# QQ plot
qqnorm(bivn[,1])
qqline(bivn[,1])

# install.packages("UsingR")
library(UsingR)
data(galton)
plot(galton)
sunflowerplot(galton)