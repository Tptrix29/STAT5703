## Time Series

# Example 1
# Australian red wine sales, January 1980 to October 1991 in 1000s of liters
# Read file AusWineSales.csv
wine = read.csv(file.choose(), header = T)
head(wine)

plot(wine$Red, type = "l")
ts.plot(wine$Red)

wine1 = ts(wine[,2:4], start = c(1980, 1), frequency = 12)
plot(wine1,main="Australian wine sales, January 1980 to October 1991")

# Showing the components using moving averages
library(forecast)
wineComp <- decompose(wine1[,1])
# Decompose a time series into seasonal, trend and irregular components 
# using moving averages.
autoplot(wineComp)


# Example 2
str(USAccDeaths)
plot(USAccDeaths)


# Example 3: White noise:
n = 1000
par(mfrow=c(2,1))
x = rnorm(n)
ts.plot(x, main = "Normal(0, 1)")
b = rbinom(n, 6, 0.5) -3
ts.plot(b, main = "Binom(6, 0.5) - 3")
par(mfrow=c(1,1))


# Example 4: simulated time series data
par(mfrow=c(3,3)) # Split the graph window 3x3
yt <- arima.sim(list(order=c(0,0,0)), n=300)
plot(yt, main = "White Noise")
acf(yt, main = "White Noise ACF")
pacf(yt, main = "White Noise PACF")
yt <- arima.sim(list(order=c(1,0,0), ar= 0.8), n=300)
plot(yt, main = "AR(1)")
acf(yt, main = "AR(1) ACF")
pacf(yt, main = "AR(1) PACF")
yt <- arima.sim(list(order=c(2,0,0), ar= c(0.6, 0.3)), n=300)
plot(yt, main = "AR(2)")
acf(yt, main = "AR(2) ACF")
pacf(yt, main = "AR(2) PACF")
par(mfrow=c(1,1))

# Back to Australian wine example
par(mfrow=c(1,2))
acf(wine$Red) # Premature ACF plot
pacf(wine$Red)
# Not so fast: the data were not detrended and deseasoned:
acf(wineComp$random[!is.na(wineComp$random)], main = "Red wine ACF")
pacf(wineComp$random[!is.na(wineComp$random)], main = "Red wine PACF")
par(mfrow=c(1,1)) # Back to 1x1 graph window


# Example 4:
# Simulate ARMA models:
par(mfrow=c(2,2))
yt <- arima.sim(list(order=c(0,0,0)), n=300)
plot(yt, main = "White Noise")
yt <- arima.sim(list(order=c(1,0,0), ar= 0.7), n=300)
plot(yt, main = "AR(1)")
yt <- arima.sim(list(order=c(0,0,1), ma = 0.7), n=300)
plot(yt, main="MA(1)")

yt <- arima.sim(list(order=c(1,0,1), ar= 0.7, ma = 0.7), n=300)
plot(yt, main="ARMA(1,1)")
par(mfrow=c(1,1))

# Exercise: check the acf and pacf of each.

# Wine data estimated:
arima(wine$Sparklin, order = c(1,1,1))
arima(wine$Sparklin, order = c(1,0,1))

acf(resid((arima(wine$Sparklin, seasonal = list(order = c(1,1,1), period = 12)))))


# Example:GNP
# install.packages("astsa")
library(astsa)
data(gnp)
plot(gnp)
acf(gnp, lag.max = 50)
plot(diff(gnp))
plot(diff(log(gnp)))
par(mfrow = c(2,1))
acf(diff(log(gnp)))
pacf(diff(log(gnp)))
par(mfrow = c(1,1))

(gnp.ma= arima(diff(log(gnp)), order = c(0,0,2)))


arima(diff(log(gnp)), order = c(1,0,0))

ARMAtoMA(ar=.35, ma=0, 10)
tsdiag(gnp.ma, gof.lag=20)

# Example of random walk and how it (should) return to the origin
RW <- function(N, x0, mu, variance) {
  z<-cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t<-1:N
  x<-x0+t*mu+z
  return(x)
}
# mu is the drift

P1 <- RW(100000,0,0,1)
plot(P1, main="Random Walk without Drift", xlab="t",ylab="x", type='l')
abline(h=0)