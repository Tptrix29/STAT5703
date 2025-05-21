# Time Series Part 2

# From last time:
# Example 1:GNP
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

# Fit: MA(2)
(gnp.ma= arima(diff(log(gnp)), order = c(0,0,2)))

# Fit AR(1)
arima(diff(log(gnp)), order = c(1,0,0))

ARMAtoMA(ar=.35, ma=0, 10)
tsdiag(gnp.ma, gof.lag=20)


# Example 2: random walk and how it (should) return to the origin
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

# Example of differencing:

library(astsa)
data(chicken)
par(mfrow=2:1)
tsplot((chicken), col=4, main="Poultry, Whole bird spot price, Georgia docks, US cents per pound" )
tsplot(diff(chicken), col=4, main="first difference")

par(mfrow = c(3,1))
acf1(chicken, col=6, lwd=2)
acf1(detrend(chicken), col=3, lwd=2)
acf1(diff(chicken), col=4, lwd=2)

# Example: Lake Huron water level
ts.plot(LakeHuron) 

# Manual extraction of the trend
x = 1875:1972
r = resid(lm(LakeHuron ~ x))
par(mfrow = c(1,2))
acf(r)
pacf(r)
x.lag1 <- r[1:(length(r)-1)]
x.c = r[-1]
r2 = resid(lm(x.c ~ x.lag1))

arima(r, order =c(1,0,0), method = "CSS") 
# CSS = conditional sum of squares is an alternative to MLE
# dealing with model involving MA factors is not easy to estimate, since there are past values of errors to be computed recursively.

acf(r2)
pacf(r2)
par(mfrow = c(1,1))


# Example:
df <- read.csv('https://raw.githubusercontent.com/ourcodingclub/CC-time-series/master/monthly_milk.csv')
df$month = as.Date(df$month)
head(df)
plot(df$month, df$milk_prod_per_cow_kg,type = "l")

# Convert to time series object
df.ts = ts(df[, -1], frequency = 12, start=c(1962, 1, 1))
head(df.ts)
plot(df.ts)

# Returns suitably lagged and iterated differences.
diff.ts = diff(df.ts)
plot(diff.ts)
# Trend is removed

# You can try 2 times differencing
diff.ts2 = diff(df.ts, differences = 2)
plot(diff.ts2)

# Removing trend and seasonal component:
diff.ts3=diff(diff.ts, lag=12)
plot(diff.ts3)
pacf(diff.ts3)



# Example 2
# Monthly Airline Passenger Numbers 1949-1960
data(AirPassengers)
data = data.frame(AirPassengers)
data

# Transform to time series
ts.data1 = ts(data=as.vector(t(data['AirPassengers'])), start = c(1949), end = c(1960), frequency=12)

# Plot seasonality, trend
plot(ts.data1)
plot(stl(ts.data1, "periodic"))
# stl is Seasonal Decomposition of Time Series by Loess

decomposed <- stl(ts.data1, s.window="periodic") # either the character string "periodic" or the span (in lags) of the loess window for seasonal extraction
seasonal <- decomposed$time.series[,1]
trend   <- decomposed$time.series[,2]
remainder <- decomposed$time.series[,3]

# Show seasonal effect
seasonal

# Deseasonalize time series
ts.data1 <- ts.data1 - seasonal
ts.data1
plot(ts.data1)

# Alternative:
library(forecast)
library(ggplot2)

# Seasonal Decomposition of Time Series by Loess
decomp = stl(log(AirPassengers), s.window = "periodic")
ap.sa <- exp(seasadj(decomp))
autoplot(cbind(AirPassengers, SeasonallyAdjusted=ap.sa)) +
  xlab("Year") + ylab("Number of passengers (thousands)")

pacf(remainder)

# Example 3:
# Total annual rainfall in inches for London, from 1813-1912 
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
# The output contains the estimated alpha and the overall trend coefficient

rainseriesforecasts$fitted
plot(rainseriesforecasts)

library(forecast)

# Forecast next 8 years:
(rainseriesforecasts2 = forecast(rainseriesforecasts, h=8))
plot(rainseriesforecasts2)

# Check if residuals are white noise
acf(rainseriesforecasts2$residuals[-1], lag.max=20) # remove first NA
Box.test(rainseriesforecasts2$residuals, lag = 20, type="Ljung-Box")
# Conclusion: residuals are uncorrelated


# Example 4:
# Exponential smoothing for time series with a trend:
# annual diameter of women's skirts at the hem, from 1866 to 1911
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)

skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts

plot(skirtsseriesforecasts)
 
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts2)
 
# Auto ARIMA
(fit = auto.arima(skirtsseries))
forecast(fit, 10)
plot(forecast(fit, 20))


# Testing for independence
x <- rnorm(100)
Box.test(x) # null hypothesis is independence
Box.test (x, lag = 2, type = "Ljung")
# Both conclude that the data do not have autocorrelation


# Example: Simulate ARCH data:
n = 300 # number of time periods
x = 1:n # initial x values
sigma = numeric() # simulated st. dev.s
x[1] = 0 # starting point
b0 = 5
b1 = 0.5
sigma[1] = b0

for (i in 2:n)
{
  sigma[i] = sqrt(b0 + b1*x[i-1]^2)
  x[i] = sigma[i]*rnorm(1)
}
# Note you can also use garchSim from fGarch package

plot(x, type = "l", main = "ARCH(1)")
acf(x) # Looks like white noise, because the autocovariances of ARCH models are all 0


# Simulate GARCH data:
n = 300
x = 1:n
sigma = 1:n
x[1] = 0
sigma[1] = 1
b0 = 5
b1 = 0.5
d1 = 0.5

for (i in 2:n)
{
  sigma[i] = sqrt(b0 + b1*x[i-1]^2 + d1*sigma[i-1]^2)
  x[i] = sigma[i]*rnorm(1)
}

plot(x, type = "l")
acf(x) # Again, looks almost like white noise

par(mfrow = c(1, 2))
acf(x^2) 
pacf(x^2)
# Both taper suggesting ARMA model
par(mfrow = c(1, 1))

library(fGarch)
y = x - mean(x) #center x 
x.g = garchFit(~garch(1,1), y, include.mean=F)
summary(x.g)
# The fGarch summary provides the Jarque Bera Test for the null hypothesis 
# that the residuals are normally distributed and the familiar Ljung-Box Tests. 
# Ideally all p-values are above 0.05.