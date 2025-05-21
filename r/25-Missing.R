## Missing data

# Simulation study to show bias of imputing the mean

x = rgamma(1000, 2, 1/2)
hist(x, breaks = 15)
mis = sample(1000, 100)
x.mis = x
x.mis[mis] = NA


mean.imp = function (a)
{
  missing = is.na(a) # missing data indexes
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  # Output the imputed vector
  return (imputed)
}

x.imp = mean.imp(x.mis)
hist(x.imp, breaks = 15)


# Example 2: Missing mechanisms and regression:
n = 200
x = rnorm(n)
y = 2*x + rnorm(n, 0.6)
par(mfrow = c(1,1))
plot(x,y, main = "MCAR")
abline(lm(y~x), lwd = 2)
miss = sample(n, n/4)
points(x[miss], y[miss], col = "red", pch = 16)
abline(lm(y[-miss]~ x[-miss]), col = "red")

plot(x,y, main = "MAR")
abline(lm(y~x), lwd = 2)
miss = x > 1
points(x[miss], y[miss], col = "red", pch = 16)
abline(lm(y[!miss]~ x[!miss]), col = "red")

plot(x,y, main = "MNAR")
abline(lm(y~x), lwd = 2)
miss = y > 3
points(x[miss], y[miss], col = "red", pch = 16)
abline(lm(y[!miss]~ x[!miss]), col = "red")


# Example 3: mice package
# Generate MAR data from airquality

library(mice)

# First get just the fully observed data
air.miss = airquality[, 3:4]
set.seed(123)
a = ampute(air.miss)

# Put it back together with the other two variables
air.miss = cbind(airquality[, 1:2], a$amp)

# Check % missing
n = nrow(airquality)
sum(is.na(air.miss$Ozone))/n
sum(is.na(air.miss$Solar.R))/n
sum(is.na(air.miss$Temp))/n
sum(is.na(air.miss$Wind))/n

summary(air.miss)


# Using package mice

# Check the missing data pattern
md.pattern(air.miss, rotate.names = T)


# More details
flux(air.miss)

# For two variables with the same proportion of missing data, 
# the variable with higher influx is better connected to 
# the observed data, and might thus be easier to impute.

# For two variables having the same proportion of missing data, 
# the variable with higher outflux is better connected to 
# the missing data, and thus potentially more useful 
# for imputing other variables.

# Impute missing data:
imp = mice(air.miss, seed = 1) # Note the default is 5 iterations and 5 chains!
summary(imp)

# To change number of iterations use maxit = 
# To change the number of chains use m = 

# Check prediction methods:
imp$method

# Research question: predict Ozone with Wind, Temp and Solar.R
fit = with(imp, lm(Ozone ~ Wind + Temp + Solar.R))
fit

# Pool together the results
pool(fit)
# ubar represents the mean of the variances within each imputation 

# riv is the relative increase in variance due to nonresponse r
# RIV = (V.B+V.B/)m/V.W
# This value can be interpreted as the proportional increase in the sampling variance of the parameter 
# of interest that is due to the missing data.


summary(pool(fit))
# You can change the number of imputed datasets with 
imp2 = mice(air.miss, m = 30, maxit = 10, seed = 1)
# Check FMI again

fit = with(imp2, lm(Ozone ~ Wind + Temp + Solar.R))
pool(fit)
summary(pool(fit))

# To obtain just the fmi:
pool(fit)$pooled$fmi

# To check the rule of thumb that fmi/m should be less than 1%:
pool(fit)$pooled$fmi/30
# Could be a bit better, but ok.

# To check imputed data:
# imp2$imp

# To obtain the complete data:

completedData <- complete(imp2,1)
completedData

# To check imputed values
head(imp2$imp$Ozone)


# We can get some seriously cool plots as well
plot(imp2)

library(lattice)
# Produces a conditional scatterplots. 
# The function automatically separates the observed (blue) and imputed (red) data.
xyplot(imp, Ozone~Wind+Temp+Solar.R, pch=18)

# Density estimates of observed and imputed:
densityplot(imp)
# hese kind of plots are usually used to see whether the multiple imputations lead to similar imputed values 

# Dotplots of observed and imputed:
stripplot(imp, pch=20)
# In ideal circumstances imputed (magenta) should have similar shape to observed (blue)

# Package VIM has very advanced graphs
# install.packages("VIM")
library(VIM)
matrixplot(air.miss)



# Example 4: Using the MI package
library(mi)

# Getting the built-in dataset nlsyV
# National Longitudinal Survey of Youth
data(nlsyV, package = "mi")

mdf = missing_data.frame(nlsyV) # Create the missing data frame object

# Histograms of all variables with missing values
hist(mdf)

# Missing patterns
# Standardized observed values together with missing
image(mdf)

# Or look at the patterns numerically:
data.frame(tabulate(mdf@patterns), levels(mdf@patterns) )
# For example, 172 cases had "nothing" missingness pattern
# 61 cases had only momrace missing ...

# Examine the default settings
show(mdf)

# Make changes to some variables as an example
mdf = change(mdf, y = c("momrace"), what = "type", to = "un")
show(mdf)
# Now momrace is nominal type

# Running the chains
imputations = mi(mdf)
# The default is just 30 iterations per chain.

(converged = mi2BUGS(imputations))
# Note the result is an array of matrices with all imputed variables means and sd's
# Each matrix is of order iter by m

# Extract specific variables from the imputations
(mean_ppvtr.36 = converged[, , 1])
mean_income = converged[, , 3]

# Switch back to 1x1 graph window
par(mfrow = c(1,1))

# Traceplot of mean imputed values of  ppvtr.36
ts.plot(mean_ppvtr.36[,1], col=1, ylim = c(-0.03, 0.03))
lines(mean_ppvtr.36[,2], col= 2)
lines(mean_ppvtr.36[,3], col= 3)
lines(mean_ppvtr.36[,4], col= 4)

# Traceplot of mean imputed income
ts.plot(mean_income[,1], col=1, ylim = c(-0.05, 0.05))
lines(mean_income[,2], col= 2)
lines(mean_income[,3], col= 3)
lines(mean_income [,4], col= 4)

# We can also check if the mean of each completed variable is roughly the same for each of the 4 chains.
round(mipply(imputations, mean, to.matrix = TRUE), 3)

# R-hat should be around 1
Rhats(imputations)

# Checks how good the models fit
plot(imputations)
# Turn off the "Hit <Return>" message
par(ask=F)
par(mfrow = c(1,1))
# Conclusion: Income variable clearly has a problem!

# How do we fix the problem with income?
# Look at income again
hist(nlsyV$income)
# It is highly right-skewed and has pileup at 0

# Make changes to income variable
mdf <- change(mdf, y = "income", what = "type", to = "nonn")
show(mdf)
# Note the log transformation on income!

# Check histograms again
hist(mdf)
# Now income looks more promising and symmetric

# Run the chains again and we can always benefit from more iterations:
imputations <- mi(mdf, n.iter = 60)
Rhats(imputations)
converged <- mi2BUGS(imputations)

mean_income = converged[, , 3]
par(mfrow = c(1,1))
# Traceplot of mean imputed income
ts.plot(mean_income[,1], col=1)
lines(mean_income[,2], col= 2)
lines(mean_income[,3], col= 3)
lines(mean_income [,4], col= 4)

plot(imputations)

# Try pmm
mdf = missing_data.frame(nlsyV)


mdf <- change(mdf, y = c("momrace"), what = "type", to = "un")
show(mdf)
mdf <- change(mdf, y = "income", what ="imputation_method", to = "pmm")
show(mdf)
imputations <- mi(mdf, n.iter=60, parallel = F)


converged <- mi2BUGS(imputations)

mean_ppvtr.36 = converged[, , 1]
mean_income = converged[, , 3]

# Traceplot of mean income
par(ask=F)
par(mfrow = c(1,1))

ts.plot(mean_income[,1], col=1)
lines(mean_income[,2], col= 2)
lines(mean_income[,3], col= 3)
lines(mean_income [,4], col= 4)

plot(imputations)

# Final step: pool the imputations together
analysis <- pool(ppvtr.36 ~ first + b.marr + scale(income) + momage + momed + momrace, imputations, m=5)
display(analysis)

# Compare to glm results:
summary(glm(ppvtr.36 ~ first + b.marr + scale(income) + momage + factor(momed) + factor(momrace), 
            family = gaussian, data = nlsyV))
# Note: 228 observations deleted due to missingness
# SE are larger