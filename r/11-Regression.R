# Multiple regression example 1 from ISLR, p. 72:

# We have data for the amount of money spent advertising on the radio, TV,  and in newspapers,
# and we want to know whether any of these three media is associated with sales.
data = read.csv("11-Advertising.csv")
head(data)

attach(data)
par(mfrow = c(2, 2)) # Splits the graph area in 2x2 grid
hist(Sales, main = "Sales in thousands units")
plot(TV, Sales)
plot(Radio, Sales)
plot(Newspaper, Sales)
# Some possible violation of constant variance assumption
par(mfrow = c(1, 1))


# One option is to run three separate simple linear regressions, each of
# which uses a diﬀerent advertising medium as a predictor. 
m1.1 = lm(Sales ~ TV, data = data)
summary(m1.1)

m1.2 = lm(Sales ~ Radio, data = data)
summary(m1.2)
# We find that a $1,000 increase in spending on radio advertising is
# associated with an increase in sales by around 203 units, on average.

m1.3 = lm(Sales ~ Newspaper , data = data)
summary(m1.3)
# A $1,000 increase in newspaper
# advertising budget is associated with an increase in sales by approximately 55 units

# The following takes a lot of time to install the package, so try at home!
# install.packages("rockchalk") 

# Multiple regression
m1.4 <- lm(Sales ~ TV + Radio , data = data)

# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(TV, Radio, Sales)

library(rockchalk)
plotPlane(m1.4, plotx1 = "TV", plotx2 = "Radio")

# Manual computations with linear algebra using all predictors:
y = Sales # Vector of response variable
X = cbind(rep(1, nrow(data)), TV, Radio, Newspaper) # the design matrix X
# Or use
model.matrix(~ TV + Radio + Newspaper)


# Matrices that occur in the formulas
solve(t(X)%*%X) 

# Finally, the estimated slope and intercept
(b = solve(t(X) %*% X) %*% t(X) %*% y)

# Interpretation of b1:
# For every $1000 increase in TV advertising, the sales 
# increase by 46 units, keeping radio and newspaper budget fixed.

# Interpretation of b2:
# For every $1000 increase in Radio advertising, the sales 
# increase by 189 units, keeping TV and newspaper budget fixed.

# Note about b3:
# The coefficient for newspaper is negative and not significant.
# Reason: predictors are correlated
cor(data)
# So newspaper sales are a surrogate for radio
# advertising; newspaper gets “credit” for the eﬀect of radio on sales.

# Check and compare to built-in lm function
reg = lm(Sales ~ TV + Radio + Newspaper)
summary(reg)

# Fitted values y-hat manually
(y.hat = X %*% b)
# Alternatively with built-in function:
predict(reg)

# Residuals
(e = y - y.hat)

# Compare to built-in function
resid(reg)

# Residual plot for checking linearity and constant variance assumptions 
plot(reg, which = 1, main = "Standardized Residuals Plot")
# Seems the relationship is not quite linear

# Better approach 
library(ggplot2)
ggplot(data=data.frame(e,y.hat), aes(x=y.hat, y=e))+geom_point()+  stat_smooth()
# The curve should stay around the x axis to indicate linearity is ok

# You can also check against specific predictors
plot(TV, e)
plot(Radio, e)
plot(Newspaper, e)

# Checking for interaction effects
plot(TV*Radio, e)
# If you see a pattern which deviates from the horizontal axis, then include the interaction!

# Checking normality
par(mfrow = c(2, 2))
boxplot(e)
hist(e)
qqnorm(e)
qqline(e)
# Possible normality violation, so check with tests:
ks.test(rstandard(reg), "pnorm")
shapiro.test(rstandard(reg))
# Both of these reject the normality.
par(mfrow = c(1, 1))
# Conclusion: the model could be improved!

# ANOVA computations
t(y) %*% y
(SSE = t(y)%*%y -t(b)%*%t(X)%*%y)
# Check with built-in function (R calls this SSResiduals)
anova(reg)

# Total SS
n = nrow(data)
J = matrix(rep(1,n^2), n, n)
(SSTO = t(y)%*%y - t(y)%*%J%*%y/n)

# Alternatively:
sum(anova(reg)[,2])

(MSE = SSE/(n-ncol(data)))

# To get overall F-test and R-squared

summary(reg)
# F-test statistic = 570.3
# p-value < 2.2e-16
# Reject H0: beta1 = beta2 = beta3 = 0

# Test the significance of each predictor separately:
# Ho: beta1 = 0
# p-value <2e-16
# Conclusion: TV is a significant predictor, even when the other 2 are included in the model

# Ho: beta2 = 0
# p-value <2e-16
# Conclusion: Radio is a significant predictor, even when the other 2 are included in the model

# Ho: beta3 = 0
# p-value = 0.86
# Conclusion: Newspaper is not a significant predictor, when the other 2 are included in the model


# Confidence intervals for betas
confint(reg)
# Can also be obtained manually with the t distribution
# Manual 95% CI for beta1:
n = nrow(data)
coef(reg)[2] - qt(0.975, n - 3)*sqrt(diag(s.sq.b))[2]
coef(reg)[2] + qt(0.975, n - 3)*sqrt(diag(s.sq.b))[2]


# Variances and covariances of the coefficients b:
(s.sq.b = drop(MSE)*solve(t(X)%*%X))

# St. errors of the coefficients
sqrt(diag(s.sq.b))
# Note they match the lm output SE

summary(reg)$r.squared
# R-sq = 0.89721
# That is, 89.72% of sales variation is explained by the regression model

# Alternative formula
1 -SSE/SSTO

# Prediction at specific values.
# Say, the company will invest $70,600 in TV, $17,000 in Radio and $36,800 in newspaper advertising
x.new = c(1, 70.6, 17, 36.8)
s.sq.new = t(x.new)%*%s.sq.b%*%x.new
s.sq.new
# Manual 95% CI for mean response:
coef(reg)%*%x.new - qt(0.975, 18)*sqrt(s.sq.new)
coef(reg)%*%x.new + qt(0.975, 18)*sqrt(s.sq.new)

# Or with built-in function
predict(reg, data.frame(TV=70.6, Radio= 17, Newspaper = 36.8), interval = "conf")

# 95% PI
predict(reg, data.frame(TV=70.6, Radio= 17, Newspaper = 36.8), interval = "predict")


# Adding interaction effect:
# Let's drop newspaper and add interaction effect between TV and radio
# Why is this a good idea?
# In this situation, given a fixed budget of $100,000, spending half on radio
# and half on TV may increase sales more than allocating the entire amount
# to either TV or to radio. In marketing, this is known as a synergy eﬀect,
# and in statistics it is referred to as an interaction eﬀect.

reg2 = lm(Sales ~ TV*Radio, data = data)
summary(reg2)

# The results strongly suggest that the model that includes the
# interaction term is superior to the model that contains only main eﬀects. 
# 1) The p-value for the interaction is highly significant!
# 2)Also the R-squared has improved a lot.

# We can interpret β3 as the increase in the eﬀectiveness of TV advertising
# for a one unit increase in radio advertising (or vice-versa).

# Checking normality
plot(reg2, which = 2)
# Did not help with normality

# Checking linearity:
plot(reg2, which = 1)
# A bit better, but probably a non-linear model might improve this (next lecture)


# Example 2: Dummy variables
# install.packages("ISLR")
library(ISLR)
data(Credit)
head(Credit)
m3.1 = lm(Balance ~ Gender, data = Credit)
summary(m3.1)

Credit$Gender2 = 2*(as.integer(Credit$Gender)-1)-1
m3.2 = lm(Balance ~ Gender2, data = Credit)
summary(m3.2)

m3.3 = lm(Balance ~ Ethnicity, data = Credit)
summary(m3.3)

m4 = lm(Balance ~ Income + Student, data = Credit)
summary(m4)

library(ggplot2)
df = cbind(Credit, pred = predict(m4))
# Plot parallel lines model
plot <- ggplot(data = df, aes(x = Income, y = Balance, colour = factor(Student)))
plot +   geom_line(mapping=aes(y=pred))+ geom_point()

# Plot non-parallel lines model
plot <- ggplot(data = df, aes(x = Income, y = Balance, colour = factor(Student)))
plot +   stat_smooth(method=lm, se = F) + geom_point()