# Drawing polynomials on a grid:
x = seq(-3, 3, len = 100)
par(mfrow = c(2,2))
plot(x, 5*x^2 - 8*x - 5, type = "l", ylab = "y", main = expression(5*x^2 - 8*x - 5))
plot(x, (-7)*x^2 + 3*x + 9, type = "l", ylab = "y", main = expression(-7*x^2 + 3*x + 9))
plot(x, 4*x^3 + 4*x^2  - 9*x + 5, type = "l", ylab = "y", main = expression( 4*x^3 + 4*x^2  - 9*x + 5))
plot(x, (-9)*x^3 + 9*x^2  + 4*x + 5, type = "l", ylab = "y", main = expression(-9*x^3 + 9*x^2  + 4*x + 5))
par(mfrow = c(1,1))

# Response surface plots:
f1 = function(x, y)  x ^ 2 + y ^ 2
f2 = function(x, y)  x ^ 2 + y ^ 2 - 3*x*y
f3 = function(x, y) 5*x + 5*x ^ 2 + y ^ 2 - 3*x*y
f4 = function(x, y) 5*x + 3*y + x ^ 2 + y ^ 2 - 3*x*y

# Simulate the variables 2-D grid
x <- y <- seq(-1, 1, length = 30)
z1 = outer(x, y, f1)
z2 = outer(x, y, f2)
z3 = outer(x, y, f3)
z4 = outer(x, y, f4)

# plot the 3D surface
par(mfrow = c(2,2))
persp(x, y, z1, main = expression(x^2 + y^2))
persp(x, y, z2,  main = expression(x^2 + y^2- 3*x*y))
persp(x, y, z3, main = expression( 5*x + 5*x ^ 2 + y ^ 2 - 3*x*y))
persp(x, y, z4, main = expression(5*x + 3*y + x ^ 2 + y ^ 2 - 3*x*y))
par(mfrow = c(1,1))

# Example 1: polynomial regression with Boston data set
library(tidyverse)
#install.packages("caret")
library(caret)
theme_set(theme_classic())

# Load the data
data("Boston", package = "MASS")
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE) # from caret package
# A series of test/training partitions are created
# You can also select randomly 80% of the rows with the sample command

train.data  = Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Build the model on the training data
reg2 = lm(medv ~ poly(lstat, 2, raw = TRUE), data = train.data)

# Make predictions on the test data
predictions <- reg2 %>% predict(test.data)

# Model performance
modelPerfomance = data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = caret::R2(predictions, test.data$medv) # from caret
)

print(lm(medv ~ lstat + I(lstat^2), data = train.data))
print(modelPerfomance)

reg5 <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Make predictions
predictions <- reg5 %>% predict(test.data)
# Model performance
data.frame(RMSE = RMSE(predictions, test.data$medv),
           R2 = R2(predictions, test.data$medv))

ggplot(train.data, aes(lstat, medv) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))

# Which one is better?
anova(reg2, reg5)
# Conclusion: there is significant difference and 5th degree has lower RMSE


# Example 2:  dummy variable
# We’ll use the Salaries data set [car package], which contains 2008-09 nine-month academic salary
# for Assistant Professors, Associate Professors and Professors in a college in the U.S.

# Load the data
library(car)
data("Salaries", package = "carData")
# write.csv(Salaries,"12-Salaries.csv", row.names = FALSE)
library(tidyverse)
# Inspect the data
sample_n(Salaries, 3)

# If the categorical variable is not a factor, you need to convert it first.
# Compute the model
model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef
# Fitted function:
# Yhat = 101002.41 +14088.01*sex

# From the output above, the average salary for female is estimated to be 101002, 
# whereas males are estimated a total of 101002 + 14088 = 115090. 
# The p-value for the dummy variable sexMale is very significant, suggesting that 
# there is a statistical evidence of a difference in average salary between the genders.

# The contrasts() function returns the coding that R have used to create the dummy variables:
contrasts(Salaries$sex)

# How about adding a numerical predictor?
model2 <- lm(salary ~ yrs.service +  sex, data = Salaries)
summary(model2)

# Now with interaction:
model3 = lm(salary ~ yrs.service *sex, data = Salaries)
summary(model3)
# Rule: when using interaction term always include the individual terms as well


# Plot:
library(ggplot2)

plot <- ggplot(data = Salaries, aes(x = yrs.service, y = salary, colour = factor(sex)))
plot + stat_smooth(method=lm, se = F) + geom_point()

# Exercise: Write down the two separate regression equations from the interaction model
# Female: y = 
# Male: y = 


# Example 3: model selection
# Read data from the file 12-rowtime.csv
rowtime = read.csv(file.choose())

summary(rowtime)

# Regression with all predictors:
summary(lm(racetime~ ., data = rowtime))

round(cor(rowtime), 2)
pairs(rowtime[,1:18])
# Shows many highly correlated predictors.

require(leaps)
# Perform model selection by exhaustive search
allmods = regsubsets(racetime~., nbest=1, nvmax= 9, data=rowtime)
# The model that minimizes the SSE is picked for each number of predictors

summary(allmods) # get summary of best subsets
summary(allmods)$adjr2 #adjusted R^2 for some models
#par(mar=c(4,1,0,1)+0.1)
plot(allmods, scale="adjr2",main="")

racetime=rowtime$racetime
d = cbind(rowtime[,-1], racetime)
library(bestglm)
# Best subset selection with timing:
st <- Sys.time()
bs3 <- bestglm(Xy = d, family = gaussian,
               IC = "BIC")
en <- Sys.time()
(tm <- en - st)
bs4 <- bestglm(Xy = d, family = gaussian, IC = "AIC")

plot(0:(dim(d)[2]-1), bs3$Subsets$BIC, type = "b", ylab = "BIC", 
     xlab = "Number of Covariates", lwd = 3, pch = 19, main = "BIC")
abline(v=which.min(bs3$Subsets$BIC)-1)

plot(0:(dim(d)[2]-1), bs4$Subsets$AIC, type = "b", ylab = "AIC", 
     xlab = "Number of Covariates", lwd = 3, pch = 19, main = "AIC")
abline(v=which.min(bs4$Subsets$AIC)-1)

summary(bs3$BestModel)
summary(bs4$BestModel)

# If bestglm is too slow, you can try stepwise methods, like backward elimination
lm.full = lm(racetime~., data=rowtime) # regress y on everything in data set
step(lm.full,direction="backward")

# Example: PLS
library(pls)
# Performs PLS which finds components from X that are also relevant for Y. 
# In contrast, PCA reduces only X and then uses some to predict Y.
# Both PCR and PLSR decompose X into orthogonal scores T and loadings P such that X = TP
# However, the objective functions of the decompositions are different between PLS and PCR

# fit model
model1 = plsr(racetime ~., data= d, scale=TRUE, validation="CV")
summary(model1)
# The RMSEP table tells us the test RMSE calculated by the k-fold cross validation.
# After the 3rd component RMSE starts increasing

# adjCV is a bias-corrected CV estimate (reference in the package documentation)

# Note the results have randomness and every time will be slightly different
# Note also that the training % variance explained always increases
validationplot(model1)
# Three components are enough

plot(model1, ncomp = 2, asp = 1, line = TRUE)
# This shows the cross-validated predictions with two components versus measured values
# The points follow the target line quite nicely, and there is no indication of a curvature or other anomalies.

plot(model1, plottype = "scores", comps = 1:3)
# This gives a pairwise plot of the score values for the three first components 
# Score plots are often used to look for patterns, groups or outliers in the data.

# You can also try PCR method:
model2 = pcr(racetime ~., data= d, scale=TRUE, validation="CV")
summary(model2)


# Extra example: look at it at home
# ECLS-K Example (Early Childhood Longitudinal Studies Kindergarten Class )
# More info about the data here:
# https://nces.ed.gov/ecls/ 

### Load the 14-eclsk_c.Rdata data by opening a browser window
load(file.choose())

### Or call the path directly (fill in your path)
# load(file = "d:\Work\Columbia\STAT5703\14-eclsk_c.Rdata")
head(eclsk_c)
dim(eclsk_c)

# Some charts:
par(mfrow = c(2,2))
hist(eclsk_c$WKSESL) # Socioeconomic status
hist(eclsk_c$RIRT) # Kindergarten reading score
hist(eclsk_c$MIRT) # Kindergarten math score
hist(eclsk_c$P1EXPECT) # Parental expectations
par(mfrow = c(1,1))

# Clean the data, remove ID numbers, which are unrelated to regression
# Select C6R4MSCL (5th grade math score) as the response variable Y
# and add it as the last column (for the purpose of bestglm package)
# Note C6 refers to data/scores collected/derived from spring fifth-grade direct child assessment

# We also remove variables which make prediction too easy.
# They are related to 5th and 4th grade results:
round(cor(eclsk_c$C6R4MSCL, eclsk_c[,-c(1,2)]), 2)

eclsk1 <- eclsk_c[, !names(eclsk_c) %in% c("S1_ID", "CHILDID", "C5R4RSCL", "C6R4RSCL", "C5R4MSCL", "C6R4MSCL")]
eclsk1 <- cbind(eclsk1, C6R4MSCL = eclsk_c$C6R4MSCL)
head(eclsk1)
# Some scatterplots
pairs(eclsk1[,3:10])
# Correlation matrix:
cor(eclsk1)

# Regression with all predictors:
summary(lm(C6R4MSCL ~ ., data = eclsk1))

# install.packages("bestglm")
library(bestglm)
# Best subset selection with timing:
st <- Sys.time()
bs3 <- bestglm(Xy = eclsk1, 
               family = gaussian,
               IC = "BIC")
en <- Sys.time()
(tm <- en - st)

bs4 <- bestglm(Xy = eclsk1, family = gaussian, IC = "AIC")

plot(0:35, bs3$Subsets$BIC, type = "b", ylab = "BIC", ylim = c(40500, 42700),
     xlab = "Number of Covariates", lwd = 3, pch = 19, main = "BIC", cex.main = 2)

summary(bs3$BestModel)
which.min(bs3$Subsets$BIC)
summary(bs4$BestModel)

### Forward selection with MASS
library(MASS)
min.model <- lm(C6R4MSCL ~ 1, data = eclsk1)
max.model <- lm(C6R4MSCL ~ ., data = eclsk1)
scp <- list(lower = min.model, upper = max.model)
fwd <- stepAIC(min.model, direction = 'forward', scope = scp)

fwd$coefficients

### Do the results match up with best subset selection?

(d1 = names(fwd$coefficients)[-1]) # Names of predictor variables without the intercept
(minAIC = which.min(bs4$Subsets$AIC)) # Which model is the best
(d2 = names(bs4$Subsets[minAIC, bs4$Subsets[minAIC,] == TRUE])[-1])

# Check if the two names sets are equivalent:
d1 %in% d2
d2 %in% d1
# Or:
sort(d1) == sort(d2)

### Forward selection with BIC (k = log(N))
fwd2 <- stepAIC(min.model,
                direction = 'forward', scope = scp,
                k = log(nrow(eclsk1)))
fwd2$coefficients

### Do the results match up with best subset selection?
d1 <- names(fwd2$coefficients)[-1]
d2 <- names(bs3$Subsets[16, bs4$Subsets[16,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1

### Backward selection with MASS
min.model <- lm(C6R4MSCL ~ 1, data = eclsk1)
max.model <- lm(C6R4MSCL ~ ., data = eclsk1)
scp <- list(lower = min.model, upper = max.model)
bwd <- stepAIC(max.model, 
               direction = 'backward', scope = scp)
bwd$coefficients

### Do the results match up with best subset selection?
d1 <- names(bwd$coefficients)[-1]
d2 <- names(bs4$Subsets[27, bs4$Subsets[27,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1

# Exercise: For backward selection with BIC change to k = log(n) and compare to AIC