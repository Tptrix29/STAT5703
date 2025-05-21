# Example 1: Bank marketing

library(readr) # for the read_csv2 function

# The dataset is in a CSV file with European-style formatting 
# (commas for decimal places and semi-colons for separators). 
# We'll read it with read_csv2() from the readr package. 

# Read the dataset and convert the target variable to a factor
bank_df <- read_csv2("15-bank-full.csv")
bank_df$y = ifelse(bank_df$y== "yes",1,0)

bank = as.data.frame(bank_df) # for the bestglm function
head(bank) 
# we have string variables that need to be converted to factors
bank[sapply(bank, is.character)] <- lapply(bank[sapply(bank, is.character)], as.factor)
str(bank)

bank = bank[,c(-10,-11)] # we remove some predictors because of the limit of bestglm
library(bestglm)

# Baby version 1:
logit0 = glm(y ~ balance, data = bank, family = "binomial")
summary(logit0)
plot(bank$balance, predict(logit0, type = "response"))
exp(coef(logit0)*1000)
# For each extra 1000 euros of balance the odds of obtaining the product increase by 4%

# Baby version 2:
logit0 = glm(y ~ housing, data = bank, family = "binomial")
summary(logit0)
exp(coef(logit0)) # see exercise within the lecture for interpretation

# Compare to cross-tabulated data
xtabs(~ y + housing, data = bank)

# Results
#       housing
# y      no   yes
# 0   16727 23195
# 1    3354  1935

# Manual computation of prob. of subscribing to the product within the housing = no group:
3354/(3354+16727)

# [1] 0.1670236

# Prob. of subscribing to the product within the housing = no group using the GLM model:
# logit = intercept = -1.60687
exp(coef(logit0))
# odds = 0.2005141

# Odds of subscribing within housing = yes group:
exp(sum(coef(logit0)))
# odds = 0.08342315

# Probability:
exp(coef(logit0)[1])/(1+exp(coef(logit0)[1]))
# 0.1670236 : same as the manual computation!

# Alternatively
table(predict(logit0, type = "response"))
# Note both probabilities within housing = no and housing = yes are shown

# Odds ratio
0.2005141/0.08342315
# [1] 2.403579
# Interpretation: The odds of subscribing to the product are 2.4 times higher
# within the no housing group, compared to housing=yes group.
# That is, if customer has a housing loan the odds are 2.4 times lower.

# First serious/adult approach: failed!
# bank.bestglm = bestglm(Xy = bank, family = binomial, IC = "BIC")
# I waited for this finish for 1 hour and just stopped it :(
# Note that there is a package glmnet which does regularization, like lasso, for GLM

# Second serious approach:
logit1 = glm(y ~ ., data = bank, family = "binomial")
summary(logit1)

# Remove some non-significant predictors
bank2 = bank[,c(-1,-5,-12,-13)]
logit2 = glm(y ~ ., data = bank2, family = "binomial")
summary(logit2)
anova(logit1, logit2, test = "Chisq")

# LRT Chi-square test for significance using deviance

# Test statistic
(chisq.test.stat = logit2$null.deviance - logit2$deviance)
# [1] 10021.77

# p-value
with(logit2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# 0

# Or automatically :
l0  = glm(y ~ 1, data = bank2, family = "binomial")
anova(l0, logit2, test = "Chisq")

# Getting the odds
exp(coef(logit2))
# Interpret:

# CI based on profiled log-likelihood
confint(logit2)

# CI using the SE
confint.default(logit2)

# Predicted probabilities
ypred= predict(logit2, type = "response")


y.pred = predict(logit2, type = "response") > 0.5

sum(abs(bank2$y-y.pred))/length(bank2$y)

# Split data into train and test
# set.seed(421)
library(tidymodels)
split <- initial_split(bank2, prop = 0.8, strata = y)
train <- split %>% 
  training()
test <- split %>% 
  testing()

logit3 = glm(y ~ ., data = train, family = "binomial")
y3 = predict(logit3, test,type = "response") > 0.5
sum(abs(test$y-y3))/length(test$y)


# Example 2:
# In a health study to investigate an epidemic outbreak of a disease that is spread by mosquitoes,
# individuals were randomly sampled within two sectors in a city to determine if the person
# had recently contracted the disease under study.
# Three predictor variables were included in the study, representing known or potential
# risk factors. They are age, socioeconomic status of household, and sector within city.
# The data for 196 individuals in the sample are given in the disease outbreak data set in
# Appendix C.10 of Kutner, Nachstein, Neter & Li "Applied Linear Statistical Models"

data = read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC10.txt", header = FALSE)
data = data[,-1] # this removes a column with row numbers
colnames(data) = c("Age", "SES", "Sector", "Disease", "Savings")
head(data) # Check the data

# Recoding:
health = data[, -5]
health$Sector = health$Sector - 1
health$SES = as.factor(health$SES)
health = cbind(health, model.matrix(~ -1 + health$SES)[,2:3])
health = health[, -2]
colnames(health)[4:5] = c("SES2", "SES3")
health = health[,c(1,4,5,2,3)]
# health$SES2 = as.factor(health$SES2)
# health$SES3 = as.factor(health$SES3)
head(health)
dim(health)

mod1 = glm(Disease ~ .,  family = "binomial", data = health, subset = 1:98)

#fitted values
head(cbind(health,predict(mod1, type = "response")))

#  coefficients and odds ratios
summary(mod1)
exp(coef(mod1))

#  covariance matrix
vcov(mod1)

# LRT for significance of age
mod2 = glm(Disease ~ . - Age,  family = "binomial", data = health, subset = 1:98)
anova(mod2, mod1, test = "Chisq")
# Conclusion: Keep age since p-value = 0.02325 < 0.05

# Trying if interaction effects are needed:
mod3 = glm(Disease ~ .*.,  family = "binomial", data = health, subset = 1:98)
anova(mod1, mod3, test = "Chisq")
# Conclusion: Since p-value = 0.2163 > 0.05 interaction effects are not needed


# Model selection:
install.packages("bestglm")
library(bestglm)

# Modify the dataset matrix to be in the Xy format
colnames(health)[5] = "y"

res.bestglm = bestglm(Xy = health,
          family = binomial,
          IC = "AIC")
res.bestglm$BestModels
res.bestglm = bestglm(Xy = health,
                      family = binomial,
                      IC = "BIC")
res.bestglm$BestModels
# Note the models selected match with Table 14.6 b), but the AIC/BIC values are different