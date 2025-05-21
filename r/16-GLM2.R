# Poisson regression.
# Example 1
library(gpk)
data(elephant)
attach(elephant)

library(ggplot2)

# install.packages("emojifont")
library(emojifont)

# Nonparametric curve fitted to data:
ggplot(elephant, aes(x = Age_in_Years, y = Number_of_Matings, label = emoji("elephant"))) +
  geom_text(family="EmojiOne", color = "black",
            position = position_jitter(width = .1, height = .1))+
  geom_smooth(method = "loess")

# First try: linear model:
ggplot(elephant, aes(x = Age_in_Years, y = Number_of_Matings, label = emoji("elephant"))) +
  geom_text(family="EmojiOne", color = "black",
            position = position_jitter(width = .1, height = .1))+
  geom_smooth(method = "lm")

elephant_lm1 <- lm(Number_of_Matings ~ Age_in_Years, data = elephant)
summary(elephant_lm1)
confint(elephant_lm1)

plot(elephant_lm1)


# Version 2:
ggplot(elephant, aes(x = Age_in_Years, y = Number_of_Matings, label = emoji("elephant"))) +
  geom_text(family="EmojiOne", color = "black",
            position = position_jitter(width = .1, height = .1))+
  geom_smooth(method = "lm")+
  scale_y_continuous(trans = "log1p")

elephant_lm2 <- lm(log1p(Number_of_Matings) ~ Age_in_Years, data = elephant)
summary(elephant_lm2)

# Version 3:
elephant.glm <- glm(Number_of_Matings ~ Age_in_Years, data = elephant, family = poisson(link = "log"))
summary(elephant.glm)

#calculate McFadden's R-squared for the model
with(summary(elephant.glm), 1 - deviance/null.deviance)

ggplot(elephant, aes(x = Age_in_Years, y = Number_of_Matings, label = emoji("elephant"))) +
  geom_text(family="EmojiOne", color = "black",
            position = position_jitter(width = .1, height = .1))+
  geom_smooth(method = "glm",method.args = list('poisson'))

# Hypothesis test:
anova(elephant.glm, test = "LRT")
# Or manually:
null.elephant.glm <- glm(Number_of_Matings ~1, data = elephant, family = poisson(link = "log"))
(D                 <- 2 * (logLik(elephant.glm) - logLik(null.elephant.glm)))
p.val             <- pchisq(q = D, df = 1, lower.tail = FALSE)

# Log-likelihood of saturated model:
library(dplyr)
oversaturated_elephants <- elephant %>%
  dplyr::select(Number_of_Matings) %>%
  mutate(logLik = dpois(x = Number_of_Matings, lambda = Number_of_Matings, log = TRUE))
oversaturated_elephants

# Test for overdispersion
#install.packages("AER")
library(AER)
dispersiontest(elephant.glm)


# Example 2
# Read file 17-winewhite.sav

library(foreign)
wine=read.spss(file.choose(), to.data.frame=T)

# Check out the data
head(wine)
summary(wine)

# Convert variables D1-D20 to categorical
for (j in 4:23) wine[,j] = factor(wine[,j])

# Note summary changes
summary(wine)
# Remove the ID
wine = wine[,-1]

summary(p1 <- glm(Y ~ ., family="poisson", data = wine))

# Remove non significant predictors
summary(p2 <- glm(Y ~ . -D9 -D11- D12- D13- D16- D17- D19- D20, family="poisson", data = wine))

# Or we could have used update function

p2 <- update(p1, . ~ . -D9 -D11- D12- D13- D16- D17- D19- D20)

# Compare models
anova(p2, p1, test="Chisq")
# Conclusion: the two models are not significantly different
# Therefore, keep the simpler one

# Check for overdispersion
dispersiontest(p2)

# Another way
E2 <- resid(p2, type = "pearson")
N  <- nrow(wine)
p  <- length(coef(p2))  
sum(E2^2) / (N - p)

# Zero-inflation Poisson model
#install.packages("pscl")
library(pscl)
summary(m1 <- zeroinfl(Y ~ . -D9 -D11- D12- D13- D16- D17- D19- D20|D1+D2 , data = wine))




# Exercise (from class notes)
# Read file 17-crabs.sav
crabs = read.spss(file.choose())
e = glm(satell ~ width, family="poisson", data = crabs)
summary(e)
library(MASS)
summary(m1 <- glm.nb(satell ~ width, data = crabs))


# Example 3: real data of smoking and survival rate
# install.packages("GLMsData")
library(GLMsData)
data(wwomen)
wwomen

(twobytwo = xtabs(Count~ Smoking+Status, data = wwomen))
prop.table(twobytwo, margin = 1)
# Notice the percent alive is higher among the smokers group!

(twobytwoperAge = xtabs(Count~ Smoking+Status+Age, data = wwomen))
(con2 = ftable(twobytwoperAge)) # this creates the so-called "flat" contingency table

# install.packages("tidyverse")
# library(tidyverse)
# con2 %>% ftable(row.vars=c("Age", "Smoking")) # transposes the flat table for better view
# 
# con2 %>% ftable(row.vars=c("Age", "Smoking")) %>% prop.table(margin = 1) %>% round(2)
# # Notice how the proportion alive is now lower within most age groups

# For the glm function y variable needs to be dichotomous 0 and 1  
wwomen$alive = ifelse(wwomen$Status == "Alive",1, 0)

glm1 = glm( alive~ Smoking, weights = Count, data = wwomen, family="binomial")
summary(glm1)
exp(coef(glm1))
# Interpretation: There is 46% increase of the odds of being alive in the smokers group

glm2 = glm(alive ~ Smoking + Age, weights = Count, data = wwomen, family="binomial")
# Notice the use of "weights" because the data are summarized!

summary(glm2)
exp(coef(glm2))

# Interpretation: 
# There is 35% decrease in the odds of being alive within the smokers group, controlling for age!
# Explanation: In the early 70s there were more nonsmokers in the 65-74 age group 
# and most of them died died anyway from old age.


# Another approach
# install.packages("mosaicData")
library(mosaicData)

head(Whickham)

# install.packages("tidyverse")
library(tidyverse)

# Summary statistics
# What percentage of the women in the study were smokers?
Whickham %>% count(smoker) %>% mutate(smoker_perc = n / sum(n)) 
# count lets you quickly count the unique values of one or more variables
# mutate creates new columns

# What percentage of the women in the study had died by the follow-up? 
Whickham %>% count(outcome) %>% mutate(outcome_perc = n / sum(n))


# What percentage of smokers had died by the follow-up and what percentage of non-smokers had died at the follow-up?
Whickham %>% 
  count(smoker, outcome) %>%
  group_by(smoker) %>%
  mutate(outcome_perc = n / sum(n)) %>%
  filter(outcome=="Dead") # subsets rows using column names

# Construct a plot that shows the relationship between survival status and smoking.
# regardless of age
ggplot(Whickham, aes(x=smoker, fill=outcome)) + 
  geom_bar(position = "fill") + labs(y="Proportion") + theme_bw()

# Collapse the values of age into a new categorical variable with 3 age categories: women between the age of 18 and 44, women who are older than 44 and younger than 65, and women who are 65 and over. 
# Note that this is the age at the time of the first survey. Examine the percentage of smokers and non-smokers who died at follow-up in each age category.

table(cut(Whickham$age, breaks = c(17.9, 24, 34, 44, 54, 64, 74, 100)))

Whickham <- Whickham %>% 
  mutate(cat_age = cut(Whickham$age, breaks = c(17.9, 24, 34, 44, 54, 64, 74, 100)))
Whickham %>% 
  count(cat_age, smoker, outcome) %>%
  group_by(cat_age, smoker) %>% # performs operations by the values of the grouping variables
  mutate(outcome_perc = n / sum(n)) %>%
  filter(outcome=="Dead")

# Construct a plot that shows the relationship between survival status and smoking for each age group.

ggplot(Whickham, aes(x=smoker, fill=outcome)) + geom_bar(position = "fill") + 
  labs(y="Proportion") + facet_grid(. ~ cat_age) + theme_bw()

# Show uneven distribution of smokers
ggplot(Whickham, aes(x=outcome, fill=smoker)) + geom_bar(position = "fill") + 
  labs(y="Proportion") + facet_grid(. ~ cat_age) + theme_bw()


Whickham$alive = ifelse(Whickham$outcome == "Alive", 1,0)
glm3 = glm( alive~ smoker,  data = Whickham, family="binomial")
summary(glm3)


glm4 = glm( alive~ smoker+cat_age,  data = Whickham, family = "binomial")
summary(glm4)

# Same conclusion as before.
# The smoking/nonsmoking status was not evenly distributed over age.
# The older women, almost sure to die, had fewer smokers, 
# so their high mortality rate was incorrectly connected to less smoking!
# Moral of the story: age is a confounding variable
# The hazards of ignoring a third variable and getting a relationship backwards is called Simpson’s paradox.


# Ordinal logistic regression
# Example 4: applying to graduate school. 

library(foreign)
dat = read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)

# This data set has a three level variable called apply, with levels "unlikely", "somewhat likely", and "very likely", 
# coded 1, 2, and 3, respectively, that we will use as our outcome variable. We also have three variables that we will 
# use as predictors: pared, which is a 0/1 variable indicating whether at least one parent has a graduate degree; 
# public, which is a 0/1 variable where 1 indicates that the undergraduate institution is public and 0 private, 
# and gpa, which is the student's grade point average.

# Descriptive stats
lapply(dat[, c("apply", "pared", "public")], table)

# three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ public + apply + pared, data = dat))

summary(dat$gpa)

# We will use the polr function in R
# model is:

# logit(P(Y <= j)) = bj0 - b1*x1 - ... - bp*xp

library(MASS)
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)
summary(m)

# Coefficients:
#           Value Std. Error t value
# pared   1.04769     0.2658  3.9418
# public -0.05879     0.2979 -0.1974
# gpa     0.61594     0.2606  2.3632
# 
# Intercepts:
#                              Value   Std. Error t value
# unlikely|somewhat likely     2.2039  0.7795     2.8272
# somewhat likely|very likely  4.2994  0.8043     5.3453
# 
# Residual Deviance: 717.0249 
# AIC: 727.0249 

# Estimated model:
# logit(P(Y <= 1)) = 2.20 - 1.05*PARED - (-0.06)*PUBLIC - 0.616*GPA
# logit(P(Y <= 2)) = 4.30 - 1.05*PARED - (-0.06)*PUBLIC - 0.616*GPA

# Profile likelihood CI
(ci <- confint(m))

# CIs assuming normality
confint.default(m)

# Interpretation:

# for pared, we would say that for a one unit increase in pared (i.e., going from 0 to 1), 
# we expect a 1.05 increase in the expected value of apply on the log odds scale, 
# given all of the other variables in the model are held constant.

# For gpa, we would say that for a one unit increase in gpa, 
# we would expect a 0.62 increase in the expected value of apply in the log odds scale, 
# given that all of the other variables in the model are held constant.

# odds ratios
exp(coef(m))

# OR and CI
exp(cbind(OR = coef(m), ci))

# Interpretation:

# Parental education:
# For students whose parents did attend college, the odds of being more likely 
# (i.e., very or somewhat likely versus unlikely) to apply is 2.85 times that 
# of students whose parents did not go to college, holding constant all other variables.

# School type:
# For students in private school, the odds of being more likely to apply is 1.06 times 
# [i.e., 1/0.943] that of public school students, holding constant all other variables (positive odds ratio).

# GPA:
# For every one unit increase in student's GPA the odds of being more likely to apply 
# (very or somewhat likely versus unlikely) is multiplied 1.85 times (i.e., increases 85%), 
# holding constant all other variables.