# Example 1: Darwin data
# Get the data from agridat package:
install.packages("lattice")
library(agridat)

data(darwin.maize)
(dat = darwin.maize)
# write.csv(dat,"~/Downloads/darwin_maize.csv", row.names = FALSE)

install.packages("lattice")
libs(lattice)
bwplot(height~type, dat, main="darwin.maize") # box-and-whisker plot with lattice package

# We first need some data cleaning and reshaping
libs(reshape2)
dm <- melt(dat) # takes wide-format data and melts it into long-format data
d2 <- dcast(dm, pot + pair ~ type) # long to wide format
d2$diff <- d2$cross-d2$self
t.test(d2$diff)

# Same as
summary(lm(height~pair+type,data=dat))

# Note this is different, as it assumes independent samples:
with(d2, t.test(cross, self, var.equal = T))
# But is the same as:
summary(lm(height~ type,data=dat))


# Are pots significantly different? Answer: no
anova(lm(diff ~ pot, data=d2))


# Example 2: Studying relationship between variables
# Load the Income data from Chapter 2 of ISLR
# income and years of education for 30 individuals 
(dat2 = read.csv(file.choose()))
attach(dat2)

plot(Education, Income, pch = 20, col = "red", xlab = "Years of Education")
# The plot suggests that one might be able to predict income using years of education. 
# But what is the function that connects the input variable to the output?

library(ggplot2)
res = residuals(loess(Income ~ Education))

ggplot(dat2, aes(Education, Income)) +
  geom_point(shape = 20, size = 4, color = "red") +
  geom_segment(aes(xend = Education, yend = Income - res)) +
  geom_smooth(method = "loess", se = FALSE, color = "blue",
              formula = "y ~ x") +
  scale_x_continuous(breaks = seq(10,22,2)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Years of Education")
# This is just one of many ways to estimate the curve: nonparametric regression.