# Example 1: Darwin data

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.formula.api import ols
import scipy.stats as stats

# Assuming darwin.maize data is available as a CSV file
dat = pd.read_csv('../data/01-darwin_maize.csv')

# Box-and-whisker plot with seaborn
sns.boxplot(x='type', y='height', data=dat)
plt.title("darwin.maize")
plt.show()

# We first need some data cleaning and reshaping
dm = pd.melt(dat, id_vars=['pot', 'pair', 'type'], value_vars = 'height')
dm
d2 = dm.pivot_table(index=['pot', 'pair'], columns='type', values='value').reset_index()
d2['diff'] = d2['cross'] - d2['self']
stats.ttest_1samp(d2['diff'], 0)

# Same as
model = ols('height ~ pair + type', data=dat).fit()
print(model.summary())

# Note this is different, as it assumes independent samples:
stats.ttest_ind(d2['cross'], d2['self'], equal_var=True)

# Are pots significantly different? Answer: no
sm.stats.anova_lm(ols('diff ~ pot', data=d2).fit())


# Example 2: Studying relationship between variables
# Load the Income data from Chapter 2 of ISLR
# income and years of education for 30 individuals
dat2 = pd.read_csv('income_data.csv')

plt.scatter(dat2['Education'], dat2['Income'], color='red')
plt.xlabel("Years of Education")
plt.show()

# The plot suggests that one might be able to predict income using years of education.
# But what is the function that connects the input variable to the output?

import numpy as np
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt

# Fit a LOESS curve
lowess = sm.nonparametric.lowess
w = lowess(dat2['Income'], dat2['Education'], frac=1/3)

# Plot the results
plt.scatter(dat2['Education'], dat2['Income'], color='red')
plt.plot(w[:, 0], w[:, 1], color='blue')
plt.xlabel("Years of Education")
plt.show()
