# Project Name: Personal Insurance and Medical Cost 
# Author: Jianan Li
# Date: Jan 29, 2019

## Part 1: Data
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
library(dplyr)
library(corrplot)
library(reshape)
library(tidyverse)
library(ggcorrplot)

# 1. Import data
MC = read.csv('/Users/jiananli/Desktop/insurance.csv', header = TRUE)
df = MC #original dataset
#Exploring the Dataset
str(df)
summary(df)
names(df)
#Dimension of the data frame
cat('Dimension of the data frame:')
dim(df)

## Exploratory Data Analysis
# Descriptive Statistics
summary(MC)

#2. Research questions
## Q1: Smokers have higher medical cost that non-smokers.
## Q2: The older the investigators, the higher medical cost.
## Q3: The higher bmi, the higher medical cost.

# 3.Exploratory data analysis
ggplot(data = melt(MC[, -6]), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = "free_x")
## Show the general data distribution among age, bmi, children, and charges
ggplot(data = MC)+
  geom_bar(mapping = aes(x = as.factor(sex)))
## The population ratio of female and male is close to 1: 1.
ggplot(data = MC)+
  geom_bar(mapping = aes(x = as.factor(smoker)))
## The population of non-smoker is over 3.5 times smoker.
ggplot(data = MC)+
  geom_bar(mapping = aes(x = as.factor(region)))
## The population of southeast is more than other three regions.
MC %>%
  ggplot(aes(x = smoker, y = charges))+
  geom_boxplot() + 
  ggtitle('Charges of smokers vs non-smokers')
## Smokers have higher medical cost than non-smokers.
MC %>%
  ggplot(aes(x = age, y = charges)) +
  geom_point(aes(color = smoker)) +
  ggtitle('Age vs charges')
## From the plot chart, smokers have higher insurance charges than non-smokers.
## And people with older age tend to have higher insurance cost.
MC %>%
  ggplot(aes(x = age, y = charges)) +
  geom_point(aes(color = smoker, size = bmi, alpha = .05)) +
  ggtitle('Age vs charges')
## Unger similiar conditions, people with higher bmi tend to have higher insurance cost.
MC %>%
  ggplot(aes(x = as.factor(children), y = charges)) +
  geom_boxplot() +
  ggtitle('Children vs charges')
## Number of children have less impact on insurance cost.
MC %>%
  ggplot(aes(x = sex, y = charges)) +
  geom_boxplot() +
  ggtitle('sex vs charges')
## Male tend to have higher insurance cost than female.
# ------Build multiple linear regression model--------
MC <- MC %>%
  mutate(obese = ifelse(bmi >= 30, 'yes', 'no'))
MC$obese <- as.factor(MC$obese)
MC$smoker <- as.factor(MC$smoker)
MC$sex <- as.factor(MC$sex)
MC$region <- as.factor(MC$region)
set.seed(134)
sampleSize <- floor(.75*nrow(MC))
trainIndexes <- sample(seq_len(nrow(MC)), sampleSize, replace = FALSE)
train <- MC[trainIndexes, ]
test <- MC[-trainIndexes, ]
par(mfrow = c(2,2))
lm.fit.m1 <- lm(charges ~ . -bmi, data = train)
summary(lm.fit.m1)
plot(lm.fit.m1)
## Our full model appears to be a pretty decent fit. 
## The model explains roughly 74% of the variation in the charges (Multiple R-squared:  0.7401).
## There are clusters in the plot of the Residuals vs Fitted values plot, which may indicate that this model is not ideal.
## Sex and region have less influence on charges
par(mfrow = c(2,2))
lm.fit.m2 <- lm(charges ~ . -bmi -sex -region, data = train)
summary(lm.fit.m2)
plot(lm.fit.m2)
## Based on the plot, our model2 is not ideal either.
## The result is influenced by outlier.
par(mfrow = c(2,2))
lm.fit.m3 <- lm(charges ~ obese*smoker + age +children, data = train)
summary(lm.fit.m3)
plot(lm.fit.m3)
## This model is better than model1 and model2.
## R-squared and F-statistic increase, but normal Q-Q is still not ideal.
par(mfrow = c(2,2))
lm.fit.m4 <- lm(charges ~ obese*smoker + age, data = train)
summary(lm.fit.m4)
plot(lm.fit.m4)
## R-squares drops only .0031 without the addition of children, while the F-statistic increased to 1324.
## For each year increase in age, we can expect a $264.57 increase in average charges.
## For a person who is an obese non-smoker, we can expect $320.24 higher charges on average.
## For a person who is a non-obese smoker, we can expect $13,151.58 higher charges on average.
## For a person who is an obese smoker, we can expect $320.24 + $13.151.58 + $19,574.27 = $33,046.09 higher charges on average.

# Predicting Values for Test Data
test$prediction <- predict(lm.fit.m4, newdata = test)
test %>%
  ggplot() +
  geom_point(aes(x = prediction, y = charges, color = smoker, shape = obese)) +
  geom_abline(color = 'red') +
  ggtitle('Training vs testing')
## Our model 4 is good fitting for testing data.
#-----Conclusion------
## Our three assumptions are confirmed by model 4. 
## Smokers have higher medical cost that non-smokers.
## The older the investigators, the higher medical cost.
## The higher bmi, the higher medical cost.