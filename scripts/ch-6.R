# Chapter 6 - Multiple Regression

# Load required packages
library(tidyverse)
library(moderndive)
library(skimr)
library(ISLR)

# 6.1 One numerican and one categorical explanatory variable

# In this section we're going to consider a model using age and gender as explanatory variables for our outcome var, teaching score. 

# 6.1.1 EDA

# Subset data to only include vars of interest

evals_ch6 <- evals %>% 
  select(ID, score, age, gender)

# Checkout the data
glimpse(evals_ch6)

skim(evals_ch6)

# There are 436 observations across 4 vars. Age is appears to be relatively normally distributed; the min is 29, max is 73 and mean is 48.4 while the median is 48. As previously seen in Ch 5, score is skewed right. 

summary(evals_ch6)

# Print out random 5 rows
evals_ch6 %>% 
  sample_n(5)

# Look at correlaton between score / age
evals_ch6 %>% 
  get_correlation(score ~ age)

# There's a weak negative correlation between age and score.

# Some quick plots

evals_ch6 %>% 
  ggplot(aes(x = gender)) + 
  geom_bar()

evals_ch6 %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(colour = "white") + 
  labs(title = "Distribution of teacher ages")

# Quick plot of relationship between age / score by gender
# Female instructors appear to be penalised more for an increase in their age. 
evals_ch6 %>% 
  ggplot(aes(x = age, y = score, colour = gender)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Age", 
       y = "Teaching Score", 
       colour = "Gender")

# 6.1.2. Interaction Model
