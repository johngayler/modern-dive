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

# We're now going to quantify the relationship' between our outcome and two explanatory variables using an interaction model. 

# Fit regression model
score_model_interaction <- lm(score ~ age * gender, data = evals_ch6)

# Get regression table
get_regression_table(score_model_interaction)

# The out put from this table nis telling us the following; firstly, 'female' is our baseline for comparison group as alphabetically it comes before 'male' (this is default in R but the levels in the factor can be reordered using forcats). The intercept for females is 4.88 (score) with an associated decrease in score of -0.018 for each additional unit increase in age. The outcome variable score is offset -0.446 for male instructors (i.e. 4.88 - 0.446 = 4.434) with an associated decrease of -0.004 units (-0.018 + 0.014) for each unit increase in age. 

# This model suggests that instructor score for females on average has a greater associated decrease for each unit increase of age than for male instructors. 

# INteraction models are so called as the associated effect of one vairable depends on the value of another vairable (e.g. male/female), that is to say, the two variables are 'interacting' with each other. In this model the associated effect of the variable age depends on the value of the other vairable gender. The difference inslopes for age of male instructors relative to femal instructors shows this.                                                        