rm(list=ls())

df = read.csv("C:\\Users\\mawal\\OneDrive - Binghamton University\\Desktop\\Desktop_Folders\\Teaching\\Forecasting Elections\\Week 9\\Compiled_data.csv")

# Standard Regression
reg = lm(inc_vote ~ heat_sep + pres_approval_avg, data = df)
summary(reg)

# Prediction equation for 2020
Trump = reg$coefficients[1] + reg$coefficients[2]*42.0 + reg$coefficients[3]*40
Trump

50 - 48.27 # The amount away from 50 percent

# Using the model to create predictions on the data
prediction = predict(reg, df)

#5.) Adding the predictions to the df
new = cbind(df, prediction)
new

# Filtering for elections after 1948 and including only Year, inc_vote, and prediction in the df.
library(dplyr)
new = new %>%
  select(Year, inc_vote, prediction) %>%
  filter(Year > 1947)

# Creating a column called error that is the prediction minus the incumbent vote
new$error = new$prediction - new$inc_vote

# Creating a column called accuracy that puts a 1 if the error is greater than 1.73 and 0 if less than 1.73
new = new %>%
  mutate(accuracy = ifelse(error > 1.73, 1, 0))

# Summing the accuracy column and dividing by the toal observations.
1 - (sum(new$accuracy)/nrow(new))

#67% confident that Trump is going to lose the election.
