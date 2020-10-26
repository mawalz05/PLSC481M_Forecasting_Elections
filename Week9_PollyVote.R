rm(list=ls())

df = read.csv("C:\\Users\\mawal\\OneDrive - Binghamton University\\Desktop\\Desktop_Folders\\Teaching\\Forecasting Elections\\Week 9\\Compiled_data.csv")

############################################################
#New section - Comaparing Model Errors

########################random_walk
# Regression
auto_reg = lm(r_vote ~ r_vote_1 + r_vote_2, data = df)

# Generate predictions 
ar_prediction = predict(auto_reg, df)

# Adding the predictions to the polls df and viewing 
new = cbind(df['r_vote'], ar_prediction)
new

# Create the absolute error
library(dplyr)
new %>%
  mutate(abs_error = abs(r_vote*100 - ar_prediction*100)) %>%
  summarize(mean(abs_error))

###########################Economic Voting
#Regression
econ = lm(inc_vote ~ score + logwth, data = df)

#Generate predictions
library(dplyr)
df2 = df %>%
  filter(Year > 1967)

ec_prediction = predict(econ, df2)

# Adding the predictions to the polls df and viewing 
new2 = cbind(df2['inc_vote'], ec_prediction)
new2

# Create the absolute error
library(dplyr)
new2 %>%
  mutate(abs_error = abs(inc_vote - ec_prediction)) %>%
  summarize(mean(abs_error))

###########################################################
#PollyVote Model
###########################################################

#Random Walk
random_walk = lm(r_vote ~ r_vote_1, data = df)
summary(random_walk)

rand_pred = random_walk$coeff[1] + df[27, 7]*random_walk$coeff[2]
rand_pred

#Auto-Regressive Model
auto_reg = lm(r_vote ~ r_vote_1 + r_vote_2, data = df)
summary(auto_reg)

autoreg_pred = auto_reg$coeff[1] + df[27, 7]*auto_reg$coeff[2] + df[26, 7]*auto_reg$coeff[3]
autoreg_pred

#Primary Model
primary = lm(r_vote ~ inc_primary + noninc_primary + r_vote_1 + r_vote_2 + partisan_adj, data = df)
summary(primary)

primary_pred = primary$coefficients[2]*(.856-mean(df[, 3])) + -1*primary$coefficients[3]*(.284-mean(df[, 4])) + 
  primary$coefficients[4]*df[27, 10] + primary$coefficients[5]*df[27, 11] + primary$coefficients[1]
primary_pred

#Polls Model
polls = lm(inc_vote ~ heat_sep + pres_approval_avg, data = df)
summary(polls)

polls_pred = polls$coeff[1] + polls$coeff[2]*42 + polls$coeff[3]*40 #Need to calculate average trial heat September and average approval rating for Trump.
polls_pred

#Economic Voting
econ = lm(inc_vote ~ score + logwth, data = df)
summary(econ)

econ_pred = econ$coeff[1] + econ$coeff[2]*2 + econ$coeff[3]*1.386294361 #Need to calculate X which is the VCF0880 score - from Consumer Survey Reports
econ_pred
#https://ig.ft.com/ft-peterson-poll/ - This is where I got 2 from.

#Averaging the 5 models and coming up with a prediction for Trump
(rand_pred*100 + autoreg_pred*100 + primary_pred*100 + polls_pred + econ_pred)/5

#Weighing the polls and economic models higher and coming up with a prediction for Trump.
(rand_pred*100 + autoreg_pred*100 + primary_pred*100 + polls_pred*2 + econ_pred*2)/7


