#Two Models: First with trial heat polls and presidential approval. Second with trial heat polls and economic indicators.

#1.) Import Polls Data


#2.) Holbrook regressions using averge trial heats (by month) and average presidential approval.
#Note these are different than what Holbrook uses.


#3.) Removing presidential approval to compare with reg_oct


#4.) Generate predictions using september trial heat


#5.) Adding the predictions to the polls df and viewing 


#6.) Create the mean squared error


#Part II
#7.) import polls2


#8.) Regression with trial heat (sep) and economic indicator variables. 
#Note: These are different variables than used in the papers.


#9.) Generate predictions using unemployment 


#10.) Adding the predictions to the polls2 df and viewing 


#11.) Create the mean squared error


#12.) Combining both predictions and actual values and viewing


#13.) Combining all three datasets


#14.) Regressing with septembter trial heat, presidential approval, and unemployment rate in december









####################################################################################################







#1.) Import Polls Data
polls = read.csv("C:\\Users\\mawal\\Desktop\\polls.csv")

#2.) Holbrook regressions using averge trial heats (by month) and average presidential approval.
#Note these are different than what Holbrook uses.

reg_jul = lm(inc_vote ~ heat_jul + pres_approval_avg, data = polls)
reg_aug = lm(inc_vote ~ heat_aug + pres_approval_avg, data = polls)
reg_sep = lm(inc_vote ~ heat_sep + pres_approval_avg, data = polls)

summary(reg_jul)
summary(reg_aug)
summary(reg_sep)


#3.) Removing presidential approval to compare with reg_oct
reg_sep2 = lm(inc_vote ~ heat_sep, data = polls)
summary(reg_sep2)

#4.) Generate predictions using september trial heat
prediction = predict(reg_sep, polls)

#5.) Adding the predictions to the polls df and viewing 
new = cbind(polls, prediction)
new

#6.) Create the mean squared error
library(dplyr)
new %>%
  mutate(square_error = (inc_vote - prediction)^2) %>%
  summarize(mean(square_error))

#Part II
#7.) import polls2
polls2 = read.csv("C:\\Users\\mawal\\Desktop\\polls_2.csv")

#8.) Regression with trial heat (sep) and economic indicator variables. 
#Note: These are different variables than used in the papers.

regr = lm(inc_vote ~ heat_sep + GDP_per_change_t_1, data = polls2)
regr_1 = lm(inc_vote ~ heat_sep + unem_per_change_t_2_t_1, data = polls2)
regr_2 = lm(inc_vote ~ heat_sep + unem_dec_t_1, data = polls2)
regr_3 = lm(inc_vote ~ heat_sep + inflation_t_1, data = polls2)

summary(regr)
summary(regr_1)
summary(regr_2)
summary(regr_3)

#9.) Generate predictions using unemployment 
prediction2 = predict(regr_2, polls2)

#10.) Adding the predictions to the polls2 df and viewing 
new2 = cbind(polls2, prediction2)

#11.) Create the mean squared error
new2 %>%
  mutate(square_error = (inc_vote - prediction2)^2) %>%
  summarize(mean(square_error))

#12.) Combining both predictions and actual values and viewing
prediction2 = prediction2[1:18]
polls_new = polls['inc_vote']
preds = cbind(polls_new, prediction, prediction2)
preds

#13.) Combining all three datasets
polls3 = polls2 %>%
  filter(year > 1947)
new_df = cbind(polls, polls3)

#14.) Regressing with septembter trial heat, presidential approval, and unemployment rate in december
regr_new = lm(inc_vote ~ heat_sep + pres_approval_avg + unem_dec_t_1, data = new_df)
summary(regr_new)
