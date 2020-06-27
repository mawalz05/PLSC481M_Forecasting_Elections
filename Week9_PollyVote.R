
df = read.csv("C:\\Users\\mawal\\Desktop\\Compiled_data.csv")

#Random Walk
random_walk = lm(r_vote ~ r_vote_1, data = df)
summary(random_walk)

rand_pred = random_walk$coeff[1] + norpoth[27, 7]*random_walk$coeff[2]
rand_pred

#Auto-Regressive Model
auto_reg = lm(r_vote ~ r_vote_1 + r_vote_2, data = df)
summary(auto_reg)

autoreg_pred = auto_reg$coeff[1] + df[27, 7]*auto_reg$coeff[2] + df[26, 7]*auto_reg$coeff[3]
autoreg_pred

#Primary Model
primary = lm(d_vote ~ inc_primary + noninc_primary + d_vote_1 + d_vote_2 + partisan_adj, data = df)
summary(primary)

primary_pred = -1*primary$coefficients[2]*(.856-mean(df[, 3])) + primary$coefficients[3]*(.284-mean(df[, 4])) + 
  primary$coefficients[4]*df[27, 10] + primary$coefficients[5]*df[27, 11] + primary$coefficients[1]
primary_pred

#Polls Model
polls = lm(inc_vote ~ heat_sep + pres_approval_avg, data = df)
summary(polls)

polls_pred = polls$coeff[1] + X*polls$coeff[2] + X*polls$coeff[3] #Need to calculate average trial heat September and average approval rating for Trump.
polls_pred

#Economic Voting
econ = lm(inc_vote ~ score + logwth, data = df2)
summary(econ)

econ_pred = econ$coeff[1] + X*econ$coeff[2] + 1.386294361*econ$coeff[3] #Need to calculate X which is the VCF0880 score - from Consumer Survey Reports
econ_pred


(rand_pred + autoreg_pred + primary_pred + polls_pred + econ_pred)/5

(rand_pred + autoreg_pred + primary_pred + polls_pred*2 + econ_pred*2)/7

(rand_pred + autoreg_pred + primary_pred + polls_pred*2 + econ_pred*2 + X + X + x)/10 #Maybe add 538 prediction, CNN, and foxNews



