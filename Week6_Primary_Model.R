
library(dplyr)

rm(list=ls())

norpoth = read.csv("C:\\Users\\mawal\\OneDrive - Binghamton University\\Desktop\\Desktop_Folders\\Teaching\\Forecasting Elections\\Week 6\\Norpoth_data.csv")

#Random Walk
random_walk = lm(r_vote ~ r_vote_1, data = norpoth)
summary(random_walk)

random_walk$coeff[1] + norpoth[27, 7]*random_walk$coeff[2]



#Auto-Regressive Model
auto_reg = lm(r_vote ~ r_vote_1 + r_vote_2, data = norpoth)
summary(auto_reg)

auto_reg$coeff[1] + norpoth[27, 7]*auto_reg$coeff[2] + norpoth[26, 7]*auto_reg$coeff[3]




#Primary Model Regression
regr = lm(r_vote ~ inc_primary + noninc_primary + r_vote_1 + r_vote_2 + partisan_adj, data = norpoth)
summary(regr)

Trump = regr$coefficients[2]*(.856-mean(norpoth[, 3])) + -1*regr$coefficients[3]*(.284-mean(norpoth[, 4])) + 
  regr$coefficients[4]*norpoth[27, 8] + regr$coefficients[5]*norpoth[27, 9] + regr$coefficients[1]
Trump

#Changing the range of the primary support variables from 35-65
for (i in 1:nrow(norpoth)){
  if (norpoth[i,3] <.35) {
    norpoth[[i, 3]] = .35
  }
  if (norpoth[i, 3] > .65) {
    norpoth[[i, 3]] = .65
  }
}

for (i in 1:nrow(norpoth)){
  if (norpoth[i,4] <.35) {
    norpoth[[i, 4]] = .35
  }
  if (norpoth[i, 4] > .65) {
    norpoth[[i, 4]] = .65
  }
}

#Rerunning the regressions
regr = lm(r_vote ~ inc_primary + noninc_primary + r_vote_1 + r_vote_2 + partisan_adj, data = norpoth)
summary(regr)

Trump = regr$coefficients[2]*(.65-mean(norpoth[, 3])) + -1*regr$coefficients[3]*(.35-mean(norpoth[, 4])) + 
  regr$coefficients[4]*norpoth[27, 11] + regr$coefficients[5]*norpoth[27, 12] + regr$coefficients[1]
Trump

#Testing the mean values
mean(norpoth[, 3])
mean(norpoth[, 4])

#Using the mean values from the article. 
Trump = regr$coefficients[2]*(.65-.54) + -1*regr$coefficients[3]*(.35-.49) + 
  regr$coefficients[4]*norpoth[27, 8] + regr$coefficients[5]*norpoth[27, 9] + regr$coefficients[1]
Trump


