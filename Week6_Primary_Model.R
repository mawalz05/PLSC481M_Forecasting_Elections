
library(dplyr)

rm(list=ls())

norpoth = read.csv("C:\\Users\\mawal\\Desktop\\Norpoth_data.csv")

#Random Walk
random_walk = lm(r_vote ~ r_vote_1, data = norpoth)
summary(random_walk)

random_walk$coeff[1] + norpoth[27, 7]*random_walk$coeff[2]



#Auto-Regressive Model
auto_reg = lm(r_vote ~ r_vote_1 + r_vote_2, data = norpoth)
summary(auto_reg)

auto_reg$coeff[1] + norpoth[27, 7]*auto_reg$coeff[2] + norpoth[26, 7]*auto_reg$coeff[3]



#Removing the last two rows and first two columns (avg value and 2020) (year and primary vote)
norpoth = norpoth[1:27, 3:13]

#Dems Regression
regd = lm(d_vote ~ inc_primary + noninc_primary + d_vote_1 + d_vote_2 + partisan_adj, data = norpoth)
summary(regd)

Biden = -1*regd$coefficients[2]*(.856-mean(norpoth[, 1])) + regd$coefficients[3]*(.284-mean(norpoth[, 2])) + 
  regd$coefficients[4]*norpoth[27, 9] + regd$coefficients[5]*norpoth[27, 10] + regd$coefficients[1]
Biden

#Changing the range of the primary support variables from 35-65
for (i in 1:nrow(norpoth)){
  if (norpoth[i,1] <.35) {
    norpoth[[i, 1]] = .35
  }
  if (norpoth[i, 1] > .65) {
    norpoth[[i, 1]] = .65
  }
}

for (i in 1:nrow(norpoth)){
  if (norpoth[i,2] <.35) {
    norpoth[[i, 2]] = .35
  }
  if (norpoth[i, 2] > .65) {
    norpoth[[i, 2]] = .65
  }
}

regd = lm(d_vote ~ inc_primary + noninc_primary + d_vote_1 + d_vote_2 + partisan_adj, data = norpoth)
summary(regd)

Biden2 = -1*regd$coefficients[2]*(.65-mean(norpoth[, 1])) + regd$coefficients[3]*(.35-mean(norpoth[, 2])) + 
  regd$coefficients[4]*norpoth[27, 9] + regd$coefficients[5]*norpoth[27, 10] + regd$coefficients[1]
Biden2

#These are the 2016 coefficients
-1*.417*(65-57) + .17*(35-48) + 51.1133*.361 + 52*-.377 + 50.6

