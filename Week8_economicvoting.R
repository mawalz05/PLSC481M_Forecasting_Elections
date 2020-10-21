#1.) Importing the ANES data
ANES = read.csv("C:\\Users\\mawal\\OneDrive - Binghamton University\\Desktop\\Desktop_Folders\\Teaching\\Forecasting Elections\\Week 8\\ANES.csv")

#2.) Data Wrangling to get the average of VCF0880 per year (pre-election)
#Question: Would you say that you [and your family]  are better off or worse off financially than you were a year ago.

library(dplyr)
ANES_2 = ANES %>%
  select(VCF0004, VCF0880, VCF1015)

ANES_2 = ANES %>%
  select(VCF0004, VCF0880, VCF1015) %>%
  filter(VCF1015 != 'NA')

ANES_2 = ANES %>%
  select(VCF0004, VCF0880, VCF1015) %>%
  filter(VCF1015 != 'NA')%>%
  filter(VCF0880 != 'NA') 

ANES_2 = ANES %>%
  select(VCF0004, VCF0880, VCF1015) %>%
  filter(VCF1015 != 'NA')%>%
  filter(VCF0880 != 'NA') %>%
  filter(VCF0880 >0 & VCF0880 < 4)

ANES_2 = ANES %>%
  select(VCF0004, VCF0880, VCF1015) %>%
  filter(VCF1015 != 'NA')%>%
  filter(VCF0880 != 'NA') %>%
  filter(VCF0880 >0 & VCF0880 < 4) %>%
  group_by(VCF0004) %>%
  summarize(score = mean(VCF0880))

ANES_2 = ANES_2[ which(ANES_2$VCF0004 !=1990 & ANES_2$VCF0004 != 2002), ]

#3.) Caluclating the log number of years of the incmbent party in the white house. 
logwth = c(8,4,8,4,4,8,12,4,8,4,8,4,8) 
logwth = log(logwth)

#4.) Combining the ANES and incumbency data
df = cbind(ANES_2, logwth)

#5.) Importing the week 7 polls data to get the incumbent party vote percentage.
polls = read.csv("C:\\Users\\mawal\\OneDrive - Binghamton University\\Desktop\\Desktop_Folders\\Teaching\\Forecasting Elections\\Week 8\\polls.csv")

#6.) filtering the polls data to include only 1972-2016
polls = polls %>% 
  filter(year > 1967)

#7.) Combining Polls data with new df.
df2 = cbind(polls, df)

#8.) Regress the incumbent party vote percentage on average VCF0880 score and years in the white house.
regr = lm(inc_vote ~ score + logwth, data = df2)
summary(regr)

#9.) Plotting the incumbency voter percentage against VCF0880 score by logwth category
library(ggplot2)
ggplot(df2, aes(x = score, y = inc_vote, col = logwth )) + geom_point() + geom_smooth(method = 'lm')
df2$logwth = as.factor(df2$logwth)

ggplot(df2, aes(x = score, y = inc_vote, col = logwth )) + geom_point() + geom_smooth(method = 'lm')
