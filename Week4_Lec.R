#1.) Creating a vector of democratic vote percentage


#2.) Downloading TTR and Smooth packages


#3.) Creating Time Series Data using ts()


#4.) Two period moving average using SMA


#5.) Subset the data in order to create the squared error


#6.) Combining the data with cbind and creating sqerr


#7.) Taking the mean of the new sqrerr using dplyr


#8.) Calculate Weighted Moving average for 3 periods with weights 1,2,3


#9.) Subset the data in order to create the squared error


#10.) Combining the data with cbind and creating sqerr


#11.) Taking the mean of the new sqrerr using dplyr


#12.) Calculate Exponential Smoothing with an alpha of 0.8 


#13.) Subset the data in order to create the squared error


#14.) Combining the data with cbind and creating sqerr


#15.) Taking the mean of the new sqrerr using dplyr


#16.) Comparing the errors






##############################################################################################################
rm(list=ls())

#1.) Creating a vector of democratic vote percentage
dem_vote = c(53, 55, 50, 49, 54, 52, 51)

#2.) Downloading TTR and Smooth packages
library(TTR)
library(smooth)

#3.) Creating Time Series Data using ts()
myts <- ts(dem_vote, start=1992, end=2016, frequency=.25)


#4.) Two period moving average using SMA
SMA(myts, n = 2)
two_sma = SMA(myts, n = 2)

#5.) Subset the data in order to create the squared error
dem_vote2 = dem_vote[3:7]
two_sma = two_sma[2:6]

#6.) Combining the data with cbind and creating sqerr
final_two_sma = as.data.frame(cbind(dem_vote2, two_sma))
final_two_sma$sqrerr = (dem_vote2 - two_sma)^2

#7.) Taking the mean of the new sqrerr using dplyr
library(dplyr)
sma = final_two_sma %>%
  summarize(err = mean(sqrerr))


#8.) Calculate Weighted Moving average for 3 periods with weights 1,2,3
WMA(myts, n = 3, wts =c(1,2,3))
three_wma = WMA(myts, n = 3, wts =c(1,2,3))

#9.) Subset the data in order to create the squared error
dem_vote3 = dem_vote[4:7]
three_wma = three_wma[3:6]

#10.) Combining the data with cbind and creating sqerr
final_three_wma = as.data.frame(cbind(dem_vote3, three_wma))
final_three_wma$sqrerr = (dem_vote3 - three_wma)^2

#11.) Taking the mean of the new sqrerr using dplyr
wma = final_three_wma %>%
  summarize(err = mean(sqrerr))


#12.) Calculate Exponential Smoothing with an alpha of 0.8 
EMA(myts, n = 3, ratio = .8)
three_ema = EMA(myts, n = 3, ratio = .8)

#13.) Subset the data in order to create the squared error
dem_vote3 = dem_vote[4:7]
three_ema = three_ema[3:6]

#14.) Combining the data with cbind and creating sqerr
final_three_ema = as.data.frame(cbind(dem_vote3, three_ema))
final_three_ema$sqrerr = (dem_vote3 - three_ema)^2

#15.) Taking the mean of the new sqrerr using dplyr
ema = final_three_ema %>%
  summarize(err = mean(sqrerr))

#16.) Comparing the errors
sma
wma
ema














sma
wma
ema




year = c(1992, 1996, 2000, 2004, 2008, 2012, 2016)

df = as.data.frame(cbind(year, dem_vote))






install.packages('TTR')
library(forecast)
library(TTR)

#moving average loop by hand

ma = list()
for(i in 1:6) {
  ma[i] = (df$dem_vote[i+1] + df$dem_vote[i])/2
  print(ma)
}
ma

ma[2]

install.packages("smooth")
library(smooth)

myts <- ts(dem_vote, start=1992, end=2016, frequency=.25)

SMA(myts, n = 2)
        
library(smooth)        
sma(myts, h = 1, order = 2, silent = FALSE)$fitted
sma(myts, h = 1, order = 2, silent = FALSE)$forecast
sma(myts, h = 1, order = 2, silent = FALSE)$residuals
