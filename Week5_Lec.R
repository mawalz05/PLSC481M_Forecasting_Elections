
#1.) Install the ggplot package
install.packages("ggplot2")
library(ggplot2)

#2.) Create the dem_vote, unem, and gallup vectors
dem_vote = c(53, 55, 50, 49, 54, 52, 51)
unem = c(7.3, 5.2, 3.9, 5.5, 6.5, 7.8, 4.9)
gallup = c(54.4, 57.5, 47.8, 50, 55.8, 50.5, 51.1)

#3.) Combine the vectors into a single data frame
df = as.data.frame(cbind(dem_vote, unem, gallup))

#4.) Create a scatter plot of unem on x axis and dem_vote on y axis.
ggplot(df, aes(x = unem, y = dem_vote)) + geom_point()

#5.) Regress unemployment on dem_vote
regr = lm(dem_vote ~ unem, df)
summary(regr)

#6.) Add a regression line to the earlier plot without the confidence bounds
ggplot(df, aes(x = unem, y = dem_vote)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

#7.) Add a regression line to the earlier plot with the confidence bounds
ggplot(df, aes(x = unem, y = dem_vote)) + geom_point() + geom_smooth(method = "lm", se = TRUE)

#8.) Create a scatter plot with gallup on the x axis
ggplot(df, aes(x = gallup, y = dem_vote)) + geom_point()

#9.) Regress gallup on dem_vote
regr2 = lm(dem_vote ~ gallup, df)
summary(regr2)

#10.) Add a regression line to the earlier plot without the confidence bounds
ggplot(df, aes(x = gallup, y = dem_vote)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

#11.) Add a regression line to the earlier plot with the confidence bounds
ggplot(df, aes(x = gallup, y = dem_vote)) + geom_point() + geom_smooth(method = "lm", se = TRUE)

#12.) Regress unem and gallup on dem_vote
regr3 = lm(dem_vote ~ unem + gallup, df)
summary(regr3)
