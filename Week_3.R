#1.) Set working directy and import mtcars


setwd("C:\\Users\\mawal\\Desktop")
rm(list=ls())
mtcars = read.csv('mtcars.csv')

#2.) Descriptive look at mtcars - head, str, hist, summary, View
head(mtcars, 10)
str(mtcars)
hist(mtcars$mpg)
summary(mtcars)
View(mtcars)

#3.) Extract mpg column from mtcars and View and save mpg
mpg = mtcars$mpg
mpg

#4.) Remove column 2 (mpg) from the mtcars dataset and view mtcars
mtcars = mtcars[-2]
mtcars

#5.) Cbind mtcars and mpg and View mtcars
mtcars = cbind(mtcars, mpg)
mtcars

#6.) Extract the Mazda RX4 row from mtcars. Save as Mazda_RX4 and View.
Mazda_RX4 = mtcars[ which(mtcars$model=='Mazda RX4'), ]
Mazda_RX4

#7.) Remove the Mazda RX4 row from mtcars. View and save mtcars.
mtcars = mtcars[ which(mtcars$model!='Mazda RX4'), ]
mtcars

#8.) Rbind mtcars and Mazda RX4
mtcars = rbind(mtcars, Mazda_RX4)
mtcars

#9.) Create a long dataset from mtcars with gather (mtcars2). First, install tidyr.
install.packages("tidyr")
library(tidyr)

mtcars2 = gather(mtcars, key = variables, val = observation)
mtcars2

#10.) Group by gear and summarize the mean mpg. First install the dplyr package
install.packages("dplyr")
library(dplyr)

mtcars %>%
  group_by(gear) %>%
  summarize(mean(mpg))

#11.) Arrange mpg in descending order.
mtcars %>%
  arrange(desc(mpg))

#12.) create wt_per_cyl  as wt divided by cyl.
mtcars %>%
  mutate(wt_per_cyl = wt/cyl)

#13.) Combine the cyl and gear columns
mtcars = unite(mtcars, cyl_gear, cyl, gear, sep = '-')
mtcars

#14.) Separate the cyl and gear columns
mtcars = separate(mtcars, cyl_gear, c('cyl','gear'), sep = '-')
mtcars

#15.) Create a column with NA values, find those values with is.na, any and sum is.na, summary.
mtcars$new = c(1:30, NA, NA)
any(is.na(mtcars))
is.na(mtcars)
sum(is.na(mtcars))
summary(mtcars)

#16.) Create a variable that contains the missing values
miss = which(is.na(mtcars$new))

#17.) Drop missing rows from the dataset.
mtcars[complete.cases(mtcars), ]
na.omit(mtcars)

#18.) Setting the missing values to 0 and viewing the data.
mtcars$new[miss] = 0
mtcars

#19.) Create a histogram of mpg. First install ggplot2 package.
install.packages('ggplot2')
library(ggplot2)
ggplot(mtcars, aes(x = mpg)) + geom_histogram()

#20.) Create a scatter plot of y = mpg and x = wt
ggplot(mtcars, aes(y = mpg, x = wt)) + geom_point()

#21.) Add color = am to the scatter plot
ggplot(mtcars, aes(y = mpg, x = wt, color = am))+ geom_point()

#22.) View the am variable and convert it to factor.
mtcars$am
mtcars$am = as.factor(mtcars$am)

#23.) Plot again with new factor variable
ggplot(mtcars, aes(y = mpg, x = wt, color = am))+ geom_point()

#24.) Generate a vector (x) with similar numbers and add it to mtcars
x = c(1:25, 4, 4.1, 4.2, 4.1, 4.2, 4, 5)
mtcars$x = x

#25.) scatter plot with y as cyl and x as mpg. Notice overlapping
ggplot(mtcars, aes(y = cyl, x = x))+ geom_point()

#26.) Add am as color to the scatter plot to test overlappping
ggplot(mtcars, aes(y = cyl, x = x, color = am))+ geom_point()

#27.) change alpha to .3 to test overlapping
ggplot(mtcars, aes(y = cyl, x = x, color = am))+ geom_point( alpha = .3)

#28.) Add jitter to test overlapping
ggplot(mtcars, aes(y = cyl, x = x)) + geom_jitter()

#29.) Create a final plot by changing labels and scheme
ggplot(mtcars, aes(y = mpg, x = wt, color = am))+ geom_point() + 
  labs(title = "Relationship between Weight and Miles Per Gallon", x = "Car Weight", y = "Miles Per Gallon", color = "Auto Transmition?") +
  scale_color_manual(labels = c("Yes", "No"), values = c("blue", "red")) +
  theme(panel.background = element_blank())

