
#1.) Create 4 vectors: vec, vec2, height, eye_color
vec = c(1, 3, 4, 3, 2, 3, 2, 6, 3)
vec2 = c(0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1)
height = c("Medium", "Medium", "Tall", "Medium", "Short")
eye_color = c("green", "blue", "brown", "blue")

#2.) Examine the class of each vector
class(vec)
class(vec2)
class(height)
class(eye_color)

#3.) Turn height into ordered and create  height_ordered: factor(height, levels = c("Short", "Medium", "Tall"))
height = as.factor(height)
class(height)
height

height_ordered = factor(height, levels = c("Short", "Medium", "Tall"))
height_ordered

#4.) Turn vec2 into a character and integer variable
class(vec2)
vec2
vec2_category = as.character(vec2)
vec2_integer = as.integer(vec2)
class(vec2_category)
vec2_category
class(vec2_integer)
vec2_integer

#5.) Subsetting data vec[1] vec[1:3] height_ordered[1:2]
vec[1]
vec[1:3]
height_ordered[1:2]


#6.) Finding the mean and median vec, vec2, height_ordered, vec2_integer, heigh, eye_color
mean(vec)
median(vec)

mean(vec2)
median(vec2)

mean(height_ordered)
mean(vec2_integer)
median(height)
median(eye_color)

#7.) Get mode for vec, vec2, and height_ordered
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(vec)
getmode(vec2)
getmode(height_ordered)


#8.) Standard deviation and variance of vec and vec2
sd(vec)
sd(vec2)

var(vec)
var(vec2)

#9.) Hand calculate the standard deviation and variance of vec and vec2
sqrt(var(vec))
sd(vec)^2

#10.) set working directory and Import mtcars
setwd("C:\\Users\\mawal\\OneDrive - Binghamton University\\Desktop\\Desktop_Folders\\Teaching\\Forecasting Elections\\Week 2")
mtcars = read.csv("mtcars.csv")

#11.) View the dataset, head, tail, and structure
View(mtcars)
str(mtcars)

#12.) Subsetting mtcars [x,x], [x, ] [ ,x]
mtcars[1, 5]
mtcars[14, ]
mtcars[ , 6]

#13.) Three ways to submit with the same output df$model, df$['model'], df[ , 1]
mtcars$model
mtcars[ , 1]
mtcars["model"]

#14.) Descriptive Statistics of mtcars: wt
mean(mtcars$wt)
median(mtcars[,7])
getmode(mtcars$wt)
var(mtcars["wt"])

#15.) Exporting mtcars write.csv(mtcars, "C:\\Users\\mawal\\Desktop\mtcars_new.csv")
write.csv(mtcars, "C:\\Users\\mawal\\Desktop\mtcars_new.csv")




#############################################################









