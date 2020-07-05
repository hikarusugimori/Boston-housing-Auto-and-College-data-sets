# HW 1 
#Section 01: Due 11:59PM Oct 3 2019
#Section 85: Due 11:59PM Oct 4 2019
#Name:
#Collaborated with:

# 8. (a)
#set your working directory ,fill in your code after this line
library(ISLR)
data(College)

#read in the file College.csv using read.csv()

college <- read.csv("College.csv")
#8. (b)
#Give data frame college rownames
rownames(college) <- college[,1] 
View(college)
college <- college[, -1]
head(college[, 1:5])
# 8. (c)
# Use a negative number to generate a subset with all but one column
# college[, -c(1, 2, 3)]  will generate a subset with all but the first three columns
college <- college[,-1]
View(college)
# i.
summary(college)
# ii.
pairs(college[, 1:10])

# iii.
plot(college$Private, college$Outstate, xlab = "Private University", 
     ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
# iv.
# replicate "No" for the same times as the number of colleges using rep()
Elite <- rep("No",nrow(college))
# change the values in Elite for colleges with proportion of students 
# coming from the top 10% of their high school classes 
# exceeds 50 % to "Yes"
Elite[college$Top10perc >50] <- "Yes"
# as.factor change ELite, a character vector to a factor vector
# (we will touch on factors later in class) 
Elite <- as.factor(Elite)
# add the newly created vector to the college data frame
college <- data.frame(college ,Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", 
     ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
# v.
#I like the summary function because I can see all the data. 
summary(college$Top10perc)
#The output is large and is hard to analyze 
#I also like the $ sign becuse I can focus on one section of the data 
summary(college$Elite)
#I figured out how to plot how many elite vs non-elite there are!
plot(college$Elite)
#It's interesting being able to visualise different data sets with R. 
#A lot of the data is noisy, so I'm very interested in learning how to 
#find patterns in the data by using regressions, for example. 
plot(College$Grad.Rate)
plot(College$Expend)
#Experimenting with different functions on the cheat sheet 
#Most of the visualisations don't make sense to me 
mosaicplot(College$Grad.Rate)
matplot(College$Grad.Rate)
pie(College$Grad.Rate)

# 9.
#load the Auto.csv into a variable called auto using read.csv()
library(ISLR)
data(Auto)
# remove all rows with missing values using na.omit()
Auto <- na.omit(Auto)
#apply the class() function to each column of auto data frame
sapply(Auto, class)

# (a)
Auto <- read.csv("Auto.csv", na.strings = "?")
Auto <- na.omit(Auto)
str(Auto)
#'data.frame':	397 obs. of  9 variables:
#  $ mpg         : num  18 15 18 16 17 15 14 14 14 15 ...
#$ cylinders   : int  8 8 8 8 8 8 8 8 8 8 ...
#$ displacement: num  307 350 318 304 302 429 454 440 455 390 ...
#$ horsepower  : Factor w/ 94 levels "100","102","103",..: 16 34 28 28 23 41 46 45 47 39 ...
#$ weight      : int  3504 3693 3436 3433 3449 4341 4354 4312 4425 3850 ...
#$ acceleration: num  12 11.5 11 12 10.5 10 9 8.5 10 8.5 ...
#$ year        : int  70 70 70 70 70 70 70 70 70 70 ...
#$ origin      : int  1 1 1 1 1 1 1 1 1 1 ...
#$ name        : Factor w/ 304 levels "amc ambassador brougham",..: 49 36 231 14 161 141 54 223 241 2 ...
#I was able to identify the qualitative variable by looking at 
# the class listed in the second column above variables except “horsepower” and “name” are quantitative.

# (b)
summary(Auto[, -c(4, 9)])
#removed column 4 and 9, which are qualitative data
sapply(Auto[, -c(4, 9)], mean)
sapply(Auto[, -c(4, 9)], sd)
# (c)
subset <- Auto[-c(10:85), -c(4,9)]
sapply(subset, range)

# (d)
plot(Auto$year,Auto$acceleration)
#Looks like car acceleration has gradually increased over time
plot(Auto$displacement, Auto$acceleration)
#Lower displcement correlates with higher acceleration



#Nope still get errors changing approach
auto <- read.csv("Auto.csv", na.strings = "?")
auto <- na.omit(auto)
str(auto)
subset <- auto[-c(10:85), -c(4,9)]
auto$cylinders <- as.factor(auto$cylinders)
auto$year <- as.factor(auto$year)
auto$origin <- as.factor(auto$origin)
pairs(auto)
pairs(Auto)
#When plotting all the column pairs, it's pretty clear that there are 
#a variety of combinations that yield positive and negative correlations. 
#It's also clear that that year, origin, and cylinders are relatively noisy when 
#paired up with other columns, whereas for mpg, discplacement, horsepower,
#and weight, the signal/noise ratio is much higher. 

# (e)
#Looking at mpg more closely 
plot(Auto$cylinders, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$weight,Auto$mpg)
#$Looks like lighter vehicles have better gas mileage. 
#Graph shows almost perfect inverse correlation. 
plot(Auto$year,Auto$weight)
#Speaking of weight, looks like the weight of cars gradually declined over time,
#Also the graph shows that the variation in car weights have declined as well. 

# 10.

# (a)
library(MASS)
Boston
?Boston
Boston$chas <- as.factor(Boston$chas)
nrow(Boston)
#[1] 506
ncol(Boston)
#[1] 14

# (b)
library(MASS)
pairs(Boston)
#I was getting lots of error messages like below when trying to use the pairs funtion:
#For this reason I had to use the plot function to plot. 
#Error in deparse(substitute(x)) : 'getCharCE' must be called on a CHARSXP
#Error during wrapup: 'getCharCE' must be called on a CHARSXP and
#Error in deparse(substitute(x)) : 
#  invalid multibyte string at '<e0><fc><80>N<b9>'
#Error during wrapup: invalid multibyte string at '<e0><fc><80>N<b9>'

par(mfrow = c(2, 2))
plot(Boston$black,Boston$nox)
plot(Boston$age,Boston$dis)
plot(Boston$chas,Boston$tax)
plot(Boston$indus,Boston$lstat)

#Attempting to resolve pair function issue with code below
detach("package:MASS", unload=TRUE)
library(MASS)
pairs(Boston)
#Ok it worked!

# (c)
par(mfrow = c(2, 2))
plot(Boston$ptratio, Boston$crim)
#Looks like there is basically no relationship between these variables except there 
#is a spike in crime at ptratio~20. Not sure if it's an outlier. 
plot(Boston$medv, Boston$crim)
#This one is obvious but areas with more expensive residences are 
#associated with lower crime. 
plot(Boston$age, Boston$crim)
#Neighborhoods with older homes have higher crime. 
plot(Boston$dis, Boston$crim)
#The greater the weighted mean of distances to five Boston employment centres,
#the greater the crime rate. 

# (d)

hist(Boston$crim, breaks = 50)
nrow(Boston[Boston$crim > 20, ])
#[1] 18
#Per capita crime rate is mostly around 0~5. 
selection <- subset( Boston, crim > 10)
nrow(selection)/ nrow(Boston)
#[1] 0.1067194
#However, 11% of neighborhoods have a crime rate above 10%. 
hist(Boston$tax, breaks = 50)
nrow(Boston[Boston$tax == 666, ])
#[1] 132
#Mostly 200~400 but there is a huge "spike" at 666. 
nrow(Boston[Boston$ptratio > 20, ])
#[1] 201
hist(Boston$ptratio, breaks = 50)
#Ranges fromn 12 to 22 with a huge spike at 20

library(ggplot2)

# (e)
nrow(subset(Boston, chas ==1)) 
#[1] 35
dim(subset(Boston, chas ==1)) 
#[1] 35 14

# (f)
median(Boston$ptratio)
#[1] 19.05

# (g)
which.min(Boston$medv)
#[1] 399
which.max(Boston$medv)
#[1] 162
selection <- Boston[order(Boston$medv),]
selection[1,]
#This gives me the stats for 399
#       crim zn indus chas   nox    rm age    dis rad tax ptratio black lstat medv
#399 38.3518  0  18.1    0 0.693 5.453 100 1.4896  24 666    20.2 396.9 30.59    5
#Compare to Boston generally 
summary(Boston)
#399 has a extremely high crime rate. The median is 0.25 but 399 is 38.35. 
#399 alsow has relatively high percentage lower status people (lstat)
#Everything else is pretty average. 
#Overall I would not want to live in 399. 

# (h)
nrow(Boston[Boston$rm > 7, ])
## [1] 64
nrow(Boston[Boston$rm > 8, ])
## [1] 13
rm_over_8 <- subset(Boston, rm>8)
nrow(rm_over_8)  
summary(rm_over_8)
summary(Boston)
#Summary shows that crime rate is higher than median, 
#median home value is much higher than median, there are fewer black people, 
#also the pupil to teacher ration is more favorable. 



