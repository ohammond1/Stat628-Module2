#clear environment 
rm(list = ls())

#load packages 
library(ggplot2)

#load data set  
bodyfat_df <- read.csv("BodyFat.csv")

# Look at head and tail
head(bodyfat_df)
tail(bodyfat_df)

# This is still below 0 so will have to drop this row
bodyfat_df <- bodyfat_df[bodyfat_df$BODYFAT > 0,]

# Fixing height that is off using BMI
# Formula for height =sqrt(703*weight/BMI)
bodyfat_df$HEIGHT[bodyfat_df$HEIGHT == 29.5] <-
  sqrt(703*bodyfat_df$WEIGHT[bodyfat_df$HEIGHT == 29.5] / 
         bodyfat_df$ADIPOSITY[bodyfat_df$HEIGHT == 29.5])

# Removing point 39 because it is an outlier with high leverage
bodyfat_df <- bodyfat_df[bodyfat_df$ABDOMEN <140,]

# Convert Abdomen measurement from centimeters to inches
bodyfat_df$ABDOMEN <- bodyfat_df$ABDOMEN* 0.393701

summary(bodyfat_df)

# Investigating the outlier in BMI and Abdomen
ggplot(bodyfat_df, aes(x=ABDOMEN)) + geom_histogram()

#models without outliers 
model_1 <- lm(BODYFAT ~ WEIGHT + HEIGHT + ABDOMEN, data = bodyfat_df)
summary(model_1)
#adjusted r squared of 0.7184
#best adjusted r squared of 4 models 

model_2 <- lm(BODYFAT ~ WEIGHT + ABDOMEN, data = bodyfat_df)
summary(model_2)
plot(model_2)
#adjusted r squared of 0.717

#find confidence interval
confint(model_2)

#test model variability 
n = dim(bodyfat_df)[1]
cook = cooks.distance(model_2)
plot(1:n, cook)
#shows 2 points that could give influence 

#test predictions 
#min values 
test <- list(WEIGHT = min(bodyfat_df$WEIGHT), ABDOMEN = min(bodyfat_df$ABDOMEN))
predict.lm(model_2, test)
min(bodyfat_df$BODYFAT)
#shows 5% body fat, min value in data set was 1.9, not too unreasonable for model

#max values 
test2 <- list(WEIGHT = max(bodyfat_df$WEIGHT), ABDOMEN = max(bodyfat_df$ABDOMEN))
predict.lm(model_2, test2)
max(bodyfat_df$BODYFAT)
#shows 39% body fat was 45.1, again not too unreasonable 

#try with extreme values 
# large
test3 <- list(WEIGHT = 300, ABDOMEN = 60 )
predict.lm(model_2, test3)
# gave prediction of 58.11% 

#extremely large
test4 <- list(WEIGHT = 350, ABDOMEN = 70 )
predict.lm(model_2, test4)
# gave prediction of 74.89% 

#small
test5 <- list(WEIGHT = 110 , ABDOMEN = 22)
predict.lm(model_2, test5)
#gave prediction of -5.6%

#very small
test6 <- list(WEIGHT = 75, ABDOMEN = 20)
predict.lm(model_2, test6)
#gave prediction of -5.88%
