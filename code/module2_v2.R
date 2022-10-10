#clear environment 
rm(list = ls())

#load packages 
library(ggplot2)

#load data set  
bodyfat_df <- read.csv("BodyFat.csv")

# Look at head and tail
head(bodyfat_df)
tail(bodyfat_df)

# Need to see if we can fix the 0 bodyfat by recalculating using "Siri's Equatio
# Bodyfat% = 495/D - 450
4.95/bodyfat_df$DENSITY[bodyfat_df$BODYFAT == 0] - 4.50

# This is still below 0 so will have to drop this row
bodyfat_df <- bodyfat_df[bodyfat_df$BODYFAT > 0,]

# Fixing height that is off using BMI
# Formula for height =sqrt(703*weight/BMI)
bodyfat_df$HEIGHT[bodyfat_df$HEIGHT == 29.5] <-
  sqrt(703*bodyfat_df$WEIGHT[bodyfat_df$HEIGHT == 29.5] / 
         bodyfat_df$ADIPOSITY[bodyfat_df$HEIGHT == 29.5])

summary(bodyfat_df)

#model with height, weight and abdomen
model_1 <- lm(BODYFAT ~ WEIGHT + HEIGHT + ABDOMEN, data = bodyfat_df)
summary(model_1)
#adjusted r squared of 0.7114

#model with weight and abdomen
model_2 <- lm(BODYFAT ~ WEIGHT + ABDOMEN, data = bodyfat_df)
summary(model_2)
#adjusted r squared of 0.712

# Investigating the outlier in BMI and Abdomen
ggplot(bodyfat_df, aes(x=ABDOMEN)) + geom_histogram()

# Removing point 39 because it is an outlier with high leverage
bodyfat_df2 <- bodyfat_df[bodyfat_df$ABDOMEN <140,]

#models without outliers 
model_3 <- lm(BODYFAT ~ WEIGHT + HEIGHT + ABDOMEN, data = bodyfat_df2)
summary(model_3)
#adjusted r squared of 0.7184
#best adjusted r squared of 4 models 

model_4 <- lm(BODYFAT ~ WEIGHT + ABDOMEN, data = bodyfat_df2)
summary(model_4)
plot(model_4)
#adjusted r squared of 0.717
View(bodyfat_df2)
#find confidence interval
confint(model_4)

#test model variability 
n = dim(bodyfat_df2)[1]
cook = cooks.distance(model_4)
plot(1:n, cooki)
#shows 2 points that could give influence 

#test predictions 
#min values 
test <- list(WEIGHT = min(bodyfat_df2$WEIGHT), ABDOMEN = min(bodyfat_df2$ABDOMEN))
predict.lm(model_4, test)
min(bodyfat_df2$BODYFAT)
#shows 5% body fat, min value in data set was 1.9, not too unreasonable for model
 
#max values 
test2 <- list(WEIGHT = max(bodyfat_df2$WEIGHT), ABDOMEN = max(bodyfat_df2$ABDOMEN))
predict.lm(model_4, test2)
max(bodyfat_df2$BODYFAT)
#shows 39% body fat was 45.1, again not too unreasonable 

#try with extreme values 
# large
test3 <- list(WEIGHT = 300, ABDOMEN = 130 )
predict.lm(model_4, test3)
# gave prediction of 37.89% 

#extremely large
test4 <- list(WEIGHT = 350, ABDOMEN = 150 )
predict.lm(model_4, test4)
# gave prediction of 49.79% 

#small
test5 <- list(WEIGHT = 110 , ABDOMEN = 40)
predict.lm(model_4, test5)
#gave prediction of -19%

#very small
test6 <- list(WEIGHT = 75, ABDOMEN = 28 )
predict.lm(model_4, test6)
#gave prediction of -26%
