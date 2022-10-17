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

ggplot(bodyfat_df,aes(x=ABDOMEN,y=BODYFAT)) +
  geom_point(size=2, shape=20,color='blue') +
  geom_smooth(method=lm, se=FALSE,color='black')

ggplot(bodyfat_df,aes(x=WEIGHT,y=BODYFAT)) +
  geom_point(size=2, shape=20,color='blue') +
  geom_smooth(method=lm, se=FALSE,color='black')

# Investigating the outlier in BMI and Abdomen
ggplot(bodyfat_df, aes(x=ABDOMEN)) + geom_histogram()

# Removing point 39 because it is an outlier with high leverage
bodyfat_df2 <- bodyfat_df[bodyfat_df$ABDOMEN <140,]

#models without outliers 
model_3 <- lm(BODYFAT ~ WEIGHT + HEIGHT + ABDOMEN, data = bodyfat_df2)
summary(model_3)
#adjusted r squared of 0.7184
#best ajusted r squared of 4 models 
plot(model_3)

model_4 <- lm(BODYFAT ~ WEIGHT + ABDOMEN, data = bodyfat_df2)
summary(model_4)
#adjusted r squared of 0.717


