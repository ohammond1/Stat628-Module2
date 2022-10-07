library(ggplot2)

# Read in CSV for data, need to adjust the directories
setwd("/Users/ohammond/Documents/628/Stat628-Module2/code")

bodyfat_df <- read.csv("../data/BodyFat.csv")

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

# Create a simple model with BMI
bmi_lm <- lm(BODYFAT ~ ADIPOSITY,data = bodyfat_df)
summary(bmi_lm)

ggplot(bodyfat_df,aes(x=ADIPOSITY,y=BODYFAT)) +
  geom_point(size=2, shape=20,color='blue') +
  geom_smooth(method=lm, se=FALSE,color='black')


# Create a simple model with Abdomen
waist_lm <- lm(BODYFAT ~ ABDOMEN,data = bodyfat_df)
summary(waist_lm)

ggplot(bodyfat_df,aes(x=ABDOMEN,y=BODYFAT)) +
  geom_point(size=2, shape=20,color='blue') +
  geom_smooth(method=lm, se=FALSE,color='black')

plot(waist_lm)

# Simple Model with BMI and Abdomen
# Not much better than just abdomen
waist_bmi_lm <- lm(BODYFAT ~ ABDOMEN + ADIPOSITY,data = bodyfat_df)
summary(waist_bmi_lm)

ggplot(bodyfat_df,aes(x=ABDOMEN,y=BODYFAT)) +
  geom_point(size=2, shape=20,color='blue') +
  geom_smooth(method=lm, se=FALSE,color='black')

# Investigating the outlier in BMI and Abdomen
ggplot(bodyfat_df, aes(x=ABDOMEN)) + geom_histogram()


# Removing point 39 because it is an outlier with high leverage
bodyfat_df <- bodyfat_df[bodyfat_df$ABDOMEN <140,]

waist_lm <- lm(BODYFAT ~ ABDOMEN,data = bodyfat_df)
summary(waist_lm)

ggplot(bodyfat_df,aes(x=ABDOMEN,y=BODYFAT)) +
  geom_point(size=2, shape=20,color='blue') +
  geom_smooth(method=lm, se=FALSE,color='black')


plot(waist_lm)

