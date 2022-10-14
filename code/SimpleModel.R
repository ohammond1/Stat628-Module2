library(ggplot2)

# Read in CSV for data, need to adjust the directories
setwd("/Users/ohammond/Documents/628/Stat628-Module2/code")

bodyfat_df <- read.csv("../data/BodyFat.csv")

# Look at head and tail
head(bodyfat_df)
tail(bodyfat_df)

summary(bodyfat_df)

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

# Investigating the outlier in BMI and Abdomen
ggplot(bodyfat_df, aes(x=ABDOMEN)) + geom_histogram()


# Removing point 39 because it is an outlier with high leverage
bodyfat_df <- bodyfat_df[bodyfat_df$ABDOMEN <140,]

summary(bodyfat_df)
hist(bodyfat_df$WEIGHT)
hist(bodyfat_df$BODYFAT)

# Data frame with all three and less combination names
combinations <- c("BMI","Height","Ab","Waist","BMI/Weight","BMI/Height",
                  "BMI/Ab","Weight/Height","Weight/Ab","Height/Ab",
                  "BMI/Weight/Height","BMI/Weight/Ab","BMI/Ab/Height",
                  "Ab/Weight/Height")

r_sqr_df <- data.frame("Variables"=combinations,
                       "r_squared"=c(rep(0,14)))

# Create all single variate models
bmi_lm <- lm(BODYFAT ~ ADIPOSITY,data = bodyfat_df)
r_sqr_df[1,2] <- summary(bmi_lm)$adj.r.squared

height_lm <- lm(BODYFAT ~ HEIGHT,data = bodyfat_df)
r_sqr_df[2,2] <- summary(height_lm)$adj.r.squared

abdomen_lm <- lm(BODYFAT ~ ABDOMEN,data = bodyfat_df)
r_sqr_df[3,2] <- summary(abdomen_lm)$adj.r.squared

weight_lm <- lm(BODYFAT ~ WEIGHT,data = bodyfat_df)
r_sqr_df[4,2] <- summary(weight_lm)$adj.r.squared

# Create all two variate models
r_sqr_df[5,2] <- summary(lm(BODYFAT ~ ADIPOSITY+ WEIGHT,data = bodyfat_df))$adj.r.squared

r_sqr_df[6,2] <- summary(lm(BODYFAT ~ ADIPOSITY+ HEIGHT,data = bodyfat_df))$adj.r.squared

r_sqr_df[7,2] <- summary(lm(BODYFAT ~ ADIPOSITY+ ABDOMEN,data = bodyfat_df))$adj.r.squared

r_sqr_df[8,2] <- summary(lm(BODYFAT ~ WEIGHT+ HEIGHT,data = bodyfat_df))$adj.r.squared

r_sqr_df[9,2] <- summary(lm(BODYFAT ~ WEIGHT + ABDOMEN,data = bodyfat_df))$adj.r.squared

r_sqr_df[10,2] <- summary(lm(BODYFAT ~ HEIGHT+ ABDOMEN,data = bodyfat_df))$adj.r.squared

#Create all three variate models
r_sqr_df[11,2] <- summary(lm(BODYFAT ~ ADIPOSITY+ WEIGHT + HEIGHT,data = bodyfat_df))$adj.r.squared

r_sqr_df[12,2] <- summary(lm(BODYFAT ~ ADIPOSITY+ WEIGHT + ABDOMEN,data = bodyfat_df))$adj.r.squared

r_sqr_df[13,2] <- summary(lm(BODYFAT ~ ADIPOSITY+ ABDOMEN + HEIGHT,data = bodyfat_df))$adj.r.squared

r_sqr_df[14,2] <- summary(lm(BODYFAT ~ ABDOMEN + WEIGHT + HEIGHT,data = bodyfat_df))$adj.r.squared

r_sqr_top_df = head(r_sqr_df[order(r_sqr_df$r_squared,decreasing=TRUE),],5)

ggplot(data=r_sqr_top_df,aes(y=Variables,x=r_squared)) + 
    geom_col(fill="cornflowerblue",width=.75) + 
    theme(axis.text.y = element_text(angle = 25, vjust = 0.5,hjust=1)) +
    coord_cartesian(xlim=c(0.7,.725)) +
    xlab("Adjusted R-Squared") +
    labs(title="Model Variables Adjusted R-Squared Comparison") +
    geom_text(aes(label=round(r_squared,3),hjust=-.2))
    

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



bodyfat_df$ABDOMEN_in <- bodyfat_df$ABDOMEN * 0.393701

waist_abd_lm <- lm(BODYFAT ~ ABDOMEN_in + WEIGHT,data = bodyfat_df)
summary(waist_abd_lm)
plot(waist_abd_lm)

ggplot(bodyfat_df,aes(x=ABDOMEN,y=BODYFAT)) +
  geom_point(size=2, shape=20,color='blue') +
  geom_smooth(method=lm, se=FALSE,color='black')

summary(bodyfat_df)
