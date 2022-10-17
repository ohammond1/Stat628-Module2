#clear environment 
rm(list = ls())

#load packages 
library(ggplot2)

# load data set 
# Working Directory should be parent directory of code/images/data
bodyfat_df <- read.csv("./data/BodyFat.csv")

# Look at head and tail, and summary
head(bodyfat_df)
tail(bodyfat_df)

summary(bodyfat_df)

# Need to see if we can fix the 0 bodyfat by recalculating using "Siri's Equatio
# Bodyfat% = 495/D - 450
4.95/bodyfat_df$DENSITY[bodyfat_df$BODYFAT == 0] - 4.50

# Bodyfat still below 0 after recalculating so will have to drop this row
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

# Review Dataset again to make sure things like okay now
summary(bodyfat_df)
hist(bodyfat_df$WEIGHT)
hist(bodyfat_df$BODYFAT)

# Convert the abdomen measurement into inches to match height
bodyfat_df$ABDOMEN_cm <- bodyfat_df$ABDOMEN
bodyfat_df$ABDOMEN <- bodyfat_df$ABDOMEN * 0.393701



# Data frame with all three and less combination names
combinations <- c("BMI","Height","Ab","Weight","BMI/Weight","BMI/Height",
                  "BMI/Ab","Weight/Height","Weight/Ab","Height/Ab",
                  "BMI/Weight/Height","BMI/Weight/Ab","BMI/Ab/Height",
                  "Ab/Weight/Height")

# Create Dataframe to store results
r_sqr_df <- data.frame("Variables"=combinations,
                       "r_squared"=c(rep(0,14)))


# Create all single variate models and store adjusted R-squared
r_sqr_df[1,2] <- summary(lm(BODYFAT ~ ADIPOSITY,data = bodyfat_df))$adj.r.squared
r_sqr_df[2,2] <- summary(lm(BODYFAT ~ HEIGHT,data = bodyfat_df))$adj.r.squared
r_sqr_df[3,2] <- summary(lm(BODYFAT ~ ABDOMEN,data = bodyfat_df))$adj.r.squared
r_sqr_df[4,2] <- summary(lm(BODYFAT ~ WEIGHT,data = bodyfat_df))$adj.r.squared

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

# Look at just the top values
r_sqr_top_df = head(r_sqr_df[order(r_sqr_df$r_squared,decreasing=TRUE),],5)

# Create plot comparing the different models R-squared value
ggplot(data=r_sqr_top_df,aes(y=Variables,x=r_squared)) + 
    geom_col(fill="cornflowerblue",width=.75) + 
    theme(axis.text.y = element_text(angle = 25, vjust = 0.5,hjust=1)) +
    coord_cartesian(xlim=c(0.7,.725)) +
    xlab("Adjusted R-Squared") +
    labs(title="Model Variables Adjusted R-Squared Comparison") +
    geom_text(aes(label=round(r_squared,3),hjust=-.2))

# Simple plot comparing single variables of selected model
ggplot(bodyfat_df,aes(x=WEIGHT,y=BODYFAT)) +
    geom_point(size=2, shape=20,color='blue') +
    geom_smooth(method=lm, se=FALSE,color='black')

# Simple plot comparing single variables of selected model
ggplot(bodyfat_df,aes(x=ABDOMEN,y=BODYFAT)) +
    geom_point(size=2, shape=20,color='blue') +
    geom_smooth(method=lm, se=FALSE,color='black')


# Quick R diagnostic plots to verify assumptions
weight_abd_lm <- lm(BODYFAT ~ ABDOMEN + WEIGHT,data = bodyfat_df)
summary(weight_abd_lm)
plot(weight_abd_lm)

# Calculating Cook's Distance
cooks_dist <- cooks.distance(weight_abd_lm)
cook_df <- data.frame("cook"=cooks_dist,
                      'idno'=bodyfat_df$IDNO)

cook_top_10 = head(cook_df[order(cook_df$cook,decreasing=TRUE),],10)

# Plotting Cook's Distance 
ggplot(data=cook_df, aes(x=idno,y=cook)) + 
    geom_bar(stat='identity',fill="cornflowerblue") +
    xlab("ID Number") +
    ylab("Cook's Distance") +
    labs(title="Cook's Distances for Weight and Abdomen Model") +
    theme_linedraw()


# Plotting fitted vs sqrt standardized residuals
fitted_bodyfat <- fitted(weight_abd_lm)
standard_resids <- rstandard(weight_abd_lm)
residuals_df <- data.frame("Fitted" = fitted_bodyfat,
                           "Std_residual" = standard_resids)

ggplot(residuals_df,aes(x=Fitted,y=sqrt(abs(Std_residual)))) +
    geom_point(size=3, shape=20,color='cornflowerblue') +
    xlab("Fitted Bodyfat %") +
    ylab("Square Root of Standardized Residual") +
    labs(title="Scale-Location for Weight and Abdomen Model") +
    theme_linedraw()


# Plotting QQ Normal Plot
ggplot(residuals_df,aes(sample=Std_residual),color='blue') +
    stat_qq(color='cornflowerblue') +
    stat_qq_line(color='orange2', linetype='dashed') +
    xlab("Theoretical Quantile") +
    ylab("Standardized Residual") +
       labs(title="Normal Q-Q Plot for Weight and Abdomen Model") +
    theme_linedraw()

# Export Model for shinyapp
saveRDS(weight_abd_lm,file="./code/bodyfat_app/weight_abd_model.rda")

