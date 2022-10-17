

create_model <- function() {
  bodyfat_df <- read.csv("./BodyFat.csv")

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
  
  bodyfat_lm <- lm(BODYFAT ~ ABDOMEN + WEIGHT,data = bodyfat_df)
  
  return(bodyfat_lm)
}

model <- create_model()
y <- data.frame("ABDOMEN"=c(85),"WEIGHT" = c(150))

fat_predict <- predict(model, y, interval='prediction')
fat_predict[2]
