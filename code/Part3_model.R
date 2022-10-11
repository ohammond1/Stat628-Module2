set.seed(100) 
index = sample(1:nrow(data), 0.7*nrow(data)) 
train = data[index,] # Create the training data 
test = data[-index,] # Create the test data
dim(train)
dim(test)

#model
#simple linear regression
lr_final = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST, data=train)
summary(lr_final)

predictions_train = predict(lr_final, newdata = train)
sqrt(mean((train$BODYFAT - predictions_train)^2))
predictions_test = predict(lr_final, newdata = test)
sqrt(mean((test$BODYFAT - predictions_test)^2))

#ridge Regression
#selecting only 4 variables
cols_reg_final = c("AGE","ADIPOSITY","CHEST","BODYFAT")

dummies_final <- dummyVars(BODYFAT ~ ., data = data[,cols_reg_final])

train_dummies_final = predict(dummies_final, newdata = train[,cols_reg_final])

test_dummies_final = predict(dummies_final, newdata = test[,cols_reg_final])

x_final = as.matrix(train_dummies_final)
y_final_train = train$BODYFAT

x_final_test = as.matrix(test_dummies_final)
y_final_test = test$BODYFAT

ridge_reg_final = glmnet(x_final, y_final_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = 0.1258925)

coef(ridge_reg_final)



