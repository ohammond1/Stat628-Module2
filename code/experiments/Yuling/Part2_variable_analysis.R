#modeling & variable importance analysis
set.seed(100) 
index = sample(1:nrow(data), 0.7*nrow(data)) 
train = data[index,] # Create the training data 
test = data[-index,] # Create the test data
dim(train)
dim(test)

#simple linear model
simple_lr = lm(BODYFAT~ ., data=train)
summary(simple_lr)

predictions_train = predict(simple_lr, newdata = train)
sqrt(mean((train$BODYFAT - predictions_train)^2))
predictions_test = predict(simple_lr, newdata = test)
sqrt(mean((test$BODYFAT - predictions_test)^2))

cols_reg = colnames(data)

dummies <- dummyVars(BODYFAT ~ ., data = data[,cols_reg])

train_dummies = predict(dummies, newdata = train[,cols_reg])

test_dummies = predict(dummies, newdata = test[,cols_reg])

print(dim(train_dummies))
print(dim(test_dummies))

x = as.matrix(train_dummies)
y_train = train$BODYFAT

x_test = as.matrix(test_dummies)
y_test = test$BODYFAT

#ridge regression
#Tuning for optimal lambda
lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda ####
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = 0.1258925)
coef(ridge_reg)

#train prediction
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
sqrt(mean((y_train - predictions_train)^2))

#test prediction
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
sqrt(mean((y_test - predictions_test)^2))

#Lasso Regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)
coef(lasso_model)

#test prediction
predictions_train <- predict(lasso_model, s = optimal_lambda, newx = x)
sqrt(mean((y_train - predictions_train)^2))

#test prediction
predictions_test <- predict(lasso_model, s = optimal_lambda, newx = x_test)
sqrt(mean((y_test - predictions_test)^2))

#randomForest
bodyfat.rf <- randomForest(BODYFAT ~ ., data=data,mtry=3, importance=TRUE,na.action=na.omit)
print(bodyfat.rf)

round(importance(bodyfat.rf), 2)
hist(treesize(bodyfat.rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(bodyfat.rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(bodyfat.rf)











