require(faraway)
require(glmnet)
require(dplyr)
require(leaps)
require(randomForest)
require(caret)
require(rpart)

set.seed(34)


## loading the data set
concrete_data = read.csv("/Users/chintu/Desktop/Course-work/ST516/Concrete_Data.csv")
## about the dataset
dim(concrete_data)
names(concrete_data)
str(concrete_data)
summary(concrete_data)

## checking for NA values
x=sum(is.na(concrete_data))
## correlation 

cor(concrete_data)
boxplot(concrete_data)
pairs(concrete_data)

## splitting the dataset
split = sample(nrow(concrete_data) , nrow(concrete_data)*0.75)
train_data=concrete_data[split,]
test_data=concrete_data[-split,]




### model_matrices 
train_matrix=model.matrix(strength~.,train_data)
test_matrix=model.matrix(strength~.,test_data)
grid<-10^seq(3,-4, length = 100)
set.seed(10)
##### ridge #####
cv_ridge <- cv.glmnet(train_matrix,train_data$strength,alpha=0,lambda = grid)
best_lambda = cv_ridge$lambda.min
best_lambda

ridge_model <- glmnet(train_matrix,train_data$strength,alpha=0,lambda=grid)
ridge_pred <- predict(ridge_model, best_lambda, newx= test_matrix)
MSE_ridge = mean((test_data$strength-ridge_pred)^2)
### MSE for the ridge regression
MSE_ridge
ridge_coeff = predict(cv_ridge,best_lambda,type="coefficients")
## coefficients of ridge
ridge_coeff
length(ridge_coeff[ridge_coeff!= 0])


### lasso ###

cv_lasso <- cv.glmnet(train_matrix,train_data$strength,alpha=1,lambda = grid)
best_lambda = cv_lasso$lambda.min
best_lambda

lasso_model <- glmnet(train_matrix,train_data$strength,alpha=1,lambda=grid)
lasso_pred<- predict(lasso_model, best_lambda, newx= test_matrix)
MSE_lasso = mean((test_data$strength-lasso_pred)^2)
## MSE of lasso regression
MSE_lasso
lasso_coeff = predict(cv_lasso,best_lambda,type="coefficients")
## lasso_coefficients
lasso_coeff
length(lasso_coeff[lasso_coeff!= 0])




