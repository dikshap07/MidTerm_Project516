#---OLS - Linear Regression Model---#

#Importing all the required packages
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(randomForest)
library(MASS)
library(corrplot)
library(car)

#Read data
concrete_data = read.csv("/Users/ameywaghmode/Documents/NCSU/Subjects/SEM 3/Experimental Statistics for Engineers II/Midterm Project/concrete_Data.csv")
glimpse(concrete_data)
#view the summary to understand more about the data
summary(concrete_data)

#checking any NA values exist or not
anyNA(concrete_data)

#checking outliers for each predictor
boxplot((concrete_data$cement))
boxplot(concrete_data$blast_furnace_slag)
boxplot(concrete_data$fly_ash)
boxplot(concrete_data$water)
boxplot(concrete_data$superplasticizer)
boxplot(concrete_data$coarse_aggregate)
boxplot(concrete_data$fine_aggregate)

#plotting correlation matrix
corrplot(cor(concrete_data), method = "number") #numeric
plot(concrete_data) #graphical

#creating model
ols_model=lm(concrete_compressive_strength~., data=concrete_data)
summary(ols_model)
#checking collinearity
vif(ols_model)
#diagnostic plots
par(mfrow=c(2,2))
plot(ols_model)

#removing non-significant predictors and creating the model again to see its effect on r2
ols_model_1=lm(concrete_compressive_strength~.-coarse_aggregate-fine_aggregate, data=concrete_data)
summary(ols_model_1)
#checking collinearity
vif(ols_model_1)
#diagnostic plots
par(mfrow=c(2,2))
plot(ols_model_1)

#Creating a test data
set.seed(5)
test_data = sample(nrow(concrete_data),200)

#calculating mean squared error for ols_model (all predictors) on test data
predict_strength = predict(ols_model,concrete_data[test_data,])
mean((concrete_data[test_data,]$concrete_compressive_strength-predict_strength)^2)

#calculating mean squared error for ols_model_1 (without non-significant predictors) on test data
predict_strength_1 = predict(ols_model_1,concrete_data[test_data,])
mean((concrete_data[test_data,]$concrete_compressive_strength-predict_strength_1)^2)

#Below calculations are done on ols_model_1 (without non-significant predictors)  due to vif values
#Comparing MSE between training and test data
mean((concrete_data[-test_data,]$concrete_compressive_strength-predict(ols_model_1,concrete_data[-test_data,]))^2)
mean((concrete_data[test_data,]$concrete_compressive_strength-predict(ols_model_1,concrete_data[test_data,]))^2)

#Correlation between response in the test data and the response of ols_model_1
cor(concrete_data[test_data,]$concrete_compressive_strength,predict(ols_model_1,concrete_data[test_data,]))


