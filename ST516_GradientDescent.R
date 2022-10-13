require(pROC)
require(bestglm)
require(glmnet)
require(car)
require(ISLR)
require(rpart)
require(randomForest)
require(caret)
require(e1071)
require(dplyr)
require(gbm)


setwd("/Users/dikshapaliwal/Documents/GitHub/MidtermProject_516/")

concrete_data <- data.frame(read.csv("concrete_data.csv"))

concrete_data


## splitting the dataset
split = sample(nrow(concrete_data) , nrow(concrete_data)*0.75)
train_data=concrete_data[split,]
test_data=concrete_data[-split,]

train_matrix=model.matrix(concrete_compressive_strength~.,train_data)
test_matrix=model.matrix(concrete_compressive_strength~.,test_data)
grid<-10^seq(3,-4, length = 100)
set.seed(10)


# tune model parameter mtry using caret

control <- trainControl(method="cv", number=5, search="grid")
RNGkind(sample.kind = "Rounding")
set.seed(123)
tunegrid <- expand.grid(n.trees=c(3000,7000,11000),
                        interaction.depth=c(8,10,12),
                        shrinkage=c(0.02,0.02,0.05),
                        n.minobsinnode=c(1,3,5))

gbm_gridsearch <- train(concrete_compressive_strength~.,data=train_data, method="gbm", metric="RMSE", 
                       tuneGrid=tunegrid, trControl=control)

print(gbm_gridsearch)
plot(gbm_gridsearch)


RNGkind(sample.kind = "Rounding")
set.seed(123)
gbm_model <- gbm(concrete_compressive_strength~.,data=train_data, n.trees=3000, 
                 shrinkage = 0.02, interaction.depth = 12, n.minobsinnode=5)
gbm_model
plot(gbm_model)

# predict

pred_gbm<- predict(gbm_model,newdata=test_data)
pred_gbm

#mse
mse.gbm <- mean((test_data$concrete_compressive_strength - pred_gbm)^2) 
mse.gbm

summary(gbm_model)



