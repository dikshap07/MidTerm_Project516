require(faraway)
require(glmnet)
require(dplyr)
require(leaps)
require(randomForest)
require(caret)
require(rpart)

set.seed(34)


## loading the data set
concrete_data = read.csv("/Users/smayanapidugu/Downloads/Concrete_Data.csv")

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

## Random Forest ##

# tune model parameter mtry using caret
control <- trainControl(method="cv", number=5, search="grid")
RNGkind(sample.kind = "Rounding")
set.seed(123)
tunegrid <- expand.grid(mtry=c(1:9))
rf_gridsearch <- train(strength~.,data=train_data, method="rf", metric="RMSE", 
                       tuneGrid=tunegrid, trControl=control)

print(rf_gridsearch)
plot(rf_gridsearch)

# CV pick mtry=4
RNGkind(sample.kind = "Rounding")
set.seed(123)
rf.mod <- randomForest(strength~.,data=train_data,mtry=4, ntree=1000, 
                       importance=T)
rf.mod
plot(rf.mod)

# predict
pred.tree<- predict(rf.mod,newdata=test_data)
pred.tree
#mse
mse.rf <- mean((test_data$strength - pred.tree)^2) 
mse.rf

#variable importance
varImpPlot(rf.mod,type=1,pch=19)



