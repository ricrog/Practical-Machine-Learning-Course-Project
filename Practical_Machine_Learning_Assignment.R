## Practical Machine Learning Final Assignment
## Author: RIccardo Roganti
## Date: 17/05/2017

## Load required packages
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)

## Download data
setwd("~/Data Science/Coursera/Practical Machine Learning/Final Assignment")
if(!file.exists("pml-training.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(fileUrl,destfile="./pml-training.csv")
}
if(!file.exists("pml-testing.csv")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(fileUrl,destfile="./pml-testing.csv")
}

## Load data
training <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

## Cleaning of data

# Selection of variables which are not NA
count_NA <- sapply(test, function(y) sum((is.na(y))))
NA_values <- count_NA[count_NA == 20]
var_remove <- names(NA_values)

training <- training[,!(names(training) %in% var_remove)]
test <- test[,!(names(test) %in% var_remove)]

# Remove other useless variables
var_remove2 <- c('user_name','raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window', 'X')

training <- training[,!(names(training) %in% var_remove2)]
test <- test[,!(names(test) %in% var_remove2)]

## Apply Machine Learning Algorithm

# Partition
partition <- createDataPartition(training$classe, p = 0.6, list = FALSE)
sub_train <- training[partition,]
sub_test <- training[-partition,]
set.seed(18)

# Random forest
mod_rf <- randomForest(classe ~ ., data = sub_train)
pred_rf <- predict(mod_rf, sub_test, type = "class")
cm_rf <- confusionMatrix(predictionB1, sub_test$classe)

# Generalized boosted regression
model_gbm <- train(classe ~ ., data = sub_train, trControl = trainControl(method = "cv", number = 5), method='gbm')
pred_gbm <- predict(model_gbm, sub_test)
cm_gbm <- confusionMatrix(pred_gbm, sub_test$classe)

# Decision tree
mod_rpart <- train(classe ~ ., data = sub_train, method = "rpart", trControl = trainControl(method = "cv", number = 5))
pred_rpart <- predict(mod_rpart, sub_test)
cm_rpart <- confusionMatrix(sub_test$classe, pred_rpart)

## Results
cm_rf
cm_gbm
cm_rpart