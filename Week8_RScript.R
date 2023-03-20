# Week 8 Connect Assignment

# 1
suppressWarnings(RNGversion("3.5.3"))
library(caret)
library(gains)
library(pROC)
library(randomForest)
# Convert into factor variables 
myData$y <- as.factor(myData$y)
# Partition data into 60% training, 40% validation
set.seed(1)
myIndex <- createDataPartition(myData$y, p=0.6, list = FALSE)
trainSet <- myData[myIndex, ]
validationSet <- myData[-myIndex, ]
# Create the bagging tree
set.seed(1)
bagging_tree <- randomForest(y ~ ., data = trainSet, ntree = 100, mtry = 4, importance = TRUE)
# Display information graphically, can set the type = 2 for average decrease in gini impurity index
varImpPlot(bagging_tree, type = 1)
# Create confusion matrix
predicted_class <- predict(bagging_tree, validationSet)
confusionMatrix(predicted_class, validationSet$y, positive = "1")
# Estimate probabilities of cases in validation Set, create gains table comparing
# actual class membership to predicted probability
predicted_prob <- predict(bagging_tree, validationSet, type = "prob")
validationSet$y <- as.numeric(as.character(validationSet$y))
gains_table <- gains(validationSet$y, predicted_prob[, 2])
gains_table
# Using info from gains_table we can plot cumulative lift chart
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$y)) ~ c(0, gains_table$cume.obs), 
     xlab = "# of cases", ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$y))~c(0, dim(validationSet)[1]), col="red", lty=2)
# Create decile wise lift chart
barplot(gains_table$mean.resp/mean(validationSet$y), names.arg=gains_table$depth, 
        xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
# Compute ROC curve as well as AUC value
roc_object <- roc(validationSet$y, predicted_prob[ , 2])
plot.roc(roc_object)
auc(roc_object)
# Convert categorical variable to factor variable
predicted_class_score <- predict(bagging_tree, myScoreData, type = "class")
predicted_class_score
predicted_class_prob <- predict(bagging_tree, myScoreData, type = "prob")
predicted_class_prob

# Create random forest tree
# random forest. Replace the R commands in step e with the following commands:
set.seed(1)
randomforest_tree <- randomForest(HELOC ~., data = trainSet, ntree = 100, mtry = 2, importance = TRUE)
varImpPlot(randomforest_tree, type=1)
predicted_class <- predict(randomforest_tree, validationSet)
confusionMatrix(predicted_class, as.factor(validationSet$HELOC), positive = "1")
predicted_prob <- predict(randomforest_tree, validationSet, type= 'prob')


# 2
myData$Disease <- as.factor(myData$Disease)
set.seed(1)
myIndex <- createDataPartition(myData$Disease, p=0.6, list = FALSE)
trainSet <- myData[myIndex, ]
validationSet <- myData[-myIndex, ]
set.seed(1)
bagging_tree <- randomForest(Disease ~ ., data = trainSet, ntree = 100, mtry = 3, importance = TRUE)
# Display information graphically, can set the type = 2 for average decrease in gini impurity index
varImpPlot(bagging_tree, type = 1)
# Create confusion matrix
predicted_class <- predict(bagging_tree, validationSet)
confusionMatrix(predicted_class, validationSet$Disease, positive = "1")
predicted_prob <- predict(bagging_tree, validationSet, type = "prob")
validationSet$Disease <- as.numeric(as.character(validationSet$Disease))
gains_table <- gains(validationSet$Disease, predicted_prob[, 2])
gains_table
# Using info from gains_table we can plot cumulative lift chart
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Disease)) ~ c(0, gains_table$cume.obs), 
     xlab = "# of cases", ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$Disease))~c(0, dim(validationSet)[1]), col="red", lty=2)
# Create decile wise lift chart
barplot(gains_table$mean.resp/mean(validationSet$Disease), names.arg=gains_table$depth, 
        xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
# Compute ROC curve as well as AUC value
roc_object <- roc(validationSet$Disease, predicted_prob[ , 2])
plot.roc(roc_object)
auc(roc_object)

# Boosting tree
myData <- data.frame(myData)
myData$Disease <- as.factor(myData$Disease)
set.seed(1)
myIndex <- createDataPartition(myData$Disease, p=0.6, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)
boosting_tree <- boosting(Disease ~ ., data = trainSet, mfinal = 100)
prediction <- predict(boosting_tree, validationSet)
confusionMatrix(as.factor(prediction$class), validationSet$Disease, positive = "1")
validationSet$Disease <- as.numeric(as.character(validationSet$Disease))
gains_table <- gains(validationSet$Disease, prediction$prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$Disease)) ~ c(0, gains_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$Disease))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$Disease), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
roc_object <- roc(validationSet$Disease, prediction$prob[,2])
plot.roc(roc_object)
auc(roc_object)


# 3
head(myData)
myData.st <- scale(myData)
head(myData.st)
pca <- prcomp(myData.st)
summary(pca)
pca$rotation
pca$x
newData <- data.frame(myData, pca$x)
head(newData)

# 4 
head(myData)
myData.st <- scale(myData[ , -1])
pca <- prcomp(myData.st)
summary(pca)
pca$rotation
pca$x
newData <- data.frame(myData, pca$x)
head(newData)


