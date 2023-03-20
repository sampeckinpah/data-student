# Week 7 Connect

# 1


# 2
suppressWarnings(RNGversion("3.5.3"))
#Example 10.5
#Import the data from the Q2 of the Connect data file into a data frame (table) and label it myData.
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(forecast)


myData$App <- as.factor(myData$App)
set.seed(1)
myIndex <- createDataPartition(myData$App, p=0.7, list=FALSE)
trainSet <- myData[myIndex, ]
validationSet <- myData[-myIndex, ]
set.seed(1)
default_tree <- rpart(App ~ ., data = trainSet, method = "class")
summary(default_tree)
prp(default_tree, type = 1, extra = 1, under = TRUE)
# Make full tree
set.seed(1)
full_tree <- rpart(App ~., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
# Plot full tree
prp(full_tree, type = 1, extra = 1, under = TRUE)
# Complexity parameter table
printcp(full_tree)
# Create pruned tree, use CP value slightly larger, but still smaller than next tree
pruned_tree <- prune(full_tree, cp = 0.0231215)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)
# Predict cases in validation set, then create a confusion matrix
predicted_class <- predict(pruned_tree, validationSet, type = "class")
confusionMatrix(predicted_class, validationSet$App, positive = "1")
# Probabilities of cases belonging to classes
predicted_prob <- predict(pruned_tree, validationSet, type = 'prob')
head(predicted_prob)
# Create different lift charts
validationSet$App <- as.numeric(as.character(validationSet$App))
# Cumulative lift table
gains_table <- gains(validationSet$App, predicted_prob[,2])
gains_table
# Cumulative lift chart
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$App)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$App))~c(0, dim(validationSet)[1]), col="red", lty=2)
# Decile-wise lift chart
barplot(gains_table$mean.resp/mean(validationSet$App), names.arg=gains_table$depth, 
        xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
# ROC Curve
roc_object <- roc(validationSet$App, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)
# Use Score data for new case prediction
predicted_class_score <- predict(pruned_tree, myScoreData, type = "class")
predicted_class_score
predicted_class_prob <- predict(pruned_tree, myScoreData, type = "prob")
predicted_class_prob

# 3
# In Excel

# 4 Take 2
suppressWarnings(RNGversion("3.5.3"))
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
install.packages("forecast")
library(caret)
library(rpart)
library(rpart.plot)
library(forecast)
myData$Female <- as.factor(myData$Female)
set.seed(1)
myIndex <- createDataPartition(myData$Spending, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)
default_tree <- rpart(Spending ~., data = trainSet, method = "anova")
summary(default_tree)
prp(default_tree, type = 1, extra = 1, under = TRUE)
set.seed(1)
full_tree <- rpart(Spending ~ ., data = trainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)
pruned_tree <- prune(full_tree, cp = 0.021428)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)
predicted_value <- predict(pruned_tree, validationSet)
library(forecast)
accuracy(predicted_value, validationSet$Spending)
myScoreData$Female <- as.factor(myScoreData$Female)
predicted_class_score <- predict(pruned_tree, myScoreData)
predicted_class_score
