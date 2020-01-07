##### Classification Trees Example #####
## This code can also be turned into a random forest very quickly
## Import libraries
library(randomForest)

##### Read in the data - Need to adjust this to include outcome #####
loans <- read.csv('~/Documents/Code/R_Programming/Data_Science/Classification_Trees/loans.csv')

# Build a random forest model
loan_model <- randomForest(outcome ~ ., data = loans_train)

# Compute the accuracy of the random forest
loans_test$pred <- predict(loan_model, loans_test)
mean(loans_test$outcome == loans_test$pred)