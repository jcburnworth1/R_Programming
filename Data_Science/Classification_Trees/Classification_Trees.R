##### Classification Trees Example #####
## This code can also be turned into a random forest very quickly
## Import libraries
library(rpart)
library(rpart.plot)

##### Read in the data - Need to adjust this to include outcome #####
loans <- read.csv('~/Documents/Code/R_Programming/Data_Science/Classification_Trees/loans.csv')

## Good credit for prediction
good_credit <- data.frame()
bad_credit <- data.frame()

##### Build a lending model predicting loan outcome versus loan amount and credit score #####
loan_model <- rpart(outcome ~ loan_amount + credit_score,
                    data = loans,
                    method = "class",
                    control = rpart.control(cp = 0))

## Make a prediction for someone with good credit
predict(loan_model, newdata = good_credit, type = "class")

## Make a prediction for someone with bad credit
predict(loan_model, newdata = bad_credit, type = "class")

##### Visualizing the tree #####
## Examine the loan_model object
loan_model

# Plot the loan_model with default settings
rpart.plot(loan_model)

# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

##### Pruning the tree: Pre - Post #####
## Pre - Controlling max depth of the tree
loan_model <- rpart(outcome ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0, maxdepth = 6))

# Run this. How does the accuracy change?
loans_test$pred <- predict(loan_model, loans_test, type = "class")
mean(loans_test$pred == loans_test$outcome)

## Post - Pruning branchs that do not materially improve accuracy
# Examine the complexity plot
plotcp(loan_model)

# Prune the tree
loan_model_pruned <- prune(loan_model, cp = 0.0014)

# Compute the accuracy of the pruned tree
loans_test$pred <- predict(loan_model_pruned, loans_test, type = "class")
mean(loans_test$outcome == loans_test$pred)