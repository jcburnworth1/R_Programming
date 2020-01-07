## Load required libraries
library(dplyr)
library(rpart)

## Load titanic data to a data frame
titanic_data <- data.frame(Titanic)

## Expand the data to the proper rows
titanic_tree <- titanic_data[1,] %>% slice(rep(1:n(), each = Freq))

for (rows in 1:nrow(titanic_data)) {
    append <- titanic_data[rows,] %>% slice(rep(1:n(), each = Freq))
    titanic_tree <- rbind(titanic_tree, append)
    }

## Remove the frequency column
titanic_tree <- titanic_tree[-5]

## Setup some categorical variables
titanic_tree$Class_1st <- ifelse(titanic_tree$Class == "1st", 1, 0)
titanic_tree$Class_2nd <- ifelse(titanic_tree$Class == "2nd", 1, 0)
titanic_tree$Class_3rd <- ifelse(titanic_tree$Class == "3rd", 1, 0)
titanic_tree$Sex_Numeric <- ifelse(titanic_tree$Sex == "Male", 1, 0)
titanic_tree$Age_Numeric <- ifelse(titanic_tree$Age == "Adult", 1, 0)
titanic_tree$Survived_Numeric <- ifelse(titanic_tree$Survived == "No", 1, 0)

## 90% of the sample size
smp_size <- floor(0.90 * nrow(titanic_tree))

## Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(titanic_tree)), size = smp_size)

train <- titanic_tree[train_ind, ]
test <- titanic_tree[-train_ind, ]

## Begin the decision tree portion ##
## fit_tree the decision tree
fit_tree <- rpart(Survived ~ Class + Sex + Age, method="class", data = train)

## Display the results
printcp(fit_tree)

## Visualize cross-validation results
plotcp(fit_tree)

## Detailed summary of splits
summary(fit_tree) 

## Plot tree 
plot(fit_tree, uniform=TRUE, 
     main = "Classification Tree for Titanic Survivors")
text(fit_tree, use.n=TRUE, all=TRUE, cex=.8)

## Create attractive postscript plot of tree 
# post(fit_tree, file = "~/Desktop/test.ps", 
#      title = "Classification Tree for Titanic Survivors")

## Test Data Prediction
prediction_tree <- predict(fit_tree, newdata = test)

## Bring into test data frame
test <- cbind(test, prediction_tree)
## End the decision tree portion ##

## Begin the logistic regresssion portion ##
## Fit the logistic regression of p(Not Surviving)
fit_log <- glm(Survived_Numeric ~ Class_1st + Class_2nd + Class_3rd + Sex_Numeric + Age_Numeric , data = train, family = binomial(link = "logit"))

## Fit the logistic regression of p(Surviving)
fit_log_2 <- glm((1-Survived_Numeric) ~ Class_1st + Class_2nd + Class_3rd + Sex_Numeric + Age_Numeric , data = train, family = binomial(link = "logit"))

## Summarize the model
summary(fit_log)
summary(fit_log_2)

## Test Data Prediction
not_surviving_logit <- predict.glm(fit_log, newdata = test)
surviving_logit <- predict.glm(fit_log_2, newdata = test)

## Bring into test data frame
test <- cbind(test, not_surviving_logit)
test <- cbind(test, surviving_logit)

## Calculate p(Not Surviving)
test$p_not_surviving_1 <- exp(test$not_surviving_logit) / (1 + exp(test$not_surviving_logit))
test$p_surviving_1 <- 1 / (1 + exp(test$not_surviving_logit))

## Calculate p(Surviving)
test$p_not_surviving_2 <- exp(test$surviving_logit) / (1 + exp(test$surviving_logit))
test$p_surviving_2 <- 1 / (1 + exp(test$surviving_logit))

## Sanity Check
test$sanity_check_1 <- test$p_not_surviving_1 + test$p_surviving_1
test$sanity_check_2 <- test$p_not_surviving_2 + test$p_surviving_2

## End the logistic regresssion portion ##