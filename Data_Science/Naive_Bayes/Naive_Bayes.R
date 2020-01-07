##### Naive Bayes Example #####
## Import libraries
library(naivebayes)

##### Data Prep #####
## Read in the data
location_data <- read.csv('~/Documents/Code/R_Programming/Data_Science/Naive_Bayes/location_data.csv')
 
## Subset the data to 9AM only
where9am <- subset(location_data, subset = hour == 9)[,c(4,7)]

## Prediction Data
thursday9am <- data.frame(where9am[1,1])
saturday9am <- data.frame(where9am[4,1])
weekday_afternoon <- data.frame(location_data[14,c(4,6,7)])
weekday_evening <- data.frame(location_data[19,c(4,6,7)])
weekend_afternoon <- data.frame(location_data[85,c(4,6,7)])

##### Basic Probability Stuff #####
## Basic probability - Using the conditional probability formula below, you can compute the probability
# that Brett is working in the office, given that it is a weekday. P(A|B)=P(A and B)P(B)

## Probabilty of office
# Compute P(A) 
p_A <- nrow(subset(where9am, location == "office")) / nrow(where9am)

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / nrow(where9am)

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, location == "office" & daytype == "weekday")) / nrow(where9am)

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
p_A_given_B

##### Naive Bayes Model - Single Variable #####
## Build the naive bayes model
locmodel <- naive_bayes(location ~ daytype, data = where9am)

# Predict location
predict(locmodel, newdata =  thursday9am)
predict(locmodel, newdata =  saturday9am)

## Exam the model
locmodel

## Obtain the predicted probabilities as opposed to predict location
predict(locmodel, newdata =  thursday9am, type = "prob")

##### Naive Bayes Model - Two Variables #####
# ## Build the naive bayes model
locmodel_2 <- naive_bayes(location ~ daytype + hourtype, data = location_data)

# Predict location
predict(locmodel_2, newdata =  weekday_afternoon)
predict(locmodel_2, newdata =  weekday_evening)

## Exam the model
locmodel_2

## Obtain the predicted probabilities as opposed to predict location
predict(locmodel_2, newdata =  thursday9am, type = "prob")

##### Naive Bayes Model - Laplace Smoothing #####
## Fixes situation where P(something) = 0 like the below to allow a very small chance
predict(locmodel_2, newdata =  weekend_afternoon, type = "prob")

## New model with laplace smoothing
locmodel_3 <- naive_bayes(location ~ daytype + hourtype, data = location_data, laplace = 1)

# Predict location
predict(locmodel_3, newdata =  weekend_afternoon, type = "prob")