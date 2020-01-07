##### Logistic Regression Example #####
## Import libraries
library(pROC)

##### Read in the data #####
donors <- read.csv('~/Documents/Code/R_Programming/Data_Science/Logistic_Regression/donors.csv')

## Examine the dataset to identify potential independent variables
str(donors)

## Explore the dependent variable
table(donors$donated)

##### Build the donation model #####
donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans, 
                         data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)

## Predict outcomes
donors$donation_prob <- predict(donation_model, type = "response")

# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donated == donors$donation_pred)

##### ROC Curve & AUC (Area under the curve) to test model performance #####
## Generally - Closer AUC to 1 the better but shape of curve also quantifies how well the model
## predicts easy vs. difficult examples (for instance)
## Use roc funtion to draw the curve
ROC <- roc(donors$donated, donors$donation_prob)

# Plot the ROC curve
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC) # Not great model performance against random guessing

##### Modeling Interactions #####
# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ recency * frequency + money, data = donors, family = "binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, type = "response")

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = "red")
auc(ROC)

##### Stepwise Model Building #####
# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = "response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)