## Load the required libraries
library(funModeling)
library(ggplot2)
library(dplyr)

## Read in the data
insurance_charges <- read.csv('~/Downloads/insurance.csv', stringsAsFactors = FALSE, na = '')

## Inspect the data
df_status(insurance_charges)
describe(insurance_charges)

## Exploratory Charts
## Charges by age
ggplot(insurance_charges, aes(x = age, y = charges)) +
  geom_jitter(aes(color = age))

## Charges by Sex
ggplot(insurance_charges, aes(x = sex, y = charges)) +
  geom_jitter(aes(color = sex))

## Charges by BMI
ggplot(insurance_charges, aes(x = bmi, y = charges)) +
  geom_jitter(aes(color = bmi))

## Charges by Children
ggplot(insurance_charges, aes(x = children, y = charges)) +
  geom_jitter(aes(color = children))

## Charges by Smoking Status
ggplot(insurance_charges, aes(x = smoker, y = charges)) +
  geom_jitter(aes(color = smoker))

## Charges by Region
ggplot(insurance_charges, aes(x = region, y = charges)) +
  geom_jitter(aes(color = region))

## Prep Data & Split into train / test
set.seed(123)
n_train <- round(0.8 * nrow(insurance_charges))
train_indices <- sample(1:nrow(insurance_charges), n_train)
train <- insurance_charges[train_indices, ]
test <- insurance_charges[-train_indices, ]

## Model 0
formula_0 <- as.formula('charges ~ age + sex + bmi + children + smoker + region')

model_0 <- lm(formula_0 , data = train)

summary(model_0)

## There are some insignificant variables that will be removed from model_1
formula_1 <- as.formula('charges ~ age + bmi + children + smoker')

model_1 <- lm(formula_1, data = train)

summary(model_1)

## Predict values from model_1
test$prediction <- predict.lm(model_1, newdata = test)

test$diff <- test$charges - test$prediction