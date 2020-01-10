## Simple script for NAV to get started calculating Shapley values for categorical
## variables on housing data set (January 2020)

## install ctree specific branch of shapr package from Github
devtools::install_github(repo = "NorskRegnesentral/shapr",ref = "ctree")

library(xgboost)
library(shapr)
library(data.table)
library(caret)


## Goals:
## 1. Load data set.
## 2. Extract certain columns from testing data set.
## 3. Convert categorical data to dummy variables using library caret.
## 4. Fit xgboost model to training data.
## 5. Using shapr package (and 'ctree' method) to calculate shapley values.
## 6. Extract shapley values.
## 7. Plot shapley values.

## -------------------------------------------

## 1. Load data set.
## Data can be found at:
## https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data
## Log on and download the dataset as train.csv and test.csv and save it in a repository.
## Set the working directory accordingly.

## (Remove this when we send to NAV) -----
if(.Platform$OS.type=="windows"){
  projDir <- "M:"
} else {
  projDir <- "/nr/project/stat"
}

dataDir <- paste(projDir, "BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data/", sep = "/")
setwd(dataDir)
## -------

## read csv
train_data <- read.table(file = "train.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE)
test_data <- read.table(file = "test.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE)


## 2. Extract certain columns from testing data set
x_var <- c("MSSubClass", "LotArea", "Neighborhood", "ExterQual")
y_var <- "SalePrice"

x_train <- train_data[, x_var]
x_test <- test_data[, x_var]

y_train <- train_data[, y_var]

## 3. Convert categorical data to dummy variables using library caret.
dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test), fullRank = TRUE)
x_train_dummy <- predict(dummyfunc, newdata = x_train)
x_test_dummy <- predict(dummyfunc, newdata = x_test)

## 4. Fit xgboost model to training data.
model <- xgboost::xgboost(
  data = x_train_dummy,
  label = y_train,
  nround = 50,
  verbose = FALSE
)
model$dummyfunc <- dummyfunc

## 5. Using shapr package (and 'ctree' method) to calculate shapley values.
explainer <- shapr::shapr(x_train, model)

p <- mean(y_train)

subs <- data.table::as.data.table(x_test)[sample.int(n = nrow(x_test), size = 10),]
explanation <- shapr::explain(
  x = subs,
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE)

## 6. Extract shapley values.
print(explanation$dt)

## 7. Plot shapley values.
## Setting plot_phi0 = FALSE removes the 'none' possibility
shapr:::plot.shapr(explanation, plot_phi0 = FALSE)
