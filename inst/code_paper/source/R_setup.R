library(xgboost)
library(data.table)
library(shapr)

path_data_and_model <- "data_and_models"
path_output <- "output"

x_explain <- fread(file.path(path_data_and_model, "x_explain.csv"))
x_train <- fread(file.path(path_data_and_model, "x_train.csv"))
y_train <- unlist(fread(file.path(path_data_and_model, "y_train.csv")))
model <- readRDS(file.path(path_data_and_model, "model.rds"))


# We compute the SHAP values for the test data.
library(future)
library(progressr)
future::plan(multisession, workers = 4)
progressr::handlers(global = TRUE)

