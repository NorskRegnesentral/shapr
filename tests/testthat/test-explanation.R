library(testthat)
library(shapr)
library(xgboost)

context("test-explanation.R")

test_that("Test functions in explanation.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

#  x_train <- as.matrix(tail(Boston[, x_var], 50))
  y_train <- tail(Boston[, y_var], 50)
  x_test <- as.matrix(head(Boston[, x_var], 2))

  #setwd("./tests/testthat") # Uncomment when running manually

  # Prepare the data for explanation. Path needs to be relative to testthat directory in the package
  explainer <- readRDS(file = "test_objects/shapley_explainer_obj.rds")

  # Creating list with lots of different explainer objects
  Ex.list <- list()

  # Using Random number generator settings as in R version 3.5.0. Seed set internally for reproducability
#  suppressWarnings(RNGversion("3.5.0")) # Testthat gives the warning for using "non-uniform 'Rounding' sampler". Suppress this to complete test

  # Ex 1: Explain predictions (gaussian)
  Ex.list[[1]] <- explain(x_test, explainer, approach = "gaussian", prediction_zero = mean(y_train))

  # Ex 2: Explain predictions (copula)
  Ex.list[[2]] <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))

  # Ex 3: Explain predictions (empirical, independence):
  Ex.list[[3]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "independence")

  # Ex 4: Explain predictions (empirical, fixed sigma)
  Ex.list[[4]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "fixed_sigma")

  # Ex 5: Explain predictions (empirical, AICc)
  Ex.list[[5]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_each_k")

  # Ex 6: Explain predictions (empirical, AICc full)
  Ex.list[[6]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_full")

  # Ex 7: Explain combined - empirical and gaussian
  Ex.list[[7]] <- explain(x_test, explainer, approach = c("empirical", rep("gaussian", 3)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 8: Explain combined II - all gaussian
  Ex.list[[8]] <- explain(x_test, explainer, approach = c(rep("gaussian", 4)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 9: Explain combined III - all copula
  Ex.list[[9]] <- explain(x_test, explainer, approach = rep("copula",4), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 10: gaussian and copula XX (works with seed)
  Ex.list[[10]] <- explain(x_test, explainer, approach = c(rep("gaussian", 2),rep("copula",2)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 11: empirical and gaussian
  Ex.list[[11]] <- explain(x_test, explainer, approach = c(rep("empirical", 2),rep("gaussian",2)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 12: empirical and copula
  Ex.list[[12]] <- explain(x_test, explainer, approach = c(rep("empirical", 2),rep("copula",2)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 13: copula and empirical XX (works now)
  Ex.list[[13]] <- explain(x_test, explainer, approach = c(rep("copula",2),rep("empirical", 2)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 14: gaussian and copula XX (works with seed)
  Ex.list[[14]] <- explain(x_test, explainer, approach = c(rep("gaussian", 1),rep("copula",3)), prediction_zero = mean(y_train), n_samples = 1e5)

  # Ex 15: empirical and copula
  Ex.list[[15]] <- explain(x_test, explainer, approach = c(rep("empirical", 1),rep("copula",3)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 16: gaussian and empirical XX (works now)
  Ex.list[[16]] <- explain(x_test, explainer, approach = c(rep("gaussian", 1),rep("empirical",3)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 17: gaussian and empirical XX (works now!)
  Ex.list[[17]] <- explain(x_test, explainer, approach = c(rep("gaussian", 2),rep("empirical",2)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Ex 8: Explain combined II - all empirical
  Ex.list[[18]] <- explain(x_test, explainer, approach = c(rep("empirical", 4)), prediction_zero = mean(y_train), n_samples = 1e4)

  # Checking that all explain objects produce the same as before
  expect_known_value(Ex.list,file = "test_objects/explanation_explain_obj_list.rds")

  #Ex_list_org <- readRDS(file = "tests/testthat/test_objects/explanation_explain_obj_list.rds")

})
