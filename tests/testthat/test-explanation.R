library(testthat)
library(shapr)

context("test-explanation.R")

test_that("Test functions in explanation.R", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  y_train <- tail(Boston[, y_var], 50)
  x_test <- as.matrix(head(Boston[, x_var], 2))

  # Prepare the data for explanation. Path needs to be relative to testthat directory in the package
  explainer <- readRDS(file = "test_objects/shapley_explainer_obj.rds")

  # Creating list with lots of different explainer objects
  p0 <- mean(y_train)
  ex_list <- list()

  # Ex 1: Explain predictions (gaussian)
  ex_list[[1]] <- explain(x_test, explainer, approach = "gaussian", prediction_zero = p0)

  # Ex 2: Explain predictions (copula)
  ex_list[[2]] <- explain(x_test, explainer, approach = "copula", prediction_zero = p0)

  # Ex 3: Explain predictions (empirical, independence):
  ex_list[[3]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "independence")

  # Ex 4: Explain predictions (empirical, fixed sigma)
  ex_list[[4]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "fixed_sigma")

  # Ex 5: Explain predictions (empirical, AICc)
  ex_list[[5]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "AICc_each_k")

  # Ex 6: Explain predictions (empirical, AICc full)
  ex_list[[6]] <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0, type = "AICc_full")

  # Ex 7: Explain combined - empirical and gaussian
  ex_list[[7]] <- explain(x_test, explainer, approach = c("empirical", rep("gaussian", 3)), prediction_zero = p0)

  # Ex 8: Explain combined II - all gaussian
  ex_list[[8]] <- explain(x_test, explainer, approach = c(rep("gaussian", 4)), prediction_zero = p0)

  # Ex 9: Explain combined III - all copula
  ex_list[[9]] <- explain(x_test, explainer, approach = rep("copula", 4), prediction_zero = p0)

  # Ex 10: gaussian and copula XX (works with seed)
  ex_list[[10]] <- explain(x_test, explainer, approach = c(rep("gaussian", 2), rep("copula", 2)), prediction_zero = p0)

  # Ex 11: empirical and gaussian
  ex_list[[11]] <- explain(x_test, explainer, approach = c(rep("empirical", 2), rep("gaussian", 2)), prediction_zero = p0)

  # Ex 12: empirical and copula
  ex_list[[12]] <- explain(x_test, explainer, approach = c(rep("empirical", 2), rep("copula", 2)), prediction_zero = p0)

  # Ex 13: copula and empirical XX (works now)
  ex_list[[13]] <- explain(x_test, explainer, approach = c(rep("copula", 2), rep("empirical", 2)), prediction_zero = p0)

  # Ex 14: gaussian and copula XX (works with seed)
  ex_list[[14]] <- explain(x_test, explainer, approach = c(rep("gaussian", 1), rep("copula", 3)), prediction_zero = p0)

  # Ex 15: empirical and copula
  ex_list[[15]] <- explain(x_test, explainer, approach = c(rep("empirical", 1), rep("copula", 3)), prediction_zero = p0)

  # Ex 16: gaussian and empirical XX (works now)
  ex_list[[16]] <- explain(x_test, explainer, approach = c(rep("gaussian", 1), rep("empirical", 3)), prediction_zero = p0)

  # Ex 17: gaussian and empirical XX (works now!)
  ex_list[[17]] <- explain(x_test, explainer, approach = c(rep("gaussian", 2), rep("empirical", 2)), prediction_zero = p0)

  # Ex 8: Explain combined II - all empirical
  ex_list[[18]] <- explain(x_test, explainer, approach = c(rep("empirical", 4)), prediction_zero = p0)

  # Checking that all explain objects produce the same as before
  expect_known_value(ex_list, file = "test_objects/explanation_explain_obj_list.rds")
})
