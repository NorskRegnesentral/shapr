library(testthat)
library(shapr)

context("test-explanation.R")

# For using same Random numer generator as CircelCI (R version 3.5.x)
if (as.numeric(version$minor) >= 6.0) RNGkind(sample.kind = "Rounding")

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

  # Ex 18: Explain combined II - all empirical
  ex_list[[18]] <- explain(x_test, explainer, approach = c(rep("empirical", 4)), prediction_zero = p0)

  # Ex 19: Explain predictions (ctree, sample = FALSE, default parameters)
  ex_list[[19]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE)

  # Ex 20: Explain predictions (ctree, sample = TRUE, default parameters)
  ex_list[[20]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE)

  # Ex 21: Explain predictions (ctree, sample = FALSE, other ctree parameters)
  ex_list[[21]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE, mincriterion = 0.9, minsplit = 20, minbucket = 25)

  # Ex 22: Explain predictions (ctree, sample = TRUE, other ctree parameters)
  ex_list[[21]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, mincriterion = 0.9, minsplit = 20, minbucket = 25)

  # Ex 22: Explain combined - ctree and gaussian, sample = FALSE
  ex_list[[22]] <- explain(x_test, explainer, approach = c("ctree", rep("gaussian", 3)), prediction_zero = p0, sample = FALSE)

  # Ex 23: Explain combined II - ctree and gaussian, sample = FALSE
  ex_list[[23]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("gaussian", 2)), prediction_zero = p0, sample = FALSE)

  # Ex 24: Explain combined III - ctree and gaussian, sample = FALSE
  ex_list[[24]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("gaussian", 1)), prediction_zero = p0, sample = FALSE)

  # Ex 25: Explain combined IV - ctree all, sample = FALSE
  ex_list[[25]] <- explain(x_test, explainer, approach = c(rep("ctree", 4)), prediction_zero = p0, sample = FALSE)

  # Ex 26: Explain combined - ctree and empirical, sample = FALSE
  ex_list[[26]] <- explain(x_test, explainer, approach = c("ctree", rep("empirical", 3)), prediction_zero = p0, sample = FALSE)

  # Ex 27: Explain combined II - ctree and empirical, sample = FALSE
  ex_list[[27]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("empirical", 2)), prediction_zero = p0, sample = FALSE)

  # Ex 28: Explain combined III - ctree and empirical, sample = FALSE
  ex_list[[28]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("empirical", 1)), prediction_zero = p0, sample = FALSE)

  # Ex 29: Explain combined - ctree and gaussian, sample = TRUE
  ex_list[[29]] <- explain(x_test, explainer, approach = c("ctree", rep("gaussian", 3)), prediction_zero = p0, sample = TRUE)

  # Ex 30: Explain combined II - ctree and gaussian, sample = TRUE
  ex_list[[30]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("gaussian", 2)), prediction_zero = p0, sample = TRUE)

  # Ex 31: Explain combined III - ctree and gaussian, sample = TRUE
  ex_list[[31]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("gaussian", 1)), prediction_zero = p0, sample = TRUE)

  # Ex 32: Explain combined IV - ctree all, sample = TRUE
  ex_list[[32]] <- explain(x_test, explainer, approach = c(rep("ctree", 4)), prediction_zero = p0, sample = TRUE)

  # Ex 33: Explain combined - ctree and empirical, sample = TRUE
  ex_list[[33]] <- explain(x_test, explainer, approach = c("ctree", rep("empirical", 3)), prediction_zero = p0, sample = TRUE)

  # Ex 34: Explain combined II - ctree and empirical, sample = TRUE
  ex_list[[34]] <- explain(x_test, explainer, approach = c(rep("ctree", 2), rep("empirical", 2)), prediction_zero = p0, sample = TRUE)

  # Ex 35: Explain combined III - ctree and empirical, sample = TRUE
  ex_list[[35]] <- explain(x_test, explainer, approach = c(rep("ctree", 3), rep("empirical", 1)), prediction_zero = p0, sample = TRUE)

  # Ex 36: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[36]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, comb_indici = 2, comb_mincriterion = c(0.05, 0.95))

  # Ex 37: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[37]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, comb_indici = 2, comb_mincriterion = c(0.95, 0.95))

  # Ex 38: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[38]] <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, comb_indici = 0, comb_mincriterion = c(0.95, 0.95))

  # Ex 39: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[39]] <- all((explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, comb_indici = 2, comb_mincriterion = c(0.95, 0.95)))$dt ==
    (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, mincriterion = 0.95))$dt)

  # Ex 40: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[40]] <- all((explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, comb_indici = 0, comb_mincriterion = c(0.05, 0.95)))$dt ==
                         (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, mincriterion = rep(0.95, 4)))$dt)

  # Ex 41: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[41]] <- all((explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, comb_indici = 0, comb_mincriterion = c(0.05, 0.95)))$dt ==
                         (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = TRUE, mincriterion = 0.95))$dt)

  # Ex 42: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[42]] <- all((explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE, mincriterion = rep(0.95, 4)))$dt ==
                         (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE, mincriterion = 0.95))$dt)

  # Ex 43: Explain different ctree mincriterion for different number of dependent variables, sample = TRUE
  ex_list[[43]] <- all((explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE, mincriterion = c(rep(0.95, 2), rep(0.92, 2))))$dt ==
                         (explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE, mincriterion = 0.95))$dt)

  # Checking that explanations with different paralellizations gives the same result
  explain_base <- explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE)

  testthat::expect_equal(
    explain_base,
    explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,mc_cores = 2)
  )

  testthat::expect_equal(
    explain_base,
    explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,mc_cores_simulateAllTrees = 1,mc_cores_sample_ctree = 2)
  )

  testthat::expect_equal(
    explain_base,
    explain(x_test, explainer, approach = "ctree", prediction_zero = p0, sample = FALSE,mc_cores_simulateAllTrees = 2,mc_cores_sample_ctree = 1)
  )

  # Checking that all explain objects produce the same as before
  expect_known_value(ex_list, file = "test_objects/explanation_explain_obj_list.rds")
})
