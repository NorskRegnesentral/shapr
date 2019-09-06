library(shapr)
library(testthat)

context("test-refactor.R")

test_that("Test new", {

  # Load data -----------
  data("Boston", package = "MASS")
  x_var <- c("lstat", "rm", "dis", "indus")
  y_var <- "medv"

  x_train <- as.matrix(tail(Boston[, x_var], -6))
  y_train <- tail(Boston[, y_var], -6)
  x_test <- as.matrix(head(Boston[, x_var], 3))

  model <- xgboost::xgboost(
    data = x_train,
    label = y_train,
    nround = 20,
    verbose = FALSE
  )

  # Prepare the data for explanation
  explainer <- shapr(x_train, model)
  explainer_orig <- prepare_kshap(x_train, x_test)

  # Ex 1: Explain predictions (gaussian)
  e1_new <- explain(x_test, explainer, approach = "gaussian", prediction_zero = mean(y_train))
  e1_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    cond_approach = "Gaussian"
  )
  expect_equal(e1_new, e1_old$Kshap)

  # Ex 2: Explain predictions (copula)
  e2_new <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))
  e2_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    cond_approach = "copula"
  )
  expect_equal(e2_new, e2_old$Kshap)

  # Ex 3: Explain predictions (empirical, independence):
  empirical_settings <- list(type = "independence", fixed_sigma_vec = 0.1, w_threshold = 0.95)
  e3_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "independence")
  e3_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(e3_new - e3_old$Kshap < 1e-6))

  # Ex 4: Explain predictions (empirical, fixed sigma)
  empirical_settings <- list(type = "fixed_sigma", fixed_sigma_vec = 0.1, w_threshold = 0.95)
  e4_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "fixed_sigma")
  e4_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(abs(e4_new - e4_old$Kshap) < 1e-6))

  # Ex 5: Explain predictions (empirical, AICc)
  empirical_settings <- list(
    type = "AICc_each_k",
    fixed_sigma_vec = 0.1,
    AICc_no_samp_per_optim = 20,
    AIC_optim_max_eval = 20,
    AIC_optim_startval = 0.1,
    w_threshold = 0.95
  )
  e5_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_each_k", n_samples = 20)
  e5_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(abs(e5_new - e5_old$Kshap) < 1e-4))

  # Ex 6: Explain predictions (empirical, AICc full)
  empirical_settings <- list(
    type = "AICc_full",
    fixed_sigma_vec = 0.1,
    AICc_no_samp_per_optim = 20,
    AIC_optim_max_eval = 20,
    AIC_optim_startval = 0.1,
    w_threshold = 0.95
  )
  e6_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_full", n_samples = 20)
  e6_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(abs(e6_new - e6_old$Kshap) < 1e-4))
})
