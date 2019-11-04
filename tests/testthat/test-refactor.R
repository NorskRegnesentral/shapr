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
  x_test <- as.matrix(head(Boston[, x_var], 2))

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
  expect_equal(e1_new$dt, e1_old$Kshap)

  # Ex 2: Explain predictions (copula)
  e2_new <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))
  e2_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    cond_approach = "copula"
  )
  expect_equal(e2_new$dt, e2_old$Kshap)

  # Ex 3: Explain predictions (empirical, independence):
  empirical_settings <- list(type = "independence", fixed_sigma_vec = 0.1, w_threshold = 0.95)
  e3_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "independence")
  e3_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(e3_new$dt - e3_old$Kshap < 1e-6))

  # Ex 4: Explain predictions (empirical, fixed sigma)
  empirical_settings <- list(type = "fixed_sigma", fixed_sigma_vec = 0.1, w_threshold = 0.95)
  e4_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "fixed_sigma")
  e4_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(abs(e4_new$dt - e4_old$Kshap) < 1e-6))

  # Ex 5: Explain predictions (empirical, AICc)
  empirical_settings <- list(
    type = "AICc_each_k",
    fixed_sigma_vec = 0.1,
    n_samples_aicc = 20,
    eval_max_aicc = 20,
    start_aicc = 0.1,
    w_threshold = 0.95
  )
  e5_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_each_k")
  e5_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(abs(e5_new$dt - e5_old$Kshap) < 1e-4))

  # Ex 6: Explain predictions (empirical, AICc full)
  empirical_settings <- list(
    type = "AICc_full",
    fixed_sigma_vec = 0.1,
    n_samples_aicc = 20,
    eval_max_aicc = 20,
    start_aicc = 0.1,
    w_threshold = 0.95
  )
  e6_new <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_full")
  e6_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    pred_zero = mean(y_train),
    empirical_settings = empirical_settings
  )
  expect_true(all(abs(e6_new$dt - e6_old$Kshap) < 1e-4))

  # -------------------------------------------------------------------

  # Ex 7: Explain combined - empirical and gaussian
  e7_new <- explain(x_test, explainer, approach = c("empirical", rep("gaussian", 3)), prediction_zero = mean(y_train), n_samples = 1e4)
  e7_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("empirical" = 1:5, "Gaussian" = 6:16)
  )
  expect_true(all(abs(e7_new$dt - e7_old$Kshap) < 1e-4))

  # Ex 8: Explain combined II - all gaussian
  e8_new <- explain(x_test, explainer, approach = c(rep("gaussian", 4)), prediction_zero = mean(y_train), n_samples = 1e4)
  e8_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("Gaussian" = 1:16)
  )
  expect_true(all(abs(e8_new$dt - e8_old$Kshap) < 1e-4))

  # Ex 9: Explain combined III - all copula
  e9_new <- explain(x_test, explainer, approach = rep("copula",4), prediction_zero = mean(y_train), n_samples = 1e4)
  e9_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("copula" = 1:16)
  )
  expect_true(all(abs(e9_new$dt - e9_old$Kshap) < 1e-4))

  # Ex 10: gaussian and copula XX (works with seed)
  #e10_new <- explain(x_test, explainer, approach = c(rep("gaussian", 2),rep("copula",2)), prediction_zero = mean(y_train), n_samples = 1e4)
  #e10_old <- compute_kshap(
  #  model = model,
    #l = explainer_orig,
    #noSamp_MC = 1e4,
    #pred_zero = mean(y_train),
    #cond_approach = list("Gaussian" = 1:11, "copula" = 12:16)
  #)
  #expect_true(all(abs(e10_new- e10_old$Kshap) < 1e-4))

  # Ex 11: empirical and gaussian
  e11_new <- explain(x_test, explainer, approach = c(rep("empirical", 2),rep("gaussian",2)), prediction_zero = mean(y_train), n_samples = 1e4)
  e11_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("empirical" = 1:11, "Gaussian" = 12:16)
  )
  expect_true(all(abs(e11_new$dt - e11_old$Kshap) < 1e-4))

  # Ex 12: empirical and copula
  e12_new <- explain(x_test, explainer, approach = c(rep("empirical", 2),rep("copula",2)), prediction_zero = mean(y_train), n_samples = 1e4)
  e12_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("empirical" = 1:11, "copula" = 12:16)
  )
  expect_true(all(abs(e12_new$dt - e12_old$Kshap) < 1e-4))

  # Ex 13: copula and empirical XX (works now)
  e13_new <- explain(x_test, explainer, approach = c(rep("copula",2),rep("empirical", 2)), prediction_zero = mean(y_train), n_samples = 1e4)
  e13_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("copula" = 1:11, "empirical" = 12:16)
  )
  expect_true(all(abs(e13_new$dt - e13_old$Kshap) < 1e-4))

  # Ex 14: gaussian and copula XX (works with seed)
  #e14_new <- explain(x_test, explainer, approach = c(rep("gaussian", 1),rep("copula",3)), prediction_zero = mean(y_train), n_samples = 1e5)
  #e14_old <- compute_kshap(
  #  model = model,
  #  l = explainer_orig,
  #  noSamp_MC = 1e5,
  #  pred_zero = mean(y_train),
  #  cond_approach = list("Gaussian" = 1:5, "copula" = 6:16)
  #)
  #expect_true(all(abs(e14_new - e14_old$Kshap) < 1e-4))

  # Ex 15: empirical and copula
  e15_new <- explain(x_test, explainer, approach = c(rep("empirical", 1),rep("copula",3)), prediction_zero = mean(y_train), n_samples = 1e4)
  e15_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("empirical" = 1:5, "copula" = 6:16)
  )
  expect_true(all(abs(e15_new$dt - e15_old$Kshap) < 1e-4))

  # Ex 16: gaussian and empirical XX (works now)
  e16_new <- explain(x_test, explainer, approach = c(rep("gaussian", 1),rep("empirical",3)), prediction_zero = mean(y_train), n_samples = 1e4)
  e16_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("Gaussian" = 1:5, "empirical" = 6:16)
  )
  expect_true(all(abs(e16_new$dt - e16_old$Kshap) < 1e-4))

  # Ex 17: gaussian and empirical XX (works now!)
  e17_new <- explain(x_test, explainer, approach = c(rep("gaussian", 2),rep("empirical",2)), prediction_zero = mean(y_train), n_samples = 1e4)
  e17_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("Gaussian" = 1:11, "empirical" = 12:16)
  )
  expect_true(all(abs(e17_new$dt - e17_old$Kshap) < 1e-4))

  # Ex 8: Explain combined II - all empirical
  e18_new <- explain(x_test, explainer, approach = c(rep("empirical", 4)), prediction_zero = mean(y_train), n_samples = 1e4)
  e18_old <- compute_kshap(
    model = model,
    l = explainer_orig,
    noSamp_MC = 1e4,
    pred_zero = mean(y_train),
    cond_approach = list("empirical" = 1:16)
  )
  expect_true(all(abs(e18_new$dt - e18_old$Kshap) < 1e-4))

})
