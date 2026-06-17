skip_on_cran()

# Smoke tests for approaches whose full snapshot tests are skipped on macOS due
# to platform-specific RNG differences (see test-regular-output.R). These tests
# run on macOS only and verify that `explain()` completes without errors and
# that the returned Shapley values satisfy the efficiency property.

test_that("output_lm_numeric_arf_runs_on_macos_and_satisfies_efficiency", {
  skip_on_os(c("windows", "linux", "solaris"))
  skip_if_not_installed("arf")

  out <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "arf",
    phi0 = p0,
    seed = 1,
    max_n_coalitions = 8,
    n_MC_samples = 5,
    arf.num_trees = 5,
    arf.max_iters = 2,
    arf.parallel_train = FALSE,
    arf.parallel_gen = FALSE,
    verbose = NULL,
    iterative = FALSE
  )

  expect_equal(rowSums(out$shapley_values_est[, -"explain_id"]), out$pred_explain, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("output_lm_categorical_arf_runs_on_macos_and_satisfies_efficiency", {
  skip_on_os(c("windows", "linux", "solaris"))
  skip_if_not_installed("arf")

  out <- explain(
    testing = TRUE,
    model = model_lm_categorical,
    x_explain = x_explain_categorical,
    x_train = x_train_categorical,
    approach = "arf",
    phi0 = p0,
    seed = 1,
    max_n_coalitions = 8,
    n_MC_samples = 5,
    arf.num_trees = 5,
    arf.max_iters = 2,
    arf.parallel_train = FALSE,
    arf.parallel_gen = FALSE,
    verbose = NULL,
    iterative = FALSE
  )

  expect_equal(rowSums(out$shapley_values_est[, -"explain_id"]), out$pred_explain, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("output_lm_numeric_vaeac_runs_on_macos_and_satisfies_efficiency", {
  skip_on_os(c("windows", "linux", "solaris"))
  skip_if_not_installed("torch")
  skip_if_not_installed("coro")
  skip_if_not(torch::torch_is_installed())

  out <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "vaeac",
    phi0 = p0,
    seed = 1,
    n_MC_samples = 10,
    vaeac.epochs = 4,
    vaeac.n_vaeacs_initialize = 2,
    vaeac.extra_parameters = list(
      vaeac.epochs_initiation_phase = 2,
      vaeac.save_model = FALSE
    ),
    iterative = FALSE
  )

  expect_equal(rowSums(out$shapley_values_est[, -"explain_id"]), out$pred_explain, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("output_lm_categorical_vaeac_runs_on_macos_and_satisfies_efficiency", {
  skip_on_os(c("windows", "linux", "solaris"))
  skip_if_not_installed("torch")
  skip_if_not_installed("coro")
  skip_if_not(torch::torch_is_installed())

  out <- explain(
    testing = TRUE,
    model = model_lm_categorical,
    x_explain = x_explain_categorical,
    x_train = x_train_categorical,
    approach = "vaeac",
    phi0 = p0,
    seed = 1,
    n_MC_samples = 10,
    vaeac.epochs = 4,
    vaeac.n_vaeacs_initialize = 2,
    vaeac.extra_parameters = list(
      vaeac.epochs_initiation_phase = 2,
      vaeac.save_model = FALSE
    ),
    iterative = FALSE
  )

  expect_equal(rowSums(out$shapley_values_est[, -"explain_id"]), out$pred_explain, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("output_lm_mixed_vaeac_runs_on_macos_and_satisfies_efficiency", {
  skip_on_os(c("windows", "linux", "solaris"))
  skip_if_not_installed("torch")
  skip_if_not_installed("coro")
  skip_if_not(torch::torch_is_installed())

  out <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_train_mixed,
    approach = "vaeac",
    phi0 = p0,
    seed = 1,
    n_MC_samples = 10,
    vaeac.epochs = 4,
    vaeac.n_vaeacs_initialize = 2,
    vaeac.extra_parameters = list(
      vaeac.epochs_initiation_phase = 2,
      vaeac.save_model = FALSE
    ),
    iterative = FALSE
  )

  expect_equal(rowSums(out$shapley_values_est[, -"explain_id"]), out$pred_explain, tolerance = 1e-6, ignore_attr = TRUE)
})
