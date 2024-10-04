# lm_numeric with different approaches

test_that("output_lm_numeric_independence_reach_exact", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      adaptive = TRUE,
      verbose = c("basic", "convergence", "shapley"),
      paired_shap_sampling = TRUE
    ),
    "output_lm_numeric_independence_reach_exact"
  )
})

test_that("output_lm_numeric_independence_converges_tol", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_coalitions = 10,
        convergence_tolerance = 0.1
      ),
      adaptive = TRUE,
      verbose = c("convergence", "shapley")
    ),
    "output_lm_numeric_independence_converges_tol"
  )
})

test_that("output_lm_numeric_independence_converges_maxit", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_coalitions = 10,
        convergence_tolerance = 0.001,
        reduction_factor_vec = rep(10^(-5), 10),
        max_iter = 8
      ),
      adaptive = TRUE,
      verbose = c("convergence", "shapley")
    ),
    "output_lm_numeric_independence_converges_maxit"
  )
})

test_that("output_lm_numeric_indep_conv_max_n_coalitions", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      max_n_coalitions = 20,
      adaptive = TRUE,
      verbose = c("convergence", "shapley")
    ),
    "output_lm_numeric_indep_conv_max_n_coalitions"
  )
})


test_that("output_lm_numeric_gaussian_group_converges_tol", {
  groups <- list(
    A = c("Solar.R", "Wind"),
    B = c("Temp", "Month"),
    C = "Day"
  )

  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      group = groups,
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_coalitions = 5,
        convergence_tolerance = 0.1
      ),
      adaptive = TRUE,
      verbose = c("convergence", "shapley")
    ),
    "output_lm_numeric_gaussian_group_converges_tol"
  )
})

test_that("output_lm_numeric_independence_converges_tol_paired", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_coalitions = 10,
        convergence_tolerance = 0.1
      ),
      adaptive = TRUE,
      verbose = c("convergence", "shapley"),
      paired_shap_sampling = TRUE
    ),
    "output_lm_numeric_independence_converges_tol_paired"
  )
})

test_that("output_lm_numeric_independence_saving_and_cont_est", {
  # Full 8 iteration estimation to compare against
  # Sets seed on the outside + seed = NULL for reproducibility in two-step estimation
  set.seed(123)
  full <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    prediction_zero = p0,
    paired_shap_sampling = FALSE,
    adaptive_arguments = list(
      initial_n_coalitions = 10,
      convergence_tolerance = 0.001,
      reduction_factor_vec = rep(10^(-5), 10),
      max_iter = 8
    ),
    adaptive = TRUE,
    seed = NULL,
    verbose = NULL
  )

  # Testing saving and continuation estimation
  # By setting the seed outside (+ seed= NULL), we should get identical objects when calling explain twice this way
  set.seed(123)
  e_init_object <- explain(
    testing = FALSE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    prediction_zero = p0,
    paired_shap_sampling = FALSE,
    adaptive_arguments = list(
      initial_n_coalitions = 10,
      convergence_tolerance = 0.001,
      reduction_factor_vec = rep(10^(-5), 10),
      max_iter = 5
    ),
    adaptive = TRUE,
    seed = NULL,
    verbose = NULL
  )

  # Continue estimation from the init object
  expect_snapshot_rds(
    e_cont_est_object <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      paired_shap_sampling = FALSE,
      adaptive_arguments = list(
        initial_n_coalitions = 10,
        convergence_tolerance = 0.001,
        reduction_factor_vec = rep(10^(-5), 10),
        max_iter = 8
      ),
      adaptive = TRUE,
      verbose = NULL,
      prev_shapr_object = e_init_object,
      seed = NULL,
    ),
    "output_lm_numeric_independence_cont_est_object"
  )

  # Testing equality with the object being run in one go
  expect_equal(e_cont_est_object, full)


  # Same as above but using the saving_path instead of the shapr object itself #
  set.seed(123)
  e_init_path <- explain(
    testing = FALSE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    prediction_zero = p0,
    paired_shap_sampling = FALSE,
    adaptive_arguments = list(
      initial_n_coalitions = 10,
      convergence_tolerance = 0.001,
      reduction_factor_vec = rep(10^(-5), 10),
      max_iter = 5
    ),
    adaptive = TRUE,
    seed = NULL,
    verbose = NULL
  )

  # Continue estimation from the init object
  expect_snapshot_rds(
    e_cont_est_path <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      paired_shap_sampling = FALSE,
      adaptive_arguments = list(
        initial_n_coalitions = 10,
        convergence_tolerance = 0.001,
        reduction_factor_vec = rep(10^(-5), 10),
        max_iter = 8
      ),
      adaptive = TRUE,
      verbose = NULL,
      prev_shapr_object = e_init_path$internal$parameters$adaptive_arguments$saving_path,
      seed = NULL
    ),
    "output_lm_numeric_independence_cont_est_path"
  )

  # Testing equality with the object being run in one go
  expect_equal(e_cont_est_path, full)
})
