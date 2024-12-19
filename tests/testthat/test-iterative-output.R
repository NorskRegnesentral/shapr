# lm_numeric with different approaches

test_that("output_lm_numeric_independence_reach_exact", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      iterative = TRUE,
      verbose = c("basic", "convergence", "shapley")
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
      phi0 = p0,
      iterative_args = list(
        initial_n_coalitions = 10,
        convergence_tol = 0.1
      ),
      iterative = TRUE,
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
      phi0 = p0,
      iterative_args = list(
        initial_n_coalitions = 10,
        convergence_tol = 0.001,
        n_coal_next_iter_factor_vec = rep(10^(-5), 10),
        max_iter = 8
      ),
      iterative = TRUE,
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
      phi0 = p0,
      max_n_coalitions = 20,
      iterative = TRUE,
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
      phi0 = p0,
      iterative_args = list(
        initial_n_coalitions = 5,
        convergence_tol = 0.1
      ),
      iterative = TRUE,
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
      phi0 = p0,
      iterative_args = list(
        initial_n_coalitions = 10,
        convergence_tol = 0.1
      ),
      iterative = TRUE,
      verbose = c("convergence", "shapley")
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
    phi0 = p0,
    iterative_args = list(
      initial_n_coalitions = 10,
      convergence_tol = 0.001,
      n_coal_next_iter_factor_vec = rep(10^(-5), 10),
      max_iter = 8
    ),
    extra_computation_args = list(
      paired_shap_sampling = FALSE
    ),
    iterative = TRUE,
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
    phi0 = p0,
    iterative_args = list(
      initial_n_coalitions = 10,
      convergence_tol = 0.001,
      n_coal_next_iter_factor_vec = rep(10^(-5), 10),
      max_iter = 5
    ),
    extra_computation_args = list(
      paired_shap_sampling = FALSE
    ),
    iterative = TRUE,
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
      phi0 = p0,
      iterative_args = list(
        initial_n_coalitions = 10,
        convergence_tol = 0.001,
        n_coal_next_iter_factor_vec = rep(10^(-5), 10),
        max_iter = 8
      ),
      extra_computation_args = list(
        paired_shap_sampling = FALSE
      ),
      iterative = TRUE,
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
    phi0 = p0,
    iterative_args = list(
      initial_n_coalitions = 10,
      convergence_tol = 0.001,
      n_coal_next_iter_factor_vec = rep(10^(-5), 10),
      max_iter = 5
    ),
    extra_computation_args = list(
      paired_shap_sampling = FALSE
    ),
    iterative = TRUE,
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
      phi0 = p0,
      iterative_args = list(
        initial_n_coalitions = 10,
        convergence_tol = 0.001,
        n_coal_next_iter_factor_vec = rep(10^(-5), 10),
        max_iter = 8
      ),
      extra_computation_args = list(
        paired_shap_sampling = FALSE
      ),
      iterative = TRUE,
      verbose = NULL,
      prev_shapr_object = e_init_path$saving_path,
      seed = NULL
    ),
    "output_lm_numeric_independence_cont_est_path"
  )

  # Testing equality with the object being run in one go
  expect_equal(e_cont_est_path, full)
})

test_that("output_verbose_1", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      iterative = TRUE,
      verbose = c("basic")
    ),
    "output_verbose_1"
  )
})

test_that("output_verbose_1_3", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      iterative = TRUE,
      verbose = c("basic", "convergence")
    ),
    "output_verbose_1_3"
  )
})

test_that("output_verbose_1_3_4", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      iterative = TRUE,
      verbose = c("basic", "convergence", "shapley")
    ),
    "output_verbose_1_3_4"
  )
})

test_that("output_verbose_1_3_4_5", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      iterative = TRUE,
      verbose = c("basic", "convergence", "shapley", "vS_details")
    ),
    "output_verbose_1_3_4_5"
  )
})


# Just checking that internal$output$dt_samp_for_vS  works for iterative
test_that("output_lm_numeric_independence_keep_samp_for_vS", {
  expect_snapshot_rds(
    (out <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      output_args = list(keep_samp_for_vS = TRUE),
      iterative = TRUE
    )),
    "output_lm_numeric_independence_keep_samp_for_vS"
  )

  expect_false(is.null(out$internal$output$dt_samp_for_vS))
})
