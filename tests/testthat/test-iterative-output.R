skip_on_cran()

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
      seed = 1,
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
      seed = 1,
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
      seed = 1,
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
      seed = 1,
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
      seed = 1,
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
      seed = 1,
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

# Tests producing output, but also other aspects below here
# These should be run on cran and are therefore placed here instead of in test-iterative-oupout.R

test_that("output_verbose_1", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      seed = 1,
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
      seed = 1,
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
      seed = 1,
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
      seed = 1,
      iterative = TRUE,
      verbose = c("basic", "convergence", "shapley", "vS_details")
    ),
    "output_verbose_1_3_4_5"
  )
})
