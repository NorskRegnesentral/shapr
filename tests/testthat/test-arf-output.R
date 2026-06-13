skip_on_cran()


test_that("arf parameter validation", {
  expect_error(
    shapr:::check_arf_parameters(
      num_trees = 0,
      min_node_size = 2,
      delta = 0,
      max_iters = 10,
      alpha = 0.1,
      epsilon = 1e-15,
      parallel_train = TRUE,
      parallel_gen = FALSE
    ),
    "arf.num_trees"
  )

  expect_error(
    shapr:::check_arf_parameters(
      num_trees = 10,
      min_node_size = 2,
      delta = 0,
      max_iters = 10,
      alpha = 1.1,
      epsilon = 1e-15,
      parallel_train = TRUE,
      parallel_gen = FALSE
    ),
    "arf.alpha"
  )
})


test_that("arf rejects ranger thread pass-through", {
  expect_error(
    shapr:::check_arf_extra_parameters(list(num.threads = 2)),
    "ranger.num.threads"
  )
})


test_that("output_lm_numeric_arf", {
  skip_if_not_installed("arf")

  expect_snapshot_rds(
    explain(
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
    ),
    "output_lm_numeric_arf"
  )
})
