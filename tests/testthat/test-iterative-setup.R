test_that("iterative_args are respected", {
  ex <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    phi0 = p0,
    max_n_coalitions = 30,
    iterative_args = list(
      initial_n_coalitions = 6,
      convergence_tol = 0.0005,
      n_coal_next_iter_factor_vec = rep(10^(-6), 10),
      max_iter = 8
    ),
    iterative = TRUE
  )

  # Check that initial_n_coalitions is respected
  expect_equal(ex$internal$iter_list[[1]]$X[, .N], 6)

  # Check that max_iter is respected
  expect_equal(length(ex$internal$iter_list), 8)
  expect_true(ex$iterative_results$iter_info_dt[.N, converged_max_iter])
})


test_that("iterative feature wise and groupwise computations identical", {
  groups <- list(
    Solar.R = "Solar.R",
    Wind = "Wind",
    Temp = "Temp",
    Month = "Month",
    Day = "Day"
  )

  expl_feat <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    phi0 = p0,
    iterative_args = list(
      initial_n_coalitions = 5,
      convergence_tol = 0.1
    ),
    iterative = TRUE
  )


  expl_group <- explain(
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
    iterative = TRUE
  )


  # Checking equality in the list with all final and intermediate results
  expect_equal(expl_feat$iter_results, expl_group$iter_results)
})

test_that("erroneous input: `min_n_batches`", {
  set.seed(123)

  # non-numeric 1
  expect_snapshot(
    {
      n_batches_non_numeric_1 <- "bla"
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        extra_computation_args = list(min_n_batches = n_batches_non_numeric_1)
      )
    },
    error = TRUE
  )

  # non-numeric 2
  expect_snapshot(
    {
      n_batches_non_numeric_2 <- TRUE
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        extra_computation_args = list(min_n_batches = n_batches_non_numeric_2)
      )
    },
    error = TRUE
  )

  # non-integer
  expect_snapshot(
    {
      n_batches_non_integer <- 10.5
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        extra_computation_args = list(min_n_batches = n_batches_non_integer)
      )
    },
    error = TRUE
  )

  # length > 1
  expect_snapshot(
    {
      n_batches_too_long <- c(1, 2)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        extra_computation_args = list(min_n_batches = n_batches_too_long)
      )
    },
    error = TRUE
  )

  # NA-numeric
  expect_snapshot(
    {
      n_batches_is_NA <- as.numeric(NA)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        extra_computation_args = list(min_n_batches = n_batches_is_NA)
      )
    },
    error = TRUE
  )

  # Non-positive
  expect_snapshot(
    {
      n_batches_non_positive <- 0
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        extra_computation_args = list(min_n_batches = n_batches_non_positive)
      )
    },
    error = TRUE
  )
})

test_that("different n_batches gives same/different shapley values for different approaches", {
  # approach "empirical" is seed independent
  explain.empirical_n_batches_5 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0,
    extra_computation_args = list(min_n_batches = 5, max_batch_size = 10)
  )

  explain.empirical_n_batches_10 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0,
    extra_computation_args = list(min_n_batches = 10, max_batch_size = 10)
  )

  # Difference in the objects (n_batches and related)
  expect_false(identical(
    explain.empirical_n_batches_5,
    explain.empirical_n_batches_10
  ))
  # Same Shapley values
  expect_equal(
    explain.empirical_n_batches_5$shapley_values_est,
    explain.empirical_n_batches_10$shapley_values_est
  )

  # approach "ctree" is seed dependent
  explain.ctree_n_batches_5 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    phi0 = p0,
    extra_computation_args = list(min_n_batches = 5, max_batch_size = 10)
  )

  explain.ctree_n_batches_10 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    phi0 = p0,
    extra_computation_args = list(min_n_batches = 10, max_batch_size = 10)
  )

  # Difference in the objects (n_batches and related)
  expect_false(identical(
    explain.ctree_n_batches_5,
    explain.ctree_n_batches_10
  ))
  # NEITHER same Shapley values
  expect_false(identical(
    explain.ctree_n_batches_5$shapley_values_est,
    explain.ctree_n_batches_10$shapley_values_est
  ))
})
