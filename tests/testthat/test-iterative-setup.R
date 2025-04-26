test_that("iterative_args are respected", {
  ex <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    phi0 = p0,
    seed = 1,
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
    seed = 1,
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
    seed = 1,
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
        seed = 1,
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
        seed = 1,
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
        seed = 1,
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
        seed = 1,
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
        seed = 1,
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
        seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_non_positive)
      )
    },
    error = TRUE
  )
})

test_that("different n_batches gives same/different shapley values for different approaches", {
  skip_if_not_installed("party")

  # approach "empirical" is seed independent
  explain.empirical_n_batches_5 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0,
    seed = 1,
    extra_computation_args = list(min_n_batches = 5, max_batch_size = 10)
  )

  explain.empirical_n_batches_10 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0,
    seed = 1,
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
    seed = 1,
    extra_computation_args = list(min_n_batches = 5, max_batch_size = 10)
  )

  explain.ctree_n_batches_10 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    phi0 = p0,
    seed = 1,
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

# Tests producing output, but also other aspects below here
# These should be run on cran and are therefore placed here instead of in test-iterative-output.R


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
      seed = 1,
      output_args = list(keep_samp_for_vS = TRUE),
      iterative = TRUE
    )),
    "output_lm_numeric_independence_keep_samp_for_vS"
  )

  expect_false(is.null(out$internal$output$dt_samp_for_vS))
})


test_that("output_verbose_suppressMessages", {
  # Test that the verbose argument works with suppressMessages
  expect_silent({
    suppressMessages({
      ex <- explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        iterative = TRUE,
        verbose = c("basic", "convergence", "shapley", "vS_details")
      )
    })
  })
})

test_that("output_verbose_NULL", {
  # Test that the verbose argument works with suppressMessages
  expect_silent({
    ex <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      seed = 1,
      iterative = TRUE,
      verbose = NULL
    )
  })
})
