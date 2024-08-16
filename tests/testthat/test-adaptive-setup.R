test_that("adaptive_arguments are respected", {
  ex <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    prediction_zero = p0,
    max_n_combinations = 30,
    adaptive_arguments = list(
      initial_n_combinations = 7,
      convergence_tolerance = 0.001,
      reduction_factor_vec = rep(10^(-5), 10),
      max_iter = 8
    ),
    adaptive = TRUE,
    print_shapleyres = FALSE,
    print_iter_info = FALSE
  )

  # Check that initial_n_combinations is respected
  expect_equal(ex$internal$iter_list[[1]]$X[, .N], 7)

  # Check that max_iter is respected
  expect_equal(length(ex$internal$iter_list), 8)
  expect_true(ex$internal$iter_results$iter_info_dt[.N, converged_max_iter])
})


test_that("adaptive feature wise and groupwise computations identical", {
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
    prediction_zero = p0,
    adaptive_arguments = list(
      initial_n_combinations = 5,
      convergence_tolerance = 0.1
    ),
    adaptive = TRUE)


  expl_group <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      group = groups,
      prediction_zero = p0,
      adaptive_arguments = list(
        initial_n_combinations = 5,
        convergence_tolerance = 0.1
      ),
      adaptive = TRUE)


  # Checking equality in the list with all final and intermediate results
  expect_equal(expl_feat$internal$iter_results,expl_group$internal$iter_results)
})

