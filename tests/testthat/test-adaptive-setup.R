
test_that("adaptive_arguments are respected", {

  ex <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    prediction_zero = p0,
    n_combinations=10,
    adaptive_arguments = list(initial_n_combinations = 17,
                              convergence_tolerance = 0.001,
                              reduction_factor_vec = rep(10^(-5),10),
                              max_iter = 8),
    adaptive = TRUE,
    print_shapleyres = FALSE,
    print_iter_info = FALSE
  )

  # Check that initial_n_combinations is respected
  expect_equal(ex$internal$iter_list[[1]]$X[,.N],17)

  # Check that max_iter is respected
  expect_equal(length(ex$internal$iter_list),8)
  expect_true(ex$internal$iter_results$iter_info_dt[.N,converged_max_iter])

})

