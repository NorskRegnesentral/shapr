# lm_numeric with different approaches

test_that("output_lm_numeric_independence", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_combinations=10,
      timing = FALSE,
      adaptive = TRUE,
      print_shapleyres = TRUE,
      print_iter_info = TRUE
    ),
    "output_lm_numeric_independence"
  )
})

