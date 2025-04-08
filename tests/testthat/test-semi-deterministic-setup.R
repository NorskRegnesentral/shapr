test_that("semi_deterministic_samplign when not paired sampling", {
  expect_snapshot(
    {
      # no regression model passed
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        seed = 1,
        extra_computation_args = list(
          paired_shap_sampling = FALSE,
          semi_deterministic_sampling = TRUE
        )
      )
    },
    error = TRUE
  )
})
