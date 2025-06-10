test_that("erroneous input: `response`", {
  set.seed(123)

  # Missing
  expect_snapshot(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = TRUE
      )
    },
    error = TRUE
  )

  # Non numerical
  expect_snapshot(
    {
      y_non_numeric <- rep(c("A", "B", "C"), length.out = nrow(data_train))

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = TRUE,
        response = y_non_numeric
      )
    },
    error = TRUE
  )

  # NA
  expect_snapshot(
    {
      y_NA <- new_vector <- rep(NA, nrow(data_train))

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = TRUE,
        response = y_NA
      )
    },
    error = TRUE
  )

  # Single integer
  expect_snapshot(
    {
      y_single_int <- 2

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = TRUE,
        response = y_single_int
      )
    },
    error = TRUE
  )

  # Wrong length vector
  expect_snapshot(
    {
      y_short <- data_train[[y_var_numeric]][-1]

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = TRUE,
        response = y_short
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `sage`", {
  set.seed(123)

  y_train_numeric <- data_train[[y_var_numeric]]

  # Non logical
  expect_snapshot(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = 3,
        response = y_train_numeric
      )
    },
    error = TRUE
  )

  # List
  expect_snapshot(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = c(TRUE, TRUE),
        response = y_train_numeric
      )
    },
    error = TRUE
  )

  # NULL
  expect_snapshot(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_train_numeric,
        x_train = x_train_numeric,
        approach = "gaussian",
        phi0 = p0,
        seed = 1,
        sage = NULL,
        response = y_train_numeric
      )
    },
    error = TRUE
  )
})
