causal_ordering = list(1:2, 3:4, 5)

test_that("asymmetric erroneous input: `causal_ordering`", {
  set.seed(123)

  expect_snapshot(
    {
      # Too many variables (6 does not exist)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list(1:6),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too many variables (5 duplicate)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list(1:5, 5),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Correct number of variables, but 5 duplicate
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list(2:5, 5),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # To few variables
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list(1:2, 4),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too many variables (not valid feature name)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Month", "Day", "FAKE"),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too many variables (duplicate)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Month", "Day", "Day"),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Duplicate + missing "Month"
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Day", "Day"),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too few variables
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind"),
        confounding = NULL,
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Group Shapley: not giving the group names
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list(c("Solar.R", "Wind", "Temp", "Month"), "Day"),
        confounding = NULL,
        approach = "gaussian",
        group = list("A" = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day"))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Group Shapley: missing a group names
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        asymmetric = TRUE,
        causal_ordering = list(c("A"), "B"),
        confounding = NULL,
        approach = "gaussian",
        group = list("A" = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day"))
      )
    },
    error = TRUE
  )
})


test_that("asymmetric erroneous input: `approach`", {
  set.seed(123)

  expect_snapshot(
    {
      # Causal Shapley values is not applicable for combined approaches.
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 3,
        timing = FALSE,
        asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = c("gaussian", "independence", "empirical", "gaussian"),
      )
    },
    error = TRUE
  )
})

test_that("asymmetric erroneous input: `asymmetric`", {
  set.seed(123)

  expect_snapshot(
    {
      # Vector
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 3,
        timing = FALSE,
        asymmetric = c(FALSE, FALSE),
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = "gaussian"
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # String
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 3,
        timing = FALSE,
        asymmetric = "Must be logical",
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = "gaussian"
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Integer
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 3,
        timing = FALSE,
        asymmetric = 1L,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = "gaussian"
      )
    },
    error = TRUE
  )
})


test_that("asymmetric erroneous input: `confounding`", {
  set.seed(123)

  expect_snapshot(
    {
      # confounding not logical vector
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 3,
        timing = FALSE,
        asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = c("A", "B", "C"),
        approach = "gaussian",
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # logical vector of incorrect length
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 3,
        timing = FALSE,
        asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = c(TRUE, FALSE),
        approach = "gaussian",
      )
    },
    error = TRUE
  )
})
