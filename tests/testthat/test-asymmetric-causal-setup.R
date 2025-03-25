test_that("asymmetric erroneous input: `causal_ordering`", {
  set.seed(123)

  expect_snapshot(
    {
      # Too many variables (6 does not exist)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(1:6),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too many variables (5 duplicate)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(1:5, 5),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Correct number of variables, but 5 duplicate
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(2:5, 5),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # To few variables
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(1:2, 4),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too many variables (not valid feature name)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Month", "Day", "Invalid feature name"),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too many variables (duplicate)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Month", "Day", "Day"),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Duplicate and missing "Month", but right number of variables
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Day", "Day"),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too few variables
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind"),
        confounding = NULL,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Group Shapley: not giving the group names
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(c("Solar.R", "Wind", "Temp", "Month"), "Day"),
        confounding = NULL,
        approach = "gaussian",
        group = list("A" = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day")),
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Group Shapley: not giving all the group names correctly
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(c("A", "C"), "Wrong name"),
        confounding = NULL,
        approach = "gaussian",
        group = list("A" = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day")),
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Group Shapley: missing a group names
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = TRUE,
        causal_ordering = list(c("A"), "B"),
        confounding = NULL,
        approach = "gaussian",
        group = list("A" = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day")),
        iterative = FALSE
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = c("gaussian", "independence", "empirical", "gaussian"),
        iterative = FALSE
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = c(FALSE, FALSE),
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # String
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = "Must be a single logical",
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Integer
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = 1L,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = TRUE,
        approach = "gaussian",
        iterative = FALSE
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = c("A", "B", "C"),
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # logical vector of incorrect length
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5),
        confounding = c(TRUE, FALSE),
        approach = "gaussian",
        iterative = FALSE
      )
    },
    error = TRUE
  )
})



test_that("cond_sym_as_NULLconfounding", {
  ex_condsym <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    phi0 = p0,
    seed = 1,
    n_MC_samples = 5 # Just for speed
  )

  ex_NULLconfounding <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    phi0 = p0,
    seed = 1,
    asymmetric = FALSE,
    causal_ordering = list(1:2, 3, 4:5),
    confounding = NULL,
    n_MC_samples = 5 # Just for speed
  )

  # When confounding is NULL, causal_ordering is ignored and regular symmetric conditional shapley values is computed
  expect_equal(ex_condsym$shapley_values_est, ex_NULLconfounding$shapley_values_est)
})
