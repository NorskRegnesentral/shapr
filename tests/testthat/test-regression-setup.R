test_that("regression erroneous input: `approach`", {
  set.seed(123)

  expect_snapshot(
    {
      # Include regression_surrogate
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = c("regression_surrogate", "gaussian", "independence", "empirical"),
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Include regression_separate
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = c("regression_separate", "gaussian", "independence", "empirical"),
      )
    },
    error = TRUE
  )
})

test_that("regression erroneous input: `regression_model`", {
  set.seed(123)

  expect_snapshot(
    {
      # no regression model passed
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = NULL
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # not a tidymodels object of class model_spec
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = lm
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # regression_tune_values` must be provided when `regression_model` contains hyperparameters to tune.
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression")
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The tunable parameters and the parameters value do not match
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = data.frame(num_terms = c(1, 2, 3))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The tunable parameters and the parameters value do not match
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = data.frame(tree_depth = c(1, 2, 3), num_terms = c(1, 2, 3))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Provide regression_tune_values but the parameter has allready been specified in the regression_model
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = 2, engine = "rpart", mode = "regression"),
        regression_tune_values = data.frame(tree_depth = c(1, 2, 3))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Provide regression_tune_values but not a model where these are to be used
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_surrogate",
        regression_tune_values = data.frame(tree_depth = c(1, 2, 3))
      )
    },
    error = TRUE
  )
})


test_that("regression erroneous input: `regression_tune_values`", {
  set.seed(123)

  expect_snapshot(
    {
      # Provide hyperparameter values, but hyperparameter has not been declared as a tunable parameter
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = 2, engine = "rpart", mode = "regression"),
        regression_tune_values = as.matrix(data.frame(tree_depth = c(1, 2, 3)))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The regression_tune_values function must return a data.frame
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = function(x) c(1, 2, 3)
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The regression_tune_values function must return a data.frame with correct names
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = function(x) data.frame(wrong_name = c(1, 2, 3))
      )
    },
    error = TRUE
  )
})

test_that("regression erroneous input: `regression_vfold_cv_para`", {
  set.seed(123)

  expect_snapshot(
    {
      # `regression_vfold_cv_para` is not a list
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = data.frame(tree_depth = c(1, 2, 3)),
        regression_vfold_cv_para = 10
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # `regression_vfold_cv_para` is not a named list
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = data.frame(tree_depth = c(1, 2, 3)),
        regression_vfold_cv_para = list(10)
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Unrecognized parameter
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = data.frame(tree_depth = c(1, 2, 3)),
        regression_vfold_cv_para = list(hey = 10)
      )
    },
    error = TRUE
  )
})


test_that("regression erroneous input: `regression_recipe_func`", {
  set.seed(123)

  expect_snapshot(
    {
      # regression_recipe_func is not a function
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_recipe_func = 3
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # regression_recipe_func must output a recipe
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_surrogate",
        regression_recipe_func = function(x) {
          return(2)
        }
      )
    },
    error = TRUE
  )
})

test_that("regression erroneous input: `regression_surr_n_comb`", {
  set.seed(123)

  expect_snapshot(
    {
      # regression_surr_n_comb must be between 1 and 2^n_features - 2
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_surrogate",
        regression_surr_n_comb = 2^ncol(x_explain_numeric) - 1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # regression_surr_n_comb must be between 1 and 2^n_features - 2
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_surrogate",
        regression_surr_n_comb = 0
      )
    },
    error = TRUE
  )
})

test_that("regression erroneous input: `is_python = TRUE`", {
  set.seed(123)
  expect_snapshot(
    {
      # Trying to call from python.
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::linear_reg(),
        is_python = TRUE
      )
    },
    error = TRUE
  )
})
