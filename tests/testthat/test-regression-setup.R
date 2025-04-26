skip_if_not_installed("parsnip")
skip_if_not_installed("recipes")
skip_if_not_installed("workflows")
skip_if_not_installed("rsample")
skip_if_not_installed("tune")
skip_if_not_installed("yardstick")

test_that("regression erroneous input: `approach`", {
  set.seed(123)

  expect_snapshot(
    {
      # Include regression_surrogate
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = c("regression_surrogate", "gaussian", "independence", "empirical"),
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Include regression_separate
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = c("regression_separate", "gaussian", "independence", "empirical"),
        iterative = FALSE
      )
    },
    error = TRUE
  )
})

test_that("regression erroneous input: `regression.model`", {
  set.seed(123)

  expect_snapshot(
    {
      # no regression model passed
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = NULL
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # not a tidymodels object of class model_spec
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = lm
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # regression.tune_values` must be provided when `regression.model` contains hyperparameters to tune.
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression")
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The tunable parameters and the parameters value do not match
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression"),
        regression.tune_values = data.frame(num_terms = c(1, 2, 3))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The tunable parameters and the parameters value do not match
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression"),
        regression.tune_values = data.frame(tree_depth = c(1, 2, 3), num_terms = c(1, 2, 3))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Provide regression.tune_values but the parameter has allready been specified in the regression.model
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = 2, engine = "rpart", mode = "regression"),
        regression.tune_values = data.frame(tree_depth = c(1, 2, 3))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Provide regression.tune_values but not a model where these are to be used
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_surrogate",
        regression.tune_values = data.frame(tree_depth = c(1, 2, 3)),
        iterative = FALSE
      )
    },
    error = TRUE
  )
})


test_that("regression erroneous input: `regression.tune_values`", {
  set.seed(123)

  expect_snapshot(
    {
      # Provide hyperparameter values, but hyperparameter has not been declared as a tunable parameter
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = 2, engine = "rpart", mode = "regression"),
        regression.tune_values = as.matrix(data.frame(tree_depth = c(1, 2, 3)))
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The regression.tune_values function must return a data.frame
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression"),
        regression.tune_values = function(x) c(1, 2, 3)
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # The regression.tune_values function must return a data.frame with correct names
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression"),
        regression.tune_values = function(x) data.frame(wrong_name = c(1, 2, 3))
      )
    },
    error = TRUE
  )
})

test_that("regression erroneous input: `regression.vfold_cv_para`", {
  set.seed(123)

  expect_snapshot(
    {
      # `regression.vfold_cv_para` is not a list
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression"),
        regression.tune_values = data.frame(tree_depth = c(1, 2, 3)),
        regression.vfold_cv_para = 10
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # `regression.vfold_cv_para` is not a named list
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression"),
        regression.tune_values = data.frame(tree_depth = c(1, 2, 3)),
        regression.vfold_cv_para = list(10)
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Unrecognized parameter
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(), engine = "rpart", mode = "regression"),
        regression.tune_values = data.frame(tree_depth = c(1, 2, 3)),
        regression.vfold_cv_para = list(hey = 10)
      )
    },
    error = TRUE
  )
})


test_that("regression erroneous input: `regression.recipe_func`", {
  set.seed(123)

  expect_snapshot(
    {
      # regression.recipe_func is not a function
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_separate",
        regression.recipe_func = 3
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # regression.recipe_func must output a recipe
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_surrogate",
        regression.recipe_func = function(x) {
          return(2)
        },
        iterative = FALSE
      )
    },
    error = TRUE
  )
})

test_that("regression erroneous input: `regression.surrogate_n_comb`", {
  set.seed(123)

  expect_snapshot(
    {
      # regression.surrogate_n_comb must be between 1 and 2^n_features - 2
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_surrogate",
        regression.surrogate_n_comb = 2^ncol(x_explain_numeric) - 1,
        iterative = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # regression.surrogate_n_comb must be between 1 and 2^n_features - 2
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        phi0 = p0,
        seed = 1,
        approach = "regression_surrogate",
        regression.surrogate_n_comb = 0,
        iterative = FALSE
      )
    },
    error = TRUE
  )
})
