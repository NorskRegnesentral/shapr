# Separate regression ==================================================================================================
test_that("output_lm_numeric_lm_separate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression_model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_separate"
  )
})

test_that("output_lm_numeric_lm_separate_n_comb", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 4,
      n_combinations = 10,
      timing = FALSE,
      regression_model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_separate_n_comb"
  )
})

test_that("output_lm_categorical_lm_separate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      regression_model = parsnip::linear_reg()
    ),
    "output_lm_categorical_lm_separate"
  )
})

test_that("output_lm_mixed_lm_separate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression_model = parsnip::linear_reg(),
    ),
    "output_lm_mixed_lm_separate"
  )
})

test_that("output_lm_mixed_splines_separate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression_model = parsnip::linear_reg(),
      regression_recipe_func = function(regression_recipe) {
        return(step_ns(regression_recipe, all_numeric_predictors(), deg_free = 2))
      }
    ),
    "output_lm_mixed_splines_separate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_separate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_separate",
      regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
      regression_tune_values = data.frame(tree_depth = c(1, 2)),
      regression_vfold_cv_para = list(v = 2)
    ),
    "output_lm_mixed_lm_separate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_separate_parallelized", {
  future::plan("multisession", workers = 2)
  expect_snapshot_rds(
    {
      explain(
        model = model_lm_mixed,
        x_explain = x_explain_mixed,
        x_train = x_train_mixed,
        prediction_zero = p0,
        n_batches = 4,
        timing = FALSE,
        approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
        regression_tune_values = data.frame(tree_depth = c(1, 2)),
        regression_vfold_cv_para = list(v = 2)
      )
    },
    "output_lm_mixed_decision_tree_cv_separate_parallelized"
  )
  future::plan("sequential")
})

test_that("output_lm_mixed_xgboost_separate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression")
    ),
    "output_lm_mixed_lm_separate"
  )
})

test_that("output_lm_mixed_xgboost_separate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_separate",
      regression_model = parsnip::boost_tree(engine = "xgboost", mode = "regression"),
      regression_recipe_func = function(regression_recipe) {
        return(step_dummy(regression_recipe, all_factor_predictors()))
      }
    ),
    "output_lm_mixed_xgboost_separate"
  )
})

# Surrogate regression =================================================================================================
test_that("output_lm_numeric_lm_surrogate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression_model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_surrogate"
  )
})

test_that("output_lm_numeric_lm_surrogate_n_comb", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 4,
      n_combinations = 10,
      timing = FALSE,
      regression_model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_surrogate_n_comb"
  )
})

test_that("output_lm_numeric_lm_surrogate_regression_surr_n_comb", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 4,
      n_combinations = 10,
      timing = FALSE,
      regression_model = parsnip::linear_reg(),
      regression_surr_n_comb = 10
    ),
    "output_lm_numeric_lm_surrogate_regression_surr_n_comb"
  )
})

test_that("output_lm_categorical_lm_surrogate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression_model = parsnip::linear_reg()
    ),
    "output_lm_categorical_lm_surrogate"
  )
})

test_that("output_lm_mixed_lm_surrogate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      regression_model = parsnip::linear_reg()
    ),
    "output_lm_mixed_lm_surrogate"
  )
})

test_that("output_lm_mixed_splines_surrogate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression_model = parsnip::linear_reg(),
      regression_recipe_func = function(regression_recipe) {
        return(step_ns(recipe, all_numeric_predictors(), -starts_with("mask_"), deg_free = 2))
      }
    ),
    "output_lm_mixed_splines_surrogate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_surrogate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_surrogate",
      regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart", mode = "regression"),
      regression_tune_values = data.frame(tree_depth = c(1, 2)),
      regression_vfold_cv_para = list(v = 2)
    ),
    "output_lm_mixed_lm_surrogate"
  )
})

test_that("output_lm_mixed_xgboost_surrogate", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_surrogate",
      regression_model = parsnip::boost_tree(engine = "xgboost", mode = "regression"),
      regression_recipe_func = function(regression_recipe) {
        return(step_dummy(regression_recipe, all_factor_predictors()))
      }
    ),
    "output_lm_mixed_xgboost_surrogate"
  )
})
