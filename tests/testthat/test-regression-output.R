# Separate regression ==================================================================================================
test_that("output_lm_numeric_lm_separate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression.model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_separate"
  )
})

test_that("output_lm_numeric_lm_separate_n_comb", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 4,
      n_combinations = 10,
      timing = FALSE,
      regression.model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_separate_n_comb"
  )
})

test_that("output_lm_categorical_lm_separate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      regression.model = parsnip::linear_reg()
    ),
    "output_lm_categorical_lm_separate"
  )
})

test_that("output_lm_mixed_lm_separate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression.model = parsnip::linear_reg(),
    ),
    "output_lm_mixed_lm_separate"
  )
})

test_that("output_lm_mixed_splines_separate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression.model = parsnip::linear_reg(),
      regression.recipe_func = function(regression.recipe) {
        recipes::step_ns(regression.recipe, recipes::all_numeric_predictors(), deg_free = 2)
      }
    ),
    "output_lm_mixed_splines_separate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_separate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_separate",
      regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
      regression.tune_values = data.frame(tree_depth = c(1, 2)),
      regression.vfold_cv_para = list(v = 2)
    ),
    "output_lm_mixed_decision_tree_cv_separate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_separate_parallel", {
  future::plan("multisession", workers = 2)
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_separate",
      regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
      regression.tune_values = data.frame(tree_depth = c(1, 2)),
      regression.vfold_cv_para = list(v = 2)
    ),
    "output_lm_mixed_decision_tree_cv_separate_parallel"
  )
  future::plan("sequential")
})

test_that("output_lm_mixed_xgboost_separate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_separate",
      regression.model = parsnip::boost_tree(engine = "xgboost", mode = "regression"),
      regression.recipe_func = function(regression.recipe) {
        return(recipes::step_dummy(regression.recipe, recipes::all_factor_predictors()))
      }
    ),
    "output_lm_mixed_xgboost_separate"
  )
})

# Surrogate regression =================================================================================================
test_that("output_lm_numeric_lm_surrogate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      regression.model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_surrogate"
  )
})

test_that("output_lm_numeric_lm_surrogate_n_comb", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 4,
      n_combinations = 10,
      timing = FALSE,
      regression.model = parsnip::linear_reg()
    ),
    "output_lm_numeric_lm_surrogate_n_comb"
  )
})

test_that("output_lm_numeric_lm_surrogate_reg_surr_n_comb", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 4,
      n_combinations = 10,
      timing = FALSE,
      regression.model = parsnip::linear_reg(),
      regression.surrogate_n_comb = 8
    ),
    "output_lm_numeric_lm_surrogate_reg_surr_n_comb"
  )
})

test_that("output_lm_categorical_lm_surrogate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 2,
      timing = FALSE,
      regression.model = parsnip::linear_reg()
    ),
    "output_lm_categorical_lm_surrogate"
  )
})

test_that("output_lm_mixed_lm_surrogate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_surrogate",
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      regression.model = parsnip::linear_reg()
    ),
    "output_lm_mixed_lm_surrogate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_surrogate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_surrogate",
      regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
      regression.tune_values = data.frame(tree_depth = c(1, 2)),
      regression.vfold_cv_para = list(v = 2)
    ),
    "output_lm_mixed_decision_tree_cv_surrogate"
  )
})

test_that("output_lm_mixed_xgboost_surrogate", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE,
      approach = "regression_surrogate",
      regression.model = parsnip::boost_tree(engine = "xgboost", mode = "regression"),
      regression.recipe_func = function(regression.recipe) {
        recipes::step_dummy(regression.recipe, recipes::all_factor_predictors())
      }
    ),
    "output_lm_mixed_xgboost_surrogate"
  )
})
