skip_on_cran()


# Separate regression ==================================================================================================
test_that("output_lm_numeric_lm_separate_iterative", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = TRUE
    ),
    "output_lm_numeric_lm_separate_iterative"
  )
})


test_that("output_lm_numeric_lm_separate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_numeric_lm_separate"
  )
})

test_that("output_lm_numeric_lm_separate_n_comb", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      phi0 = p0,
      seed = 1,
      max_n_coalitions = 10,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_numeric_lm_separate_n_comb"
  )
})

test_that("output_lm_categorical_lm_separate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "regression_separate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_categorical_lm_separate"
  )
})

test_that("output_lm_mixed_lm_separate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_mixed_lm_separate"
  )
})

test_that("output_lm_mixed_splines_separate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      regression.recipe_func = function(regression.recipe) {
        recipes::step_ns(regression.recipe, recipes::all_numeric_predictors(), deg_free = 2)
      },
      iterative = FALSE
    ),
    "output_lm_mixed_splines_separate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_separate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      phi0 = p0,
      seed = 1,
      approach = "regression_separate",
      regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
      regression.tune_values = data.frame(tree_depth = c(1, 2)),
      regression.vfold_cv_para = list(v = 2),
      iterative = FALSE
    ),
    "output_lm_mixed_decision_tree_cv_separate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_separate_parallel", {
  testthat::skip_on_cran() # Avoiding CRAN Note: Running R code in ‘testthat.R’ had CPU time 3.6 times elapsed time
  future::plan("multisession", workers = 2)
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      phi0 = p0,
      seed = 1,
      approach = "regression_separate",
      regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
      regression.tune_values = data.frame(tree_depth = c(1, 2)),
      regression.vfold_cv_para = list(v = 2),
      iterative = FALSE
    ),
    "output_lm_mixed_decision_tree_cv_separate_parallel"
  )
  future::plan("sequential")
})

test_that("output_lm_mixed_xgboost_separate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      phi0 = p0,
      seed = 1,
      approach = "regression_separate",
      regression.model = parsnip::boost_tree(engine = "xgboost", mode = "regression"),
      regression.recipe_func = function(regression.recipe) {
        return(recipes::step_dummy(regression.recipe, recipes::all_factor_predictors()))
      },
      iterative = FALSE
    ),
    "output_lm_mixed_xgboost_separate"
  )
})

# Surrogate regression =================================================================================================
test_that("output_lm_numeric_lm_surrogate_iterative", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = TRUE
    ),
    "output_lm_numeric_lm_surrogate_iterative"
  )
})


test_that("output_lm_numeric_lm_surrogate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_numeric_lm_surrogate"
  )
})

test_that("output_lm_numeric_lm_surrogate_n_comb", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      phi0 = p0,
      seed = 1,
      max_n_coalitions = 12,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_numeric_lm_surrogate_n_comb"
  )
})

test_that("output_lm_numeric_lm_surrogate_reg_surr_n_comb", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_surrogate",
      phi0 = p0,
      seed = 1,
      max_n_coalitions = 12,
      regression.model = parsnip::linear_reg(),
      regression.surrogate_n_comb = 8,
      iterative = FALSE
    ),
    "output_lm_numeric_lm_surrogate_reg_surr_n_comb"
  )
})

test_that("output_lm_categorical_lm_surrogate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "regression_surrogate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_categorical_lm_surrogate"
  )
})

test_that("output_lm_mixed_lm_surrogate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_surrogate",
      phi0 = p0,
      seed = 1,
      regression.model = parsnip::linear_reg(),
      iterative = FALSE
    ),
    "output_lm_mixed_lm_surrogate"
  )
})

test_that("output_lm_mixed_decision_tree_cv_surrogate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      phi0 = p0,
      seed = 1,
      approach = "regression_surrogate",
      regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
      regression.tune_values = data.frame(tree_depth = c(1, 2)),
      regression.vfold_cv_para = list(v = 2),
      iterative = FALSE
    ),
    "output_lm_mixed_decision_tree_cv_surrogate"
  )
})

test_that("output_lm_mixed_xgboost_surrogate", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      phi0 = p0,
      seed = 1,
      approach = "regression_surrogate",
      regression.model = parsnip::boost_tree(engine = "xgboost", mode = "regression"),
      regression.recipe_func = function(regression.recipe) {
        recipes::step_dummy(regression.recipe, recipes::all_factor_predictors())
      },
      iterative = FALSE
    ),
    "output_lm_mixed_xgboost_surrogate"
  )
})
