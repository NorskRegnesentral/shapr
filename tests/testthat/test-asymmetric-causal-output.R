# Continuous data -------------------------------------------------------------------------------------------------
test_that("output_asymmetric_conditional", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = NULL,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asymmetric_conditional"
  )
})

test_that("output_asymmetric_conditional_regression", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      regression.model = parsnip::linear_reg(),
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = NULL
    ),
    "output_asymmetric_conditional_regression"
  )
})

test_that("output_symmetric_conditional", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5), # Does not matter when asymmetric = TRUE and confounding = NULL
      confounding = NULL,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_conditional"
  )
})

test_that("output_symmetric_marginal_independence", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_marginal_independence"
  )
})

test_that("output_symmetric_marginal_gaussian", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_marginal_gaussian"
  )
})

test_that("output_asymmetric_causal_confounding_TRUE", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asymmetric_causal_confounding_TRUE"
  )
})

test_that("output_asymmetric_causal_confounding_FALSE", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asymmetric_causal_confounding_FALSE"
  )
})

test_that("output_asymmetric_causal_confounding_mix", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_asymmetric_causal_confounding_mix"
  )
})

test_that("output_asymmetric_causal_confounding_mix_n_comb", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5, # Just for speed
      n_combinations = 6
    ),
    "output_asymmetric_causal_confounding_mix_n_comb"
  )
})

test_that("output_asymmetric_causal_confounding_mix_empirical", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_asymmetric_causal_confounding_mix_empirical"
  )
})

test_that("output_asymmetric_causal_confounding_mix_ctree", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_asymmetric_causal_confounding_mix_ctree"
  )
})

test_that("output_symmetric_causal_confounding_TRUE", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_causal_confounding_TRUE"
  )
})

test_that("output_symmetric_causal_confounding_FALSE", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_causal_confounding_FALSE"
  )
})

test_that("output_symmetric_causal_confounding_mix", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_causal_confounding_mix"
  )
})


## Group-wise  -----------------------------------------------------------------------------------------------------
test_that("output_symmetric_causal_confounding_TRUE_group", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3),
      confounding = TRUE,
      group = list("A" = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day")),
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_causal_confounding_TRUE_group"
  )
})


test_that("output_symmetric_causal_confounding_mix_group", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1, 2, 3),
      confounding = c(TRUE, TRUE, FALSE),
      group = list("A" = c("Solar.R"), B = c("Wind", "Temp"), C = c("Month", "Day")),
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_causal_confounding_mix_group"
  )
})




# Mixed data ------------------------------------------------------------------------------------------------------
test_that("output_mixed_symmetric_causal_confounding_TRUE", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_mixed_symmetric_causal_confounding_TRUE"
  )
})

test_that("output_mixed_asymmetric_causal_confounding_mixed", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_mixed_symmetric_causal_confounding_mixed"
  )
})

test_that("output_mixed_asymmetric_causal_confounding_mixed_2", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(FALSE, TRUE, TRUE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_mixed_symmetric_causal_confounding_mixed_2"
  )
})


test_that("output_mixed_asymmetric_conditional_regression", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      regression.model = parsnip::linear_reg(),
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = NULL
    ),
    "output_mixed_asymmetric_conditional_regression"
  )
})



# Categorical data ------------------------------------------------------------------------------------------------
test_that("output_categorical_asymmetric_causal_mixed_categorical", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "categorical",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(3:4, 2, 1),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_categorical_asymmetric_causal_mixed_categorical"
  )
})

test_that("output_categorical_asymmetric_causal_mixed_ctree", {
  expect_snapshot_rds(
    shapr::explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "ctree",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      asymmetric = FALSE,
      causal_ordering = list(3:4, 2, 1),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_categorical_asymmetric_causal_mixed_ctree"
  )
})
