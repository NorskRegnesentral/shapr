# lm_numeric with different approaches

test_that("output_lm_numeric_independence", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_independence"
  )
})

test_that("output_lm_numeric_independence_MSEv_Shapley_weights", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      MSEv_uniform_comb_weights = FALSE
    ),
    "output_lm_numeric_independence_MSEv_Shapley_weights"
  )
})

test_that("output_lm_numeric_empirical", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_empirical"
  )
})

test_that("output_lm_numeric_empirical_n_combinations", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      n_combinations = 20,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_empirical_n_combinations"
  )
})

test_that("output_lm_numeric_empirical_independence", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      empirical.type = "independence",
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_empirical_independence"
  )
})

test_that("output_lm_numeric_empirical_AICc_each", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      n_combinations = 8,
      empirical.type = "AICc_each_k",
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_empirical_AICc_each"
  )
})

test_that("output_lm_numeric_empirical_AICc_full", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      n_combinations = 8,
      empirical.type = "AICc_full",
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_empirical_AICc_full"
  )
})

test_that("output_lm_numeric_gaussian", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_gaussian"
  )
})

test_that("output_lm_numeric_copula", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "copula",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_copula"
  )
})

test_that("output_lm_numeric_ctree", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "ctree",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_ctree"
  )
})

test_that("output_lm_numeric_vaeac", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "vaeac",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      n_samples = 10, # Low value here to speed up the time
      vaeac.epochs = 4, # Low value here to speed up the time
      vaeac.n_vaeacs_initialize = 2, # Low value here to speed up the time
      vaeac.extra_parameters = list(
        vaeac.epochs_initiation_phase = 2, # Low value here to speed up the time
        vaeac.save_model = FALSE # Removes names and objects such as tmpdir and tmpfile
      )
    ),
    "output_lm_numeric_vaeac"
  )
})

test_that("output_lm_categorical_ctree", {
  expect_snapshot_rds(
    explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "ctree",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_categorical_ctree"
  )
})

test_that("output_lm_categorical_vaeac", {
  expect_snapshot_rds(
    explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "vaeac",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      n_samples = 10, # Low value here to speed up the time
      vaeac.epochs = 4, # Low value here to speed up the time
      vaeac.n_vaeacs_initialize = 2, # Low value here to speed up the time
      vaeac.extra_parameters = list(
        vaeac.epochs_initiation_phase = 2, # Low value here to speed up the time
        vaeac.save_model = FALSE # Removes tmpdir and tmpfiles
      )
    ),
    "output_lm_categorical_vaeac"
  )
})

test_that("output_lm_categorical_categorical", {
  expect_snapshot_rds(
    explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "categorical",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_categorical_method"
  )
})

test_that("output_lm_categorical_independence", {
  expect_snapshot_rds(
    explain(
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "independence",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_categorical_independence"
  )
})

test_that("output_lm_ts_timeseries", {
  expect_snapshot_rds(
    explanation_timeseries <- explain(
      model = model_lm_ts,
      x_explain = x_explain_ts,
      x_train = x_train_ts,
      approach = "timeseries",
      prediction_zero = p0_ts,
      group = group_ts,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_timeseries_method"
  )
})

test_that("output_lm_numeric_comb1", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = c("gaussian", "empirical", "ctree", "independence"),
      prediction_zero = p0,
      n_batches = 4,
      timing = FALSE
    ),
    "output_lm_numeric_comb1"
  )
})

test_that("output_lm_numeric_comb2", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = c("ctree", "copula", "independence", "copula"),
      prediction_zero = p0,
      n_batches = 3,
      timing = FALSE
    ),
    "output_lm_numeric_comb2"
  )
})

test_that("output_lm_numeric_comb3", {
  expect_snapshot_rds(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = c("independence", "empirical", "gaussian", "empirical"),
      prediction_zero = p0,
      n_batches = 3,
      timing = FALSE
    ),
    "output_lm_numeric_comb3"
  )
})


# lm_mixed with different approaches

test_that("output_lm_mixed_independence", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "independence",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_mixed_independence"
  )
})

test_that("output_lm_mixed_ctree", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_mixed_ctree"
  )
})

test_that("output_lm_mixed_vaeac", {
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "vaeac",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      n_samples = 10, # Low value here to speed up the time
      vaeac.epochs = 4, # Low value here to speed up the time
      vaeac.n_vaeacs_initialize = 2, # Low value here to speed up the time
      vaeac.extra_parameters = list(
        vaeac.epochs_initiation_phase = 2, # Low value here to speed up the time
        vaeac.save_model = FALSE # Removes tmpdir and tmpfiles
      )
    ),
    "output_lm_mixed_vaeac"
  )
})

test_that("output_lm_mixed_comb", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = c("ctree", "independence", "ctree", "independence"),
      prediction_zero = p0,
      n_batches = 2,
      timing = FALSE
    ),
    "output_lm_mixed_comb"
  )
})



### Custom model by passing predict_model
test_that("output_custom_lm_numeric_independence_1", {
  set.seed(123)
  custom_pred_func <- function(x, newdata) {
    beta <- coef(x)
    X <- cbind(1, newdata)
    return(as.vector(beta %*% t(X)))
  }

  model_custom_lm_numeric <- model_lm_numeric

  expect_snapshot_rds(
    explain(
      model = model_custom_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      predict_model = custom_pred_func,
      n_batches = 1,
      timing = FALSE
    ),
    "output_custom_lm_numeric_independence_1"
  )
})

test_that("output_custom_lm_numeric_independence_2", {
  set.seed(123)
  custom_pred_func <- function(x, newdata) {
    beta <- coef(x)
    X <- cbind(1, newdata)
    return(as.vector(beta %*% t(X)))
  }

  model_custom_lm_numeric <- model_lm_numeric
  class(model_custom_lm_numeric) <- "whatever"


  expect_snapshot_rds(
    (custom <- explain(
      model = model_custom_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      predict_model = custom_pred_func,
      n_batches = 1,
      timing = FALSE
    )),
    "output_custom_lm_numeric_independence_2"
  )

  native <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    prediction_zero = p0,
    n_batches = 1,
    timing = FALSE
  )

  # Check that the printed Shapley values are identical
  expect_equal(
    custom$shapley_values,
    native$shapley_values
  )
})

test_that("output_custom_xgboost_mixed_dummy_ctree", {
  if (requireNamespace("xgboost", quietly = TRUE)) {
    x_train_mixed_dummy <- model.matrix(~ . + 0, x_train_mixed)
    x_explain_mixed_dummy <- model.matrix(~ . + 0, x_explain_mixed)

    y_train <- data_train[, get(y_var_numeric)]

    # Fitting a basic xgboost model to the training data
    model_xgboost_mixed_dummy <- xgboost::xgboost(
      data = x_train_mixed_dummy,
      label = y_train,
      nround = 20,
      verbose = FALSE
    )

    predict_model.xgboost_dummy <- function(x, newdata) {
      newdata_dummy <- model.matrix(~ . + 0, newdata)

      predict(x, newdata_dummy)
    }

    # Check that created predict_model works as intended
    expect_equal(
      predict_model.xgboost_dummy(model_xgboost_mixed_dummy, x_explain_mixed),
      predict(model_xgboost_mixed_dummy, x_explain_mixed_dummy)
    )

    # Specifying the phi_0, i.e. the expected prediction without any features
    p0 <- data_train[, mean(get(y_var_numeric))]


    expect_snapshot_rds(
      {
        custom <- explain(
          model = model_xgboost_mixed_dummy,
          x_train = x_train_mixed,
          x_explain = x_explain_mixed,
          approach = "ctree",
          prediction_zero = p0,
          predict_model = predict_model.xgboost_dummy,
          get_model_specs = NA,
          n_batches = 1,
          timing = FALSE
        )
        #      custom$internal$objects$predict_model <- "Del on purpose" # Avoids issues with xgboost package updates
        custom
      },
      "output_custom_xgboost_mixed_dummy_ctree"
    )
  }
})

test_that("output_lm_numeric_interaction", {
  x_train_interaction <- x_train_numeric[, mget(all.vars(formula(model_lm_interaction))[-1])]
  x_explain_interaction <- x_explain_numeric[, mget(all.vars(formula(model_lm_interaction))[-1])]
  expect_snapshot_rds(
    explain(
      model = model_lm_interaction,
      x_explain = x_explain_interaction,
      x_train = x_train_interaction,
      approach = "independence",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE
    ),
    "output_lm_numeric_interaction"
  )
})

test_that("output_lm_numeric_ctree_parallelized", {
  future::plan("multisession", workers = 2)
  expect_snapshot_rds(
    {
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "ctree",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    "output_lm_numeric_ctree_parallelized"
  )
  future::plan("sequential")
})

test_that("output_lm_numeric_independence_more_batches", {
  expect_snapshot_rds(
    {
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 10,
        timing = FALSE
      )
    },
    "output_lm_numeric_independence_n_batches_10"
  )
})

# Nothing special here, as the test does not record the actual progress output.
# It just checks whether calling on progressr does not produce an error or unexpected output.
test_that("output_lm_numeric_empirical_progress", {
  progressr::handlers("txtprogressbar")
  expect_snapshot_rds(
    {
      progressr::with_progress({
        explain(
          model = model_lm_numeric,
          x_explain = x_explain_numeric,
          x_train = x_train_numeric,
          approach = "empirical",
          prediction_zero = p0,
          n_batches = 10,
          timing = FALSE
        )
      })
    },
    "output_lm_numeric_empirical_progress"
  )
})


# Just checking that internal$output$dt_samp_for_vS  keep_samp_for_vS
test_that("output_lm_numeric_independence_keep_samp_for_vS", {
  expect_snapshot_rds(
    (out <- explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      n_batches = 1,
      timing = FALSE,
      keep_samp_for_vS = TRUE
    )),
    "output_lm_numeric_independence_keep_samp_for_vS"
  )

  expect_false(is.null(out$internal$output$dt_samp_for_vS))
})
