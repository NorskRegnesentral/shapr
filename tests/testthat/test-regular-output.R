# lm_numeric with different approaches

test_that("output_lm_numeric_independence", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_independence"
  )
})

test_that("output_lm_numeric_independence_MSEv_Shapley_weights", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      output_args = list(MSEv_uniform_comb_weights = FALSE),
      iterative = FALSE
    ),
    "output_lm_numeric_independence_MSEv_Shapley_weights"
  )
})

test_that("output_lm_numeric_empirical", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_empirical"
  )
})

test_that("output_lm_numeric_empirical_n_coalitions", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      phi0 = p0,
      max_n_coalitions = 20,
      iterative = FALSE
    ),
    "output_lm_numeric_empirical_n_coalitions"
  )
})

test_that("output_lm_numeric_empirical_independence", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      phi0 = p0,
      empirical.type = "independence",
      iterative = FALSE
    ),
    "output_lm_numeric_empirical_independence"
  )
})

test_that("output_lm_numeric_empirical_AICc_each", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      phi0 = p0,
      max_n_coalitions = 8,
      empirical.type = "AICc_each_k",
      iterative = FALSE
    ),
    "output_lm_numeric_empirical_AICc_each"
  )
})

test_that("output_lm_numeric_empirical_AICc_full", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      phi0 = p0,
      max_n_coalitions = 8,
      empirical.type = "AICc_full",
      iterative = FALSE
    ),
    "output_lm_numeric_empirical_AICc_full"
  )
})

test_that("output_lm_numeric_gaussian", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_gaussian"
  )
})

test_that("output_lm_numeric_copula", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "copula",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_copula"
  )
})

test_that("output_lm_numeric_ctree", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "ctree",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_ctree"
  )
})

test_that("output_lm_numeric_vaeac", {
  skip_on_os("mac") # The code runs on macOS, but it gives different Shapley values due to inconsistencies in torch seed
  skip_if_not(torch::torch_is_installed())
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "vaeac",
      phi0 = p0,
      n_MC_samples = 10, # Low value here to speed up the time
      vaeac.epochs = 4, # Low value here to speed up the time
      vaeac.n_vaeacs_initialize = 2, # Low value here to speed up the time
      vaeac.extra_parameters = list(
        vaeac.epochs_initiation_phase = 2, # Low value here to speed up the time
        vaeac.save_model = FALSE # Removes names and objects such as tmpdir and tmpfile
      ),
      iterative = FALSE
    ),
    "output_lm_numeric_vaeac"
  )
})

test_that("output_lm_categorical_ctree", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "ctree",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_categorical_ctree"
  )
})

test_that("output_lm_categorical_vaeac", {
  skip_on_os("mac") # The code runs on macOS, but it gives different Shapley values due to inconsistencies in torch seed
  skip_if_not(torch::torch_is_installed())
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "vaeac",
      phi0 = p0,
      n_MC_samples = 10, # Low value here to speed up the time
      vaeac.epochs = 4, # Low value here to speed up the time
      vaeac.n_vaeacs_initialize = 2, # Low value here to speed up the time
      vaeac.extra_parameters = list(
        vaeac.epochs_initiation_phase = 2, # Low value here to speed up the time
        vaeac.save_model = FALSE # Removes tmpdir and tmpfiles
      ),
      iterative = FALSE
    ),
    "output_lm_categorical_vaeac"
  )
})

test_that("output_lm_categorical_categorical", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "categorical",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_categorical_method"
  )
})

test_that("output_lm_categorical_independence", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "independence",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_categorical_independence"
  )
})

test_that("output_lm_ts_timeseries", {
  expect_snapshot_rds(
    explanation_timeseries <- explain(
      testing = TRUE,
      model = model_lm_ts,
      x_explain = x_explain_ts,
      x_train = x_train_ts,
      approach = "timeseries",
      phi0 = p0_ts,
      group = group_ts,
      iterative = FALSE
    ),
    "output_lm_timeseries_method"
  )
})

test_that("output_lm_numeric_comb1", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = c("gaussian", "empirical", "ctree", "independence"),
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_comb1"
  )
})

test_that("output_lm_numeric_comb2", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = c("ctree", "copula", "independence", "copula"),
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_comb2"
  )
})

test_that("output_lm_numeric_comb3", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = c("independence", "empirical", "gaussian", "empirical"),
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_comb3"
  )
})


# lm_mixed with different approaches

test_that("output_lm_mixed_independence", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "independence",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_mixed_independence"
  )
})

test_that("output_lm_mixed_ctree", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_mixed_ctree"
  )
})

test_that("output_lm_mixed_vaeac", {
  skip_on_os("mac") # The code runs on macOS, but it gives different Shapley values due to inconsistencies in torch seed
  skip_if_not(torch::torch_is_installed())
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "vaeac",
      phi0 = p0,
      n_MC_samples = 10, # Low value here to speed up the time
      vaeac.epochs = 4, # Low value here to speed up the time
      vaeac.n_vaeacs_initialize = 2, # Low value here to speed up the time
      vaeac.extra_parameters = list(
        vaeac.epochs_initiation_phase = 2, # Low value here to speed up the time
        vaeac.save_model = FALSE # Removes tmpdir and tmpfiles
      ),
      iterative = FALSE
    ),
    "output_lm_mixed_vaeac"
  )
})

test_that("output_lm_mixed_comb", {
  set.seed(123)
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = c("ctree", "independence", "ctree", "independence"),
      phi0 = p0,
      iterative = FALSE
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
      testing = TRUE,
      model = model_custom_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      predict_model = custom_pred_func,
      iterative = FALSE
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
      testing = TRUE,
      model = model_custom_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      predict_model = custom_pred_func,
      iterative = FALSE
    )),
    "output_custom_lm_numeric_independence_2"
  )

  native <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "independence",
    phi0 = p0,
    iterative = FALSE
  )

  # Check that the printed Shapley values are identical
  expect_equal(
    custom$shapley_values_est,
    native$shapley_values_est
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
          testing = TRUE,
          model = model_xgboost_mixed_dummy,
          x_train = x_train_mixed,
          x_explain = x_explain_mixed,
          approach = "ctree",
          phi0 = p0,
          predict_model = predict_model.xgboost_dummy,
          get_model_specs = NA,
          iterative = FALSE
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
      testing = TRUE,
      model = model_lm_interaction,
      x_explain = x_explain_interaction,
      x_train = x_train_interaction,
      approach = "independence",
      phi0 = p0,
      iterative = FALSE
    ),
    "output_lm_numeric_interaction"
  )
})

test_that("output_lm_numeric_ctree_parallelized", {
  testthat::skip_on_cran() # Avoiding CRAN Note: Running R code in ‘testthat.R’ had CPU time 3.6 times elapsed time
  future::plan("multisession", workers = 2)
  expect_snapshot_rds(
    {
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "ctree",
        phi0 = p0,
        iterative = FALSE
      )
    },
    "output_lm_numeric_ctree_parallelized"
  )
  future::plan("sequential")
})

# Nothing special here, as the test does not record the actual progress output.
# It just checks whether calling on progressr does not produce an error or unexpected output.
test_that("output_lm_numeric_empirical_progress", {
  progressr::handlers("txtprogressbar")
  expect_snapshot_rds(
    {
      progressr::with_progress({
        explain(
          testing = TRUE,
          model = model_lm_numeric,
          x_explain = x_explain_numeric,
          x_train = x_train_numeric,
          approach = "empirical",
          phi0 = p0,
          iterative = FALSE
        )
      })
    },
    "output_lm_numeric_empirical_progress"
  )
})


# Just checking that internal$output$dt_samp_for_vS  works
test_that("output_lm_numeric_independence_keep_samp_for_vS", {
  expect_snapshot_rds(
    (out <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      phi0 = p0,
      output_args = list(keep_samp_for_vS = TRUE),
      iterative = FALSE
    )),
    "output_lm_numeric_independence_keep_samp_for_vS"
  )

  expect_false(is.null(out$internal$output$dt_samp_for_vS))
})
