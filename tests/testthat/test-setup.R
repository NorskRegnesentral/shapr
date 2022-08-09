

test_that("error with custom model without providing predict_model",{
  set.seed(123)

  model_custom_lm_mixed <- model_lm_mixed
  class(model_custom_lm_mixed) <- "whatever"

  # Custom model with no predict_model
  expect_snapshot(
    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0),
    error = T
  )

})

test_that("messages with missing detail in get_model_specs", {
  set.seed(123)

  model_custom_lm_mixed <- model_lm_mixed
  class(model_custom_lm_mixed) <- "whatever"

  custom_predict_model <- function(x, newdata) {
    beta <- coef(x)
    X <- model.matrix(~.,newdata)
    return(as.vector(beta %*% t(X)))
  }

  # Custom model with no get_model_specs
  expect_snapshot(
    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = NA)
  )

  # Custom model where get_model_specs gives NA-labels
  custom_get_model_specs_no_labels <- function(x){
    feature_specs <- list(labels = NA, classes = NA, factor_levels = NA)
  }

  expect_snapshot(
    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_labels)
  )

  # Custom model where get_model_specs gives NA-classes
  custom_get_model_specs_no_classes <- function(x){
    feature_specs <- list(labels = labels(x$terms), classes = NA, factor_levels = NA)
  }

  expect_snapshot(
    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_classes)
  )

  # Custom model where get_model_specs gives NA-factor levels
  custom_get_model_specs_no_factor_levels <- function(x){
    feature_specs <- list(labels = labels(x$terms),
                          classes = attr(x$terms, "dataClasses")[-1],
                          factor_levels = NA)
    }

  expect_snapshot(
    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_factor_levels)
  )


})

test_that("erroneous input: `prediction_zero`", {
  set.seed(123)

  # non-numeric 1
  p0_non_numeric_1 <- "bla"
  expect_snapshot(
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_non_numeric_1),
    error = T
  )

  # non-numeric 2
  p0_non_numeric_2 <- NULL
  expect_snapshot(
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_non_numeric_2),
    error = T
  )


  # length > 1
  p0_too_long <- c(1,2)
  expect_snapshot(
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_too_long),
    error = T
  )

  # NA-numeric
  p0_is_NA <- as.numeric(NA)
  expect_snapshot(
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_is_NA),
    error = T
  )

})

test_that("erroneous input: `n_combinations`", {
  set.seed(123)

  # non-numeric 1
  expect_snapshot({
    n_combinations_non_numeric_1 <- "bla"
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_non_numeric_1)
    },
    error = T)

  # non-numeric 2
  expect_snapshot({
    n_combinations_non_numeric_2 <- TRUE
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_non_numeric_2)
  },
  error = T)


  # non-integer
  expect_snapshot({
    n_combinations_non_integer <- 10.5
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_non_integer)
  },
  error = T)



  # length > 1
  expect_snapshot({
    n_combinations_too_long <- c(1,2)
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_too_long)
  },
  error = T)

  # NA-numeric
  expect_snapshot({
    n_combinations_is_NA <- as.numeric(NA)
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_is_NA)
  },
  error = T)

  # Non-positive
  expect_snapshot({
    n_combinations_non_positive <- 0
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_non_positive)
  },
  error = T)


})

test_that("erroneous input: `n_samples`", {
  set.seed(123)

  # non-numeric 1
  expect_snapshot({
    n_samples_non_numeric_1 <- "bla"
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_non_numeric_1)
  },
  error = T)

  # non-numeric 2
  expect_snapshot({
    n_samples_non_numeric_2 <- TRUE
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_non_numeric_2)
  },
  error = T)

  # non-integer
  expect_snapshot({
    n_samples_non_integer <- 10.5
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_non_integer)
  },
  error = T)

  # length > 1
  expect_snapshot({
    n_samples_too_long <- c(1,2)
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_too_long)
  },
  error = T)

  # NA-numeric
  expect_snapshot({
    n_samples_is_NA <- as.numeric(NA)
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_is_NA)
  },
  error = T)

  # Non-positive
  expect_snapshot({
    n_samples_non_positive <- 0
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_non_positive)
  },
  error = T)


})

test_that("erroneous input: `n_batches`", {
  set.seed(123)

  # non-numeric 1
  expect_snapshot({
    n_batches_non_numeric_1 <- "bla"
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_batches = n_batches_non_numeric_1)
  },
  error = T)

  # non-numeric 2
  expect_snapshot({
    n_batches_non_numeric_2 <- TRUE
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_batches = n_batches_non_numeric_2)
  },
  error = T)

  # non-integer
  expect_snapshot({
    n_batches_non_integer <- 10.5
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_batches = n_batches_non_integer)
  },
  error = T)

  # length > 1
  expect_snapshot({
    n_batches_too_long <- c(1,2)
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_batches = n_batches_too_long)
  },
  error = T)

  # NA-numeric
  expect_snapshot({
    n_batches_is_NA <- as.numeric(NA)
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_batches = n_batches_is_NA)
  },
  error = T)

  # Non-positive
  expect_snapshot({
    n_batches_non_positive <- 0
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_batches = n_batches_non_positive)
  },
  error = T)

  # Larger than number of n_combinations
  expect_snapshot({
    n_combinations <- 10
    n_batches_too_large <- 11
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations,
            n_batches = n_batches_too_large)
  },
  error = T)



})

test_that("erroneous input: `seed`", {
  set.seed(123)

  # Not interpretable as integer
  expect_snapshot({
    seed_not_integer_interpretable <- "bla"
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            seed = seed_not_integer_interpretable)
  },
  error = T)
})


test_that("erroneous input: `keep_samp_for_vS`", {
  set.seed(123)

  # non-logical 1
  expect_snapshot({
    keep_samp_for_vS_non_logical_1 <- "bla"
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            keep_samp_for_vS = keep_samp_for_vS_non_logical_1)
  },
  error = T)

  # non-logical 2
  expect_snapshot({
    keep_samp_for_vS_non_logical_2 <- NULL
    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            keep_samp_for_vS = keep_samp_for_vS_non_logical_2)
  },
  error = T)


  # length > 1
  expect_snapshot({
    expect_snapshot({
      keep_samp_for_vS_too_long <- c(TRUE,FALSE)
      explain(x_train_numeric,
              x_explain_numeric,
              model_lm_numeric,
              approach = "independence",
              prediction_zero = p0,
              keep_samp_for_vS = keep_samp_for_vS_too_long)
    },
    error = T)
  })
})

