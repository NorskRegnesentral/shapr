

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

test_that("erroneous input: `x_train/x_explain`", {
  set.seed(123)

  # not matrix or data.table 1
  expect_snapshot({
    x_train_wrong_format <- c(a=1,b=2)
    explain(x_train = x_train_wrong_format,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  # not matrix or data.table 2
  expect_snapshot({
    x_explain_wrong_format <- c(a=1,b=2)
    explain(x_train = x_explain_numeric,
            x_explain = x_explain_wrong_format,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  # not matrix or data.table 3
  expect_snapshot({
    x_train_wrong_format <- c(a=1,b=2)
    x_explain_wrong_format <- c(a=3,b=4)
    explain(x_train = x_train_wrong_format,
            x_explain = x_explain_wrong_format,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)


  # missing column names x_train
  expect_snapshot({
    x_train_no_column_names <- as.data.frame(x_train_numeric)
    names(x_train_no_column_names) <- NULL
    explain(x_train = x_train_no_column_names,
            x_explain = x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  # missing column names x_explain
  expect_snapshot({
    x_explain_no_column_names <- as.data.frame(x_explain_numeric)
    names(x_explain_no_column_names) <- NULL
    explain(x_train = x_train_numeric,
            x_explain = x_explain_no_column_names,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  # missing column names in both x_train and x_explain
  expect_snapshot({
    x_train_no_column_names <- as.data.frame(x_train_numeric)
    x_explain_no_column_names <- as.data.frame(x_explain_numeric)
    names(x_explain_no_column_names) <- NULL
    explain(x_train = x_train_no_column_names,
            x_explain = x_explain_no_column_names,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  })

test_that("erroneous input: `predict_model`", {
  set.seed(123)

  # not a function
  expect_snapshot({
    predict_model_nonfunction <- "bla"
    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            predict_model = predict_model_nonfunction)
  },
  error = T)

})

test_that("erroneous input: `get_model_specs`", {
  set.seed(123)

  # not a function
  expect_snapshot({
    get_model_specs_nonfunction <- "bla"
    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            get_model_specs = get_model_specs_nonfunction)
  },
  error = T)

})

test_that("erroneous input: `group`", {
  set.seed(123)

  # not a list
  expect_snapshot({
    group_non_list <- "bla"
    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            group = group_non_list)
  },
  error = T)

  # non-characters in list
  expect_snapshot({
    group_with_non_characters <- list(A=1,B=2)
    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            group = group_with_non_characters)
  },
  error = T)

  # group features not in data
  expect_snapshot({
    group_with_non_data_features <- list(A=c("Solar.R","Wind","not_a_data_feature"),
                                    B=c("Temp", "Month", "Day"))
    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            group = group_with_non_data_features)
  },
  error = T)

  # missing feature in group
  expect_snapshot({
    group_with_missing_data_features <- list(A=c("Solar.R"),
                                         B=c("Temp", "Month", "Day"))
    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            group = group_with_missing_data_features)
  },
  error = T)

  # missing feature in group
  expect_snapshot({
    group_with_duplicated_data_features <- list(A=c("Solar.R","Solar.R","Wind"),
                                             B=c("Temp", "Month", "Day"))
    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            group = group_with_duplicated_data_features)
  },
  error = T)


})
