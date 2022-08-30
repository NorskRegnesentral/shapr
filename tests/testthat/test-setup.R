

test_that("error with custom model without providing predict_model",{
  set.seed(123)


  expect_snapshot({
    # Custom model with no predict_model

    model_custom_lm_mixed <- model_lm_mixed
    class(model_custom_lm_mixed) <- "whatever"

    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0)
    },
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

  expect_snapshot({
    # Custom model with no get_model_specs
    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = NA)
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-labels
    custom_get_model_specs_no_labels <- function(x){
      feature_specs <- list(labels = NA, classes = NA, factor_levels = NA)
    }

    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_labels)
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-classes
    custom_get_model_specs_no_classes <- function(x){
      feature_specs <- list(labels = labels(x$terms), classes = NA, factor_levels = NA)
    }

    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_classes)
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-factor levels
    custom_get_model_specs_no_factor_levels <- function(x){
      feature_specs <- list(labels = labels(x$terms),
                            classes = attr(x$terms, "dataClasses")[-1],
                            factor_levels = NA)
    }

    explain(x_train_mixed,
            x_explain_mixed,
            model_custom_lm_mixed,
            approach = "independence",
            prediction_zero = p0,
            predict_model = custom_predict_model,
            get_model_specs = custom_get_model_specs_no_factor_levels)
  })


})

test_that("erroneous input: `x_train/x_explain`", {
  set.seed(123)

  expect_snapshot({
    # not matrix or data.table 1
    x_train_wrong_format <- c(a=1,b=2)

    explain(x_train = x_train_wrong_format,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # not matrix or data.table 2
    x_explain_wrong_format <- c(a=1,b=2)

    explain(x_train = x_explain_numeric,
            x_explain = x_explain_wrong_format,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # not matrix or data.table 3
    x_train_wrong_format <- c(a=1,b=2)
    x_explain_wrong_format <- c(a=3,b=4)

    explain(x_train = x_train_wrong_format,
            x_explain = x_explain_wrong_format,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)


  expect_snapshot({
    # missing column names x_train
    x_train_no_column_names <- as.data.frame(x_train_numeric)
    names(x_train_no_column_names) <- NULL

    explain(x_train = x_train_no_column_names,
            x_explain = x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # missing column names x_explain
    x_explain_no_column_names <- as.data.frame(x_explain_numeric)
    names(x_explain_no_column_names) <- NULL

    explain(x_train = x_train_numeric,
            x_explain = x_explain_no_column_names,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # missing column names in both x_train and x_explain
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

test_that("erroneous input: `model`", {
  set.seed(123)

  expect_snapshot({
    # no model passed
    explain(x_train = x_train_numeric,
            x_explain = x_explain_numeric,
            approach = "independence",
            prediction_zero = p0)
  },
  error = T)



})

test_that("erroneous input: `approach`", {
  set.seed(123)

  expect_snapshot({
    # not a character (vector)
    approach_non_character <- 1

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = approach_non_character,
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # incorrect length
    approach_incorrect_length <- c("empirical","gaussian")

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = approach_incorrect_length,
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # incorrect character
    approach_incorrect_character <- "bla"

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = approach_incorrect_character,
            prediction_zero = p0)
  },
  error = T)


})

test_that("erroneous input: `prediction_zero`", {
  set.seed(123)

  expect_snapshot({
    # non-numeric 1
    p0_non_numeric_1 <- "bla"

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_non_numeric_1)
    },
    error = T)

  expect_snapshot({
    # non-numeric 2
    p0_non_numeric_2 <- NULL

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_non_numeric_2)
    },
    error = T)


  expect_snapshot({
    # length > 1
    p0_too_long <- c(1,2)

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_too_long)
    },
    error = T)

  expect_snapshot({
    # NA-numeric
    p0_is_NA <- as.numeric(NA)

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0_is_NA)
  },
    error = T)

})

test_that("erroneous input: `n_combinations`", {
  set.seed(123)

  expect_snapshot({
    # non-numeric 1
    n_combinations_non_numeric_1 <- "bla"

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_non_numeric_1)
    },
    error = T)

  expect_snapshot({
    # non-numeric 2
    n_combinations_non_numeric_2 <- TRUE

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_non_numeric_2)
  },
  error = T)


  expect_snapshot({
    # non-integer
    n_combinations_non_integer <- 10.5

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_non_integer)
  },
  error = T)



  expect_snapshot({
    # length > 1
    n_combinations_too_long <- c(1,2)

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_too_long)
  },
  error = T)

  expect_snapshot({
    # NA-numeric
    n_combinations_is_NA <- as.numeric(NA)

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_combinations = n_combinations_is_NA)
  },
  error = T)

  expect_snapshot({
    # Non-positive
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

test_that("erroneous input: `group`", {
  set.seed(123)

  expect_snapshot({
    # not a list
    group_non_list <- "bla"

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            group = group_non_list)
  },
  error = T)

  expect_snapshot({
    # non-characters in list
    group_with_non_characters <- list(A=1,B=2)

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            group = group_with_non_characters)
  },
  error = T)

  expect_snapshot({
    # group features not in data
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

  expect_snapshot({
    # missing feature in group
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

  expect_snapshot({
    # missing feature in group
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

test_that("erroneous input: `n_samples`", {
  set.seed(123)

  expect_snapshot({
    # non-numeric 1
    n_samples_non_numeric_1 <- "bla"

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_non_numeric_1)
  },
  error = T)

  expect_snapshot({
    # non-numeric 2
    n_samples_non_numeric_2 <- TRUE

    explain(x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            n_samples = n_samples_non_numeric_2)
  },
  error = T)

  expect_snapshot({
    # non-integer
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

  expect_snapshot({
    # non-numeric output
    predict_model_non_numeric_output <- function(model,x){"bla"}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            predict_model = predict_model_non_numeric_output)
  },
  error = T)

  expect_snapshot({
    # incorrect output length
    predict_model_incorrect_output_length <- function(model,x){rep(1,nrow(x)+1)}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            predict_model = predict_model_incorrect_output_length)
  },
  error = T)

  expect_snapshot({
    # invalid function format
    predict_model_invalid_argument <- function(model){rep(1,nrow(x))}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            predict_model = predict_model_invalid_argument)
  },
  error = T)

  expect_snapshot({
    # error within function
    predict_model_error <- function(model,x){1+"bla"}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            predict_model = predict_model_error)
  },
  error = T)


})

test_that("erroneous input: `get_model_specs`", {
  set.seed(123)

  expect_snapshot({
    # not a function
    get_model_specs_nonfunction <- "bla"

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            get_model_specs = get_model_specs_nonfunction)
  },
  error = T)


  expect_snapshot({
    # wrong output (not list)
    get_model_specs_output_not_list <- function(x){"bla"}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            get_model_specs = get_model_specs_output_not_list)
  },
  error = T)

  expect_snapshot({
    # wrong output (wrong length)
    get_model_specs_output_too_long <- function(x){list(1,2,3,4)}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            get_model_specs = get_model_specs_output_too_long)
  },
  error = T)

  expect_snapshot({
    # wrong output (wrong length)
    get_model_specs_output_wrong_names <- function(x){list(labels=1,
                                                           classes=2,
                                                           not_a_name=3)}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            get_model_specs = get_model_specs_output_wrong_names)
  },
  error = T)

  expect_snapshot({
    # wrong output (wrong length)
    get_model_specs_error <- function(x){1+"bla"}

    explain(x_train = x_train_numeric,
            x_explain_numeric,
            model_lm_numeric,
            approach = "independence",
            prediction_zero = p0,
            get_model_specs = get_model_specs_error)
  },
  error = T)


})

test_that("incompatible input: `data/approach`", {
  set.seed(123)

  expect_snapshot({
    # factor model/data with approach gaussian
    non_factor_approach_1 <- "gaussian"
    explain(x_train = x_explain_mixed,
            x_explain_mixed,
            model_lm_mixed,
            approach = non_factor_approach_1,
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # factor model/data with approach empirical
    non_factor_approach_2 <- "empirical"
    explain(x_train = x_explain_mixed,
            x_explain_mixed,
            model_lm_mixed,
            approach = non_factor_approach_2,
            prediction_zero = p0)
  },
  error = T)

  expect_snapshot({
    # factor model/data with approach copula
    non_factor_approach_3 <- "copula"
    explain(x_train = x_explain_mixed,
            x_explain_mixed,
            model_lm_mixed,
            approach = non_factor_approach_3,
            prediction_zero = p0)
  },
  error = T)


})

test_that("Correct dimension of S when sampling combinations", {

  n_combinations = 10

  res = explain(x_train = x_explain_mixed,
                x_explain_mixed,
                model_lm_mixed,
                prediction_zero = p0,
                approach = "ctree",
                n_combinations = n_combinations)

  expect_equal(nrow(res$internal$objects$S), n_combinations)

})


