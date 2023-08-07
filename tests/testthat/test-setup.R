test_that("error with custom model without providing predict_model", {
  set.seed(123)


  expect_snapshot(
    {
      # Custom model with no predict_model

      model_custom_lm_mixed <- model_lm_mixed
      class(model_custom_lm_mixed) <- "whatever"

      explain(
        model = model_custom_lm_mixed,
        x_train = x_train_mixed,
        x_explain = x_explain_mixed,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("messages with missing detail in get_model_specs", {
  set.seed(123)

  model_custom_lm_mixed <- model_lm_mixed
  class(model_custom_lm_mixed) <- "whatever"

  custom_predict_model <- function(x, newdata) {
    beta <- coef(x)
    X <- model.matrix(~., newdata)
    return(as.vector(beta %*% t(X)))
  }

  expect_snapshot({
    # Custom model with no get_model_specs
    explain(
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      prediction_zero = p0,
      predict_model = custom_predict_model,
      get_model_specs = NA,
      n_batches = 1,
      timing = FALSE
    )
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-labels
    custom_get_model_specs_no_lab <- function(x) {
      feature_specs <- list(labels = NA, classes = NA, factor_levels = NA)
    }

    explain(
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      prediction_zero = p0,
      predict_model = custom_predict_model,
      get_model_specs = custom_get_model_specs_no_lab,
      n_batches = 1,
      timing = FALSE
    )
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-classes
    custom_gms_no_classes <- function(x) {
      feature_specs <- list(labels = labels(x$terms), classes = NA, factor_levels = NA)
    }

    explain(
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      prediction_zero = p0,
      predict_model = custom_predict_model,
      get_model_specs = custom_gms_no_classes,
      n_batches = 1,
      timing = FALSE
    )
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-factor levels
    custom_gms_no_factor_levels <- function(x) {
      feature_specs <- list(
        labels = labels(x$terms),
        classes = attr(x$terms, "dataClasses")[-1],
        factor_levels = NA
      )
    }

    explain(
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      prediction_zero = p0,
      predict_model = custom_predict_model,
      get_model_specs = custom_gms_no_factor_levels,
      n_batches = 1,
      timing = FALSE
    )
  })
})

test_that("erroneous input: `x_train/x_explain`", {
  set.seed(123)

  expect_snapshot(
    {
      # not matrix or data.table 1
      x_train_wrong_format <- c(a = 1, b = 2)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_wrong_format,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # not matrix or data.table 2
      x_explain_wrong_format <- c(a = 1, b = 2)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_wrong_format,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # not matrix or data.table 3
      x_train_wrong_format <- c(a = 1, b = 2)
      x_explain_wrong_format <- c(a = 3, b = 4)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_wrong_format,
        x_train = x_train_wrong_format,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # missing column names x_train
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      names(x_train_no_column_names) <- NULL

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_no_column_names,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # missing column names x_explain
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_no_column_names,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # missing column names in both x_train and x_explain
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_no_column_names,
        x_train = x_train_no_column_names,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `model`", {
  set.seed(123)

  expect_snapshot(
    {
      # no model passed
      explain(
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `approach`", {
  set.seed(123)

  expect_snapshot(
    {
      # not a character (vector)
      approach_non_character <- 1

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = approach_non_character,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # incorrect length
      approach_incorrect_length <- c("empirical", "gaussian")

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = approach_incorrect_length,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # incorrect character
      approach_incorrect_character <- "bla"

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = approach_incorrect_character,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `prediction_zero`", {
  set.seed(123)

  expect_snapshot(
    {
      # non-numeric 1
      p0_non_numeric_1 <- "bla"

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0_non_numeric_1,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-numeric 2
      p0_non_numeric_2 <- NULL

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0_non_numeric_2,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # length > 1
      p0_too_long <- c(1, 2)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0_too_long,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # NA-numeric
      p0_is_NA <- as.numeric(NA)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0_is_NA,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `n_combinations`", {
  set.seed(123)

  expect_snapshot(
    {
      # non-numeric 1
      n_combinations_non_numeric_1 <- "bla"

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_combinations = n_combinations_non_numeric_1,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-numeric 2
      n_combinations_non_numeric_2 <- TRUE

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_combinations = n_combinations_non_numeric_2,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # non-integer
      n_combinations_non_integer <- 10.5

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_combinations = n_combinations_non_integer,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )



  expect_snapshot(
    {
      # length > 1
      n_combinations_too_long <- c(1, 2)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_combinations = n_combinations_too_long,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # NA-numeric
      n_combinations_is_NA <- as.numeric(NA)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_combinations = n_combinations_is_NA,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Non-positive
      n_combinations_non_positive <- 0

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_combinations = n_combinations_non_positive,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Too low n_combinations (smaller than # features
      n_combinations <- ncol(x_explain_numeric) - 1

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        approach = "gaussian",
        n_combinations = n_combinations,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # Too low n_combinations (smaller than # groups
      groups <- list(
        A = c("Solar.R", "Wind"),
        B = c("Temp", "Month"),
        C = "Day"
      )

      n_combinations <- length(groups) - 1

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        prediction_zero = p0,
        approach = "gaussian",
        group = groups,
        n_combinations = n_combinations,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `group`", {
  set.seed(123)

  expect_snapshot(
    {
      # not a list
      group_non_list <- "bla"

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        group = group_non_list,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-characters in list
      group_with_non_characters <- list(A = 1, B = 2)

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        group = group_with_non_characters,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # group features not in data
      group_with_non_data_features <- list(
        A = c("Solar.R", "Wind", "not_a_data_feature"),
        B = c("Temp", "Month", "Day")
      )
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        group = group_with_non_data_features,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # missing feature in group
      group_missing_data_features <- list(
        A = c("Solar.R"),
        B = c("Temp", "Month", "Day")
      )
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        group = group_missing_data_features,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # missing feature in group
      group_dup_data_features <- list(
        A = c("Solar.R", "Solar.R", "Wind"),
        B = c("Temp", "Month", "Day")
      )
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        group = group_dup_data_features,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # a single group only
      single_group <- list(A = c("Solar.R", "Wind", "Temp", "Month", "Day"))
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        group = single_group,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `n_samples`", {
  set.seed(123)

  expect_snapshot(
    {
      # non-numeric 1
      n_samples_non_numeric_1 <- "bla"

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_samples = n_samples_non_numeric_1,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-numeric 2
      n_samples_non_numeric_2 <- TRUE

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_samples = n_samples_non_numeric_2,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-integer
      n_samples_non_integer <- 10.5
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_samples = n_samples_non_integer,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # length > 1
  expect_snapshot(
    {
      n_samples_too_long <- c(1, 2)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_samples = n_samples_too_long,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # NA-numeric
  expect_snapshot(
    {
      n_samples_is_NA <- as.numeric(NA)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_samples = n_samples_is_NA,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # Non-positive
  expect_snapshot(
    {
      n_samples_non_positive <- 0
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_samples = n_samples_non_positive,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `n_batches`", {
  set.seed(123)

  # non-numeric 1
  expect_snapshot(
    {
      n_batches_non_numeric_1 <- "bla"
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = n_batches_non_numeric_1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # non-numeric 2
  expect_snapshot(
    {
      n_batches_non_numeric_2 <- TRUE
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = n_batches_non_numeric_2,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # non-integer
  expect_snapshot(
    {
      n_batches_non_integer <- 10.5
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = n_batches_non_integer,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # length > 1
  expect_snapshot(
    {
      n_batches_too_long <- c(1, 2)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = n_batches_too_long,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # NA-numeric
  expect_snapshot(
    {
      n_batches_is_NA <- as.numeric(NA)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = n_batches_is_NA,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # Non-positive
  expect_snapshot(
    {
      n_batches_non_positive <- 0
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = n_batches_non_positive,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # Larger than number of n_combinations
  expect_snapshot(
    {
      n_combinations <- 10
      n_batches_too_large <- 11
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_combinations = n_combinations,
        n_batches = n_batches_too_large,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # Larger than number of n_combinations without specification
  expect_snapshot(
    {
      n_batches_too_large_2 <- 32
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        n_batches = n_batches_too_large_2,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `seed`", {
  set.seed(123)

  # Not interpretable as integer
  expect_snapshot(
    {
      seed_not_integer_interpretable <- "bla"
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        seed = seed_not_integer_interpretable,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `keep_samp_for_vS`", {
  set.seed(123)

  # non-logical 1
  expect_snapshot(
    {
      keep_samp_for_vS_non_logical_1 <- "bla"
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        keep_samp_for_vS = keep_samp_for_vS_non_logical_1,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  # non-logical 2
  expect_snapshot(
    {
      keep_samp_for_vS_non_logical_2 <- NULL
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        keep_samp_for_vS = keep_samp_for_vS_non_logical_2,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )


  # length > 1
  expect_snapshot(
    {
      keep_samp_for_vS_too_long <- c(TRUE, FALSE)
      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        keep_samp_for_vS = keep_samp_for_vS_too_long,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `predict_model`", {
  set.seed(123)

  # not a function
  expect_snapshot(
    {
      predict_model_nonfunction <- "bla"

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        predict_model = predict_model_nonfunction,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-numeric output
      predict_model_non_num_output <- function(model, x) {
        "bla"
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        predict_model = predict_model_non_num_output,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # incorrect output length
      predict_model_wrong_output_len <- function(model, x) {
        rep(1, nrow(x) + 1)
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        predict_model = predict_model_wrong_output_len,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # invalid function format
      predict_model_invalid_argument <- function(model) {
        rep(1, nrow(x))
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        predict_model = predict_model_invalid_argument,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # error within function
      predict_model_error <- function(model, x) {
        1 + "bla"
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        predict_model = predict_model_error,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `get_model_specs`", {
  set.seed(123)

  expect_snapshot(
    {
      # not a function
      get_model_specs_nonfunction <- "bla"

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        get_model_specs = get_model_specs_nonfunction,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # wrong output (not list)
      get_ms_output_not_list <- function(x) {
        "bla"
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        get_model_specs = get_ms_output_not_list,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # wrong output (wrong length)
      get_ms_output_too_long <- function(x) {
        list(1, 2, 3, 4)
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        get_model_specs = get_ms_output_too_long,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # wrong output (wrong length)
      get_ms_output_wrong_names <- function(x) {
        list(
          labels = 1,
          classes = 2,
          not_a_name = 3
        )
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        get_model_specs = get_ms_output_wrong_names,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # wrong output (wrong length)
      get_model_specs_error <- function(x) {
        1 + "bla"
      }

      explain(
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        prediction_zero = p0,
        get_model_specs = get_model_specs_error,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("incompatible input: `data/approach`", {
  set.seed(123)

  expect_snapshot(
    {
      # factor model/data with approach gaussian
      non_factor_approach_1 <- "gaussian"
      explain(
        model = model_lm_mixed,
        x_explain = x_explain_mixed,
        x_train = x_explain_mixed,
        approach = non_factor_approach_1,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # factor model/data with approach empirical
      non_factor_approach_2 <- "empirical"
      explain(
        model = model_lm_mixed,
        x_explain = x_explain_mixed,
        x_train = x_explain_mixed,
        approach = non_factor_approach_2,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # factor model/data with approach copula
      non_factor_approach_3 <- "copula"
      explain(
        model = model_lm_mixed,
        x_explain = x_explain_mixed,
        x_train = x_explain_mixed,
        approach = non_factor_approach_3,
        prediction_zero = p0,
        n_batches = 1,
        timing = FALSE
      )
    },
    error = TRUE
  )
})

test_that("Correct dimension of S when sampling combinations", {
  n_combinations <- 10

  res <- explain(
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_explain_mixed,
    prediction_zero = p0,
    approach = "ctree",
    n_combinations = n_combinations,
    n_batches = 1,
    timing = FALSE
  )

  expect_equal(nrow(res$internal$objects$S), n_combinations)
})

test_that("Error with too low `n_combinations`", {
  n_combinations <- ncol(x_explain_numeric) - 1

  expect_error(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_explain_numeric,
      prediction_zero = p0,
      approach = "gaussian",
      n_combinations = n_combinations,
      n_batches = 1,
      timing = FALSE
    )
  )

  # Same for groups
  groups <- list(
    A = c("Solar.R", "Wind"),
    B = c("Temp", "Month"),
    C = "Day"
  )

  n_combinations <- length(groups) - 1

  expect_error(
    explain(
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_explain_numeric,
      prediction_zero = p0,
      approach = "gaussian",
      group = groups,
      n_combinations = n_combinations,
      n_batches = 1,
      timing = FALSE
    )
  )
})

test_that("Correct dimension of S when sampling combinations with groups", {
  n_combinations <- 5

  groups <- list(
    A = c("Solar.R", "Wind"),
    B = c("Temp", "Month_factor"),
    C = "Day"
  )

  res <- explain(
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_explain_mixed,
    prediction_zero = p0,
    approach = "ctree",
    group = groups,
    n_combinations = n_combinations,
    n_batches = 1,
    timing = FALSE
  )

  expect_equal(nrow(res$internal$objects$S), n_combinations)
})

test_that("data feature ordering is output_lm_numeric_column_order", {
  explain.original <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    prediction_zero = p0,
    n_batches = 1,
    timing = FALSE
  )

  explain.new_data_feature_order <- explain(
    model = model_lm_numeric,
    x_explain = rev(x_explain_numeric),
    x_train = rev(x_train_numeric),
    approach = "empirical",
    prediction_zero = p0,
    n_batches = 1,
    timing = FALSE
  )

  explain.new_model_feat_order <- explain(
    model = model_lm_numeric_col_order,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    prediction_zero = p0,
    n_batches = 1,
    timing = FALSE
  )

  # Same Shapley values, but different order
  expect_false(identical(
    explain.original$shapley_values,
    explain.new_data_feature_order$shapley_values
  ))
  expect_equal(
    explain.original$shapley_values[, mget(sort(names(explain.original$shapley_values)))],
    explain.new_data_feature_order$shapley_values[, mget(sort(names(explain.new_data_feature_order$shapley_values)))]
  )

  # Same Shapley values in same order
  expect_equal(explain.original, explain.new_model_feat_order)
})

test_that("parallelization gives same output for any approach", {
  # Empirical is seed independent
  explain.empirical_sequential <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    prediction_zero = p0,
    n_batches = 10,
    timing = FALSE
  )

  future::plan("multisession", workers = 2) # Parallelized with 2 cores
  explain.empirical_multisession <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    prediction_zero = p0,
    n_batches = 10,
    timing = FALSE
  )

  future::plan("sequential") # Resetting to sequential computation

  # Identical results
  expect_equal(
    explain.empirical_sequential,
    explain.empirical_multisession
  )


  # ctree is seed NOT independent
  explain.ctree_sequential <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    prediction_zero = p0,
    n_batches = 10,
    timing = FALSE
  )

  future::plan("multisession", workers = 5) # Parallelized with 2 cores
  explain.ctree_multisession <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    prediction_zero = p0,
    n_batches = 10,
    timing = FALSE
  )

  future::plan("sequential") # Resetting to sequential computation

  # Identical results also for seed dependent methods.
  expect_equal(
    explain.ctree_sequential,
    explain.ctree_multisession
  )
})

test_that("different n_batches gives same/different shapley values for different approaches", {
  # approach "empirical" is seed independent
  explain.empirical_n_batches_5 <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    prediction_zero = p0,
    n_batches = 5,
    timing = FALSE
  )

  explain.empirical_n_batches_10 <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    prediction_zero = p0,
    n_batches = 10,
    timing = FALSE
  )


  # Difference in the objects (n_batches and related)
  expect_false(identical(
    explain.empirical_n_batches_5,
    explain.empirical_n_batches_10
  ))
  # Same Shapley values
  expect_equal(
    explain.empirical_n_batches_5$shapley_values,
    explain.empirical_n_batches_10$shapley_values
  )


  # approach "ctree" is seed dependent
  explain.ctree_n_batches_5 <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    prediction_zero = p0,
    n_batches = 5,
    timing = FALSE
  )

  explain.ctree_n_batches_10 <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    prediction_zero = p0,
    n_batches = 10,
    timing = FALSE
  )

  # Difference in the objects (n_batches and related)
  expect_false(identical(
    explain.ctree_n_batches_5,
    explain.ctree_n_batches_10
  ))
  # NEITHER same Shapley values
  expect_false(identical(
    explain.ctree_n_batches_5$shapley_values,
    explain.ctree_n_batches_10$shapley_values
  ))
})

test_that("gaussian approach use the user provided parameters", {
  # approach "gaussian" with default parameter estimation, i.e., sample mean and covariance
  explain.gaussian_sample_mean_and_covariance <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    prediction_zero = p0,
    timing = FALSE
  )

  # Expect that gaussian.mu is the sample mean when no values are provided
  expect_identical(
    explain.gaussian_sample_mean_and_covariance$internal$parameters$gaussian.mu,
    colMeans(unname(x_train_numeric))
  )

  # Expect that gaussian.mu is the sample covariance when no values are provided
  expect_identical(
    explain.gaussian_sample_mean_and_covariance$internal$parameters$gaussian.cov_mat,
    cov(x_train_numeric)
  )

  # Provide parameter values for the Gaussian approach
  gaussian.provided_mu = seq(ncol(x_train_numeric)) # 1,2,3,4,5
  gaussian.provided_cov_mat = diag(ncol(x_train_numeric))

  # approach "gaussian" with provided parameters
  explain.gaussian_provided_mean_and_covariance <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    prediction_zero = p0,
    timing = FALSE,
    gaussian.mu = gaussian.provided_mu,
    gaussian.cov_mat = gaussian.provided_cov_mat
  )

  # Expect that the gaussian.mu parameter is the same as the provided gaussian.provided_mu
  expect_identical(
    explain.gaussian_provided_mean_and_covariance$internal$parameters$gaussian.mu,
    gaussian.provided_mu
  )

  # Expect that gaussian.cov_mat is the same as the provided gaussian.provided_cov_mat
  expect_identical(
    explain.gaussian_provided_mean_and_covariance$internal$parameters$gaussian.cov_mat,
    gaussian.provided_cov_mat
  )
})
