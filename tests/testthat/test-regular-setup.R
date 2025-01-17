test_that("error with custom model without providing predict_model", {
  set.seed(123)


  expect_snapshot(
    {
      # Custom model with no predict_model

      model_custom_lm_mixed <- model_lm_mixed
      class(model_custom_lm_mixed) <- "whatever"

      explain(
        testing = TRUE,
        model = model_custom_lm_mixed,
        x_train = x_train_mixed,
        x_explain = x_explain_mixed,
        approach = "independence",
        phi0 = p0
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
      testing = TRUE,
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      phi0 = p0,
      predict_model = custom_predict_model,
      get_model_specs = NA
    )
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-labels
    custom_get_model_specs_no_lab <- function(x) {
      feature_specs <- list(labels = NA, classes = NA, factor_levels = NA)
    }

    explain(
      testing = TRUE,
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      phi0 = p0,
      predict_model = custom_predict_model,
      get_model_specs = custom_get_model_specs_no_lab
    )
  })


  expect_snapshot({
    # Custom model where get_model_specs gives NA-classes
    custom_gms_no_classes <- function(x) {
      feature_specs <- list(labels = labels(x$terms), classes = NA, factor_levels = NA)
    }

    explain(
      testing = TRUE,
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      phi0 = p0,
      predict_model = custom_predict_model,
      get_model_specs = custom_gms_no_classes
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
      testing = TRUE,
      model = model_custom_lm_mixed,
      x_train = x_train_mixed,
      x_explain = x_explain_mixed,
      approach = "independence",
      phi0 = p0,
      predict_model = custom_predict_model,
      get_model_specs = custom_gms_no_factor_levels
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_wrong_format,
        approach = "independence",
        phi0 = p0
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # not matrix or data.table 2
      x_explain_wrong_format <- c(a = 1, b = 2)

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_wrong_format,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_wrong_format,
        x_train = x_train_wrong_format,
        approach = "independence",
        phi0 = p0
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_no_column_names,
        approach = "independence",
        phi0 = p0
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_no_column_names,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_no_column_names,
        x_train = x_train_no_column_names,
        approach = "independence",
        phi0 = p0
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `model`", {
  # R versions earlier than 4.3 gives assigns the error to the internal function instead of the explain_forecast,
  # and therefore marks this as an error (which it is not)
  Rversion_number <- as.numeric(paste0(R.version$major, R.version$minor))
  skip_if_not(Rversion_number >= 43)
  set.seed(123)

  expect_snapshot(
    {
      # no model passed
      explain(
        testing = TRUE,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = approach_non_character,
        phi0 = p0
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # incorrect length
      approach_incorrect_length <- c("empirical", "gaussian")

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = approach_incorrect_length,
        phi0 = p0
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # incorrect character
      approach_incorrect_character <- "bla"

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = approach_incorrect_character,
        phi0 = p0
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `phi0`", {
  set.seed(123)

  expect_snapshot(
    {
      # non-numeric 1
      p0_non_numeric_1 <- "bla"

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0_non_numeric_1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-numeric 2
      p0_non_numeric_2 <- NULL

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0_non_numeric_2
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # length > 1
      p0_too_long <- c(1, 2)

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0_too_long
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # NA-numeric
      p0_is_NA <- as.numeric(NA)

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0_is_NA
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `max_n_coalitions`", {
  set.seed(123)

  expect_snapshot(
    {
      # non-numeric 1
      max_n_comb_non_numeric_1 <- "bla"

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        max_n_coalitions = max_n_comb_non_numeric_1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-numeric 2
      max_n_comb_non_numeric_2 <- TRUE

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        max_n_coalitions = max_n_comb_non_numeric_2
      )
    },
    error = TRUE
  )


  expect_snapshot(
    {
      # non-integer
      max_n_coalitions_non_integer <- 10.5

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        max_n_coalitions = max_n_coalitions_non_integer
      )
    },
    error = TRUE
  )



  expect_snapshot(
    {
      # length > 1
      max_n_coalitions_too_long <- c(1, 2)

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        max_n_coalitions = max_n_coalitions_too_long
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # NA-numeric
      max_n_coalitions_is_NA <- as.numeric(NA)

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        max_n_coalitions = max_n_coalitions_is_NA
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # Non-positive
      max_n_comb_non_positive <- 0

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        max_n_coalitions = max_n_comb_non_positive
      )
    },
    error = TRUE
  )

  expect_snapshot({
    # Too low max_n_coalitions (smaller than # features
    max_n_coalitions <- ncol(x_explain_numeric) - 1

    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      phi0 = p0,
      approach = "gaussian",
      max_n_coalitions = max_n_coalitions
    )
  })


  expect_snapshot({
    # Too low max_n_coalitions (smaller than # groups
    groups <- list(
      A = c("Solar.R", "Wind"),
      B = c("Temp", "Month"),
      C = "Day"
    )

    max_n_coalitions <- length(groups) - 1

    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      phi0 = p0,
      approach = "gaussian",
      group = groups,
      max_n_coalitions = max_n_coalitions
    )
  })
})

test_that("erroneous input: `group`", {
  set.seed(123)

  expect_snapshot(
    {
      # not a list
      group_non_list <- "bla"

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        group = group_non_list
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-characters in list
      group_with_non_characters <- list(A = 1, B = 2)

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        group = group_with_non_characters
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        group = group_with_non_data_features
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        group = group_missing_data_features
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        group = group_dup_data_features
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # a single group only
      single_group <- list(A = c("Solar.R", "Wind", "Temp", "Month", "Day"))
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        group = single_group
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `n_MC_samples`", {
  set.seed(123)

  expect_snapshot(
    {
      # non-numeric 1
      n_samples_non_numeric_1 <- "bla"

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        n_MC_samples = n_samples_non_numeric_1
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-numeric 2
      n_samples_non_numeric_2 <- TRUE

      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        n_MC_samples = n_samples_non_numeric_2
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # non-integer
      n_samples_non_integer <- 10.5
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        n_MC_samples = n_samples_non_integer
      )
    },
    error = TRUE
  )

  # length > 1
  expect_snapshot(
    {
      n_samples_too_long <- c(1, 2)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        n_MC_samples = n_samples_too_long
      )
    },
    error = TRUE
  )

  # NA-numeric
  expect_snapshot(
    {
      n_samples_is_NA <- as.numeric(NA)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        n_MC_samples = n_samples_is_NA
      )
    },
    error = TRUE
  )

  # Non-positive
  expect_snapshot(
    {
      n_samples_non_positive <- 0
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        n_MC_samples = n_samples_non_positive
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        seed = seed_not_integer_interpretable
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        output_args = list(keep_samp_for_vS = keep_samp_for_vS_non_logical_1)
      )
    },
    error = TRUE
  )

  # non-logical 2
  expect_snapshot(
    {
      keep_samp_for_vS_non_logical_2 <- NULL
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        output_args = list(keep_samp_for_vS = keep_samp_for_vS_non_logical_2)
      )
    },
    error = TRUE
  )

  # length > 1
  expect_snapshot(
    {
      keep_samp_for_vS_too_long <- c(TRUE, FALSE)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        output_args = list(keep_samp_for_vS = keep_samp_for_vS_too_long)
      )
    },
    error = TRUE
  )
})

test_that("erroneous input: `MSEv_uniform_comb_weights`", {
  set.seed(123)

  # non-logical 1
  expect_snapshot(
    {
      MSEv_uniform_comb_weights_nl_1 <- "bla"
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        output_args = list(MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_nl_1)
      )
    },
    error = TRUE
  )

  # non-logical 2
  expect_snapshot(
    {
      MSEv_uniform_comb_weights_nl_2 <- NULL
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        output_args = list(MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_nl_2)
      )
    },
    error = TRUE
  )

  # length > 1
  expect_snapshot(
    {
      MSEv_uniform_comb_weights_long <- c(TRUE, FALSE)
      explain(
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        output_args = list(MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_long)
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        predict_model = predict_model_nonfunction
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        predict_model = predict_model_non_num_output
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        predict_model = predict_model_wrong_output_len
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        predict_model = predict_model_invalid_argument
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        predict_model = predict_model_error
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        get_model_specs = get_model_specs_nonfunction
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        get_model_specs = get_ms_output_not_list
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        get_model_specs = get_ms_output_too_long
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        get_model_specs = get_ms_output_wrong_names
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
        testing = TRUE,
        model = model_lm_numeric,
        x_explain = x_explain_numeric,
        x_train = x_train_numeric,
        approach = "independence",
        phi0 = p0,
        get_model_specs = get_model_specs_error
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
        testing = TRUE,
        model = model_lm_mixed,
        x_explain = x_explain_mixed,
        x_train = x_explain_mixed,
        approach = non_factor_approach_1,
        phi0 = p0
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # factor model/data with approach empirical
      non_factor_approach_2 <- "empirical"
      explain(
        testing = TRUE,
        model = model_lm_mixed,
        x_explain = x_explain_mixed,
        x_train = x_explain_mixed,
        approach = non_factor_approach_2,
        phi0 = p0
      )
    },
    error = TRUE
  )

  expect_snapshot(
    {
      # factor model/data with approach copula
      non_factor_approach_3 <- "copula"
      explain(
        testing = TRUE,
        model = model_lm_mixed,
        x_explain = x_explain_mixed,
        x_train = x_explain_mixed,
        approach = non_factor_approach_3,
        phi0 = p0
      )
    },
    error = TRUE
  )
})

test_that("Correct dimension of S when sampling combinations", {
  max_n_coalitions <- 10

  res <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_explain_mixed,
    phi0 = p0,
    approach = "ctree",
    max_n_coalitions = max_n_coalitions
  )

  expect_equal(nrow(res$internal$objects$S), max_n_coalitions)
})

test_that("Message with too low `max_n_coalitions`", {
  max_n_coalitions <- ncol(x_explain_numeric) - 1

  expect_snapshot(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_explain_numeric,
      phi0 = p0,
      approach = "gaussian",
      max_n_coalitions = max_n_coalitions
    )
  )

  # Same for groups
  groups <- list(
    A = c("Solar.R", "Wind"),
    B = c("Temp", "Month"),
    C = "Day"
  )

  max_n_coalitions <- length(groups) - 1

  expect_snapshot(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_explain_numeric,
      phi0 = p0,
      approach = "gaussian",
      group = groups,
      max_n_coalitions = max_n_coalitions
    )
  )
})

test_that("Shapr with `max_n_coalitions` >= 2^m uses exact Shapley kernel weights", {
  # Check that the `explain()` function enters the exact mode when max_n_coalitions
  # is larger than or equal to 2^m.

  # Create three explainer object: one with exact mode, one with
  # `max_n_coalitions` = 2^m, and one with `max_n_coalitions` > 2^m
  # No message as n_combination = NULL sets exact mode
  expect_snapshot(
    explanation_exact <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      n_MC_samples = 2, # Low value for fast computations
      seed = 123,
      max_n_coalitions = NULL,
      iterative = FALSE
    )
  )

  expect_snapshot(
    explanation_equal <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      n_MC_samples = 2, # Low value for fast computations
      seed = 123,
      extra_computation_args = list(compute_sd = FALSE),
      max_n_coalitions = 2^ncol(x_explain_numeric),
      iterative = FALSE
    )
  )

  # We should get a message saying that we are using the exact mode.
  # The `regexp` format match the one written in `create_coalition_table()`.
  expect_snapshot(
    explanation_larger <- explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      phi0 = p0,
      n_MC_samples = 2, # Low value for fast computations
      seed = 123,
      extra_computation_args = list(compute_sd = FALSE),
      max_n_coalitions = 2^ncol(x_explain_numeric) + 1,
      iterative = FALSE
    )
  )

  # Test that returned objects are identical (including all using the exact option and having the same Shapley weights)
  expect_equal(
    explanation_exact,
    explanation_equal
  )
  expect_equal(
    explanation_exact,
    explanation_larger
  )

  # Explicitly check that exact mode is set and that max_n_coalitions equals 2^ncol(x_explain_numeric) (32)
  # Since all 3 explanation objects are equal (per the above test) it suffices to do this for explanation_exact
  expect_true(
    explanation_exact$internal$parameters$exact
  )
  expect_equal(
    explanation_exact$internal$objects$X[, .N],
    2^ncol(x_explain_numeric)
  )
})

test_that("Correct dimension of S when sampling combinations with groups", {
  max_n_coalitions <- 6

  groups <- list(
    A = c("Solar.R", "Wind"),
    B = c("Temp", "Month_factor"),
    C = "Day"
  )

  res <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_explain_mixed,
    phi0 = p0,
    approach = "ctree",
    group = groups,
    max_n_coalitions = max_n_coalitions
  )

  expect_equal(nrow(res$internal$objects$S), max_n_coalitions)
})

test_that("data feature ordering is output_lm_numeric_column_order", {
  explain.original <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0
  )

  ex.new_data_feature_order <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = rev(x_explain_numeric),
    x_train = rev(x_train_numeric),
    approach = "empirical",
    phi0 = p0
  )

  explain.new_model_feat_order <- explain(
    testing = TRUE,
    model = model_lm_numeric_col_order,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0
  )

  # Same Shapley values, but different order
  expect_false(identical(
    explain.original$shapley_values_est,
    ex.new_data_feature_order$shapley_values_est
  ))
  expect_equal(
    explain.original$shapley_values_est[, mget(sort(names(explain.original$shapley_values_est)))],
    ex.new_data_feature_order$shapley_values_est[, mget(sort(names(ex.new_data_feature_order$shapley_values_est)))]
  )

  # Same Shapley values in same order
  expect_equal(explain.original, explain.new_model_feat_order)
})

test_that("parallelization gives same output for any approach", {
  testthat::skip_on_cran() # Avoiding CRAN Note: Running R code in ‘testthat.R’ had CPU time 3.6 times elapsed time
  # Empirical is seed independent
  explain.empirical_sequential <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0
  )

  future::plan("multisession", workers = 2) # Parallelized with 2 cores
  explain.empirical_multisession <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "empirical",
    phi0 = p0
  )

  future::plan("sequential") # Resetting to sequential computation

  # Identical results
  expect_equal(
    explain.empirical_sequential,
    explain.empirical_multisession
  )


  # ctree is seed NOT independent
  explain.ctree_sequential <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    phi0 = p0
  )

  future::plan("multisession", workers = 2) # Parallelized with 2 cores
  explain.ctree_multisession <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "ctree",
    phi0 = p0
  )

  future::plan("sequential") # Resetting to sequential computation

  # Identical results also for seed dependent methods.
  expect_equal(
    explain.ctree_sequential,
    explain.ctree_multisession
  )
})


test_that("gaussian approach use the user provided parameters", {
  # approach "gaussian" with default parameter estimation, i.e., sample mean and covariance
  e.gaussian_samp_mean_cov <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    phi0 = p0,
  )

  # Expect that gaussian.mu is the sample mean when no values are provided
  expect_equal(
    e.gaussian_samp_mean_cov$internal$parameters$gaussian.mu,
    colMeans(unname(x_train_numeric))
  )

  # Expect that gaussian.cov_mat is the sample covariance when no values are provided
  expect_equal(
    e.gaussian_samp_mean_cov$internal$parameters$gaussian.cov_mat,
    cov(x_train_numeric)
  )

  # Provide parameter values for the Gaussian approach
  gaussian.provided_mu <- seq_len(ncol(x_train_numeric)) # 1,2,3,4,5
  gaussian.provided_cov_mat <- diag(ncol(x_train_numeric))

  # approach "gaussian" with provided parameters
  e.gaussian_provided_mean_cov <- explain(
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    phi0 = p0,
    gaussian.mu = gaussian.provided_mu,
    gaussian.cov_mat = gaussian.provided_cov_mat
  )

  # Expect that the gaussian.mu parameter is the same as the provided gaussian.provided_mu
  expect_equal(
    e.gaussian_provided_mean_cov$internal$parameters$gaussian.mu,
    gaussian.provided_mu
  )

  # Expect that gaussian.cov_mat is the same as the provided gaussian.provided_cov_mat
  expect_equal(
    e.gaussian_provided_mean_cov$internal$parameters$gaussian.cov_mat,
    gaussian.provided_cov_mat
  )
})


test_that("setting the seed for combined approaches works", {
  # Check that setting the seed works for a combination of approaches
  explanation_combined_1 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = c("independence", "empirical", "gaussian", "copula"),
    phi0 = p0,
    seed = 1
  )

  explanation_combined_2 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = c("independence", "empirical", "gaussian", "copula"),
    phi0 = p0,
    seed = 1
  )

  # Check that they are equal
  expect_equal(explanation_combined_1, explanation_combined_2)
})

test_that("counting the number of unique approaches", {
  # Test several combinations of combined approaches and check that the number of
  # counted unique approaches is correct.
  # Recall that the last approach is not counted in `n_unique_approaches` as
  # we do not use it as we then condition on all features.
  explanation_combined_1 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = c("independence", "empirical", "gaussian", "copula"),
    phi0 = p0,
    seed = 1
  )
  expect_equal(explanation_combined_1$internal$parameters$n_approaches, 4)
  expect_equal(explanation_combined_1$internal$parameters$n_unique_approaches, 4)

  explanation_combined_2 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = c("empirical"),
    phi0 = p0,
    seed = 1
  )
  expect_equal(explanation_combined_2$internal$parameters$n_approaches, 1)
  expect_equal(explanation_combined_2$internal$parameters$n_unique_approaches, 1)

  explanation_combined_3 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = c("gaussian", "gaussian", "gaussian", "gaussian"),
    phi0 = p0,
    seed = 1
  )
  expect_equal(explanation_combined_3$internal$parameters$n_approaches, 4)
  expect_equal(explanation_combined_3$internal$parameters$n_unique_approaches, 1)

  explanation_combined_4 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = c("independence", "empirical", "independence", "empirical"),
    phi0 = p0,
    seed = 1
  )
  expect_equal(explanation_combined_4$internal$parameters$n_approaches, 4)
  expect_equal(explanation_combined_4$internal$parameters$n_unique_approaches, 2)

  # Check that the last one is not counted
  explanation_combined_5 <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = c("independence", "empirical", "independence", "empirical"),
    phi0 = p0,
    seed = 1
  )
  expect_equal(explanation_combined_5$internal$parameters$n_approaches, 4)
  expect_equal(explanation_combined_5$internal$parameters$n_unique_approaches, 2)
})



test_that("vaeac_set_seed_works", {
  skip_if_not(torch::torch_is_installed())
  # Train two vaeac models with the same seed
  explanation_vaeac_1 <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_train_mixed,
    approach = "vaeac",
    phi0 = p0,
    n_MC_samples = 10,
    seed = 1,
    vaeac.epochs = 4,
    vaeac.n_vaeacs_initialize = 2,
    vaeac.extra_parameters = list(
      vaeac.epochs_initiation_phase = 2
    ),
    iterative = FALSE
  )

  explanation_vaeac_2 <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_train_mixed,
    approach = "vaeac",
    phi0 = p0,
    n_MC_samples = 10,
    seed = 1,
    vaeac.epochs = 4,
    vaeac.n_vaeacs_initialize = 2,
    vaeac.extra_parameters = list(
      vaeac.epochs_initiation_phase = 2
    ),
    iterative = FALSE
  )

  # Check for equal Shapley values
  expect_equal(explanation_vaeac_1$shapley_values_est, explanation_vaeac_2$shapley_values_est)
})

test_that("vaeac_pretreained_vaeac_model", {
  skip_if_not(torch::torch_is_installed())

  # Test that we can skip training a new vaeac model if we already
  # have trained it in a previous shapr::explain object.

  explanation_vaeac_1 <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_train_mixed,
    approach = "vaeac",
    phi0 = p0,
    n_MC_samples = 10,
    seed = 1,
    vaeac.epochs = 4,
    vaeac.n_vaeacs_initialize = 2,
    vaeac.extra_parameters = list(
      vaeac.epochs_initiation_phase = 2
    ),
    iterative = FALSE
  )

  #### We can do this by reusing the vaeac model OBJECT
  # Get the pretrained vaeac model object
  vaeac.pretrained_vaeac_model <- explanation_vaeac_1$internal$parameters$vaeac

  # send the pre-trained vaeac model to the explain function
  explanation_pretrained_vaeac <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_train_mixed,
    approach = "vaeac",
    phi0 = p0,
    n_MC_samples = 10,
    seed = 1,
    vaeac.extra_parameters = list(
      vaeac.pretrained_vaeac_model = vaeac.pretrained_vaeac_model
    ),
    iterative = FALSE
  )

  # Check for equal Shapley values
  expect_equal(explanation_vaeac_1$shapley_values_est, explanation_pretrained_vaeac$shapley_values_est)

  #### We can also do this by reusing the vaeac model PATH
  # Get the pre-trained vaeac model path
  vaeac.pretrained_vaeac_path <- explanation_vaeac_1$internal$parameters$vaeac$models$best

  # send the pre-trained vaeac model to the explain function
  explanation_pretrained_vaeac <- explain(
    testing = TRUE,
    model = model_lm_mixed,
    x_explain = x_explain_mixed,
    x_train = x_train_mixed,
    approach = "vaeac",
    phi0 = p0,
    n_MC_samples = 10,
    seed = 1,
    vaeac.extra_parameters = list(
      vaeac.pretrained_vaeac_model = vaeac.pretrained_vaeac_path
    ),
    iterative = FALSE
  )

  # Check for equal Shapley values
  expect_equal(explanation_vaeac_1$shapley_values_est, explanation_pretrained_vaeac$shapley_values_est)
})


test_that("feature wise and groupwise computations are identical", {
  groups <- list(
    Solar.R = "Solar.R",
    Wind = "Wind",
    Temp = "Temp",
    Month = "Month",
    Day = "Day"
  )

  expl_feat <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    phi0 = p0
  )


  expl_group <- explain(
    testing = TRUE,
    model = model_lm_numeric,
    x_explain = x_explain_numeric,
    x_train = x_train_numeric,
    approach = "gaussian",
    group = groups,
    phi0 = p0
  )


  # Checking equality in the list with all final and intermediate results
  expect_equal(expl_feat$shapley_values_est, expl_group$shapley_values_est)
})
