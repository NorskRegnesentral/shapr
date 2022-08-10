# error with custom model without providing predict_model

    Code
      model_custom_lm_mixed <- model_lm_mixed
      class(model_custom_lm_mixed) <- "whatever"
      explain(x_train_mixed, x_explain_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0)
    Error <simpleError>
      You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' function to explain().
      See ?shapr::explain or the vignette for more information on how to run shapr with custom models.

# messages with missing detail in get_model_specs

    Code
      explain(x_train_mixed, x_explain_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = NA)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

---

    Code
      custom_get_model_specs_no_labels <- (function(x) {
        feature_specs <- list(labels = NA, classes = NA, factor_levels = NA)
      })
      explain(x_train_mixed, x_explain_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_labels)
    Message <simpleMessage>
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

---

    Code
      custom_get_model_specs_no_classes <- (function(x) {
        feature_specs <- list(labels = labels(x$terms), classes = NA, factor_levels = NA)
      })
      explain(x_train_mixed, x_explain_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_classes)
    Message <simpleMessage>
      Note: Feature classes extracted from the model contains NA.
      Assuming feature classes from the data are correct.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

---

    Code
      custom_get_model_specs_no_factor_levels <- (function(x) {
        feature_specs <- list(labels = labels(x$terms), classes = attr(x$terms,
        "dataClasses")[-1], factor_levels = NA)
      })
      explain(x_train_mixed, x_explain_mixed, model_custom_lm_mixed, approach = "independence",
        prediction_zero = p0, predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_factor_levels)
    Message <simpleMessage>
      Note: Feature factor levels extracted from the model contains NA.
      Assuming feature factor levels from the data are correct.
      
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.48359 18.4777 12.316 -3.4762     -0.21431
      2: 40.752 -0.85689  9.7603 25.769 -3.4762      9.40306

# erroneous input: `x_train/x_explain`

    Code
      x_train_wrong_format <- c(a = 1, b = 2)
      explain(x_train = x_train_wrong_format, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0)
    Error <simpleError>
      x_train should be a matrix or a data.frame/data.table.

---

    Code
      x_explain_wrong_format <- c(a = 1, b = 2)
      explain(x_train = x_explain_numeric, x_explain = x_explain_wrong_format,
        model_lm_numeric, approach = "independence", prediction_zero = p0)
    Error <simpleError>
      x_explain should be a matrix or a data.frame/data.table.

---

    Code
      x_train_wrong_format <- c(a = 1, b = 2)
      x_explain_wrong_format <- c(a = 3, b = 4)
      explain(x_train = x_train_wrong_format, x_explain = x_explain_wrong_format,
        model_lm_numeric, approach = "independence", prediction_zero = p0)
    Error <simpleError>
      x_train should be a matrix or a data.frame/data.table.
      x_explain should be a matrix or a data.frame/data.table.

---

    Code
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      names(x_train_no_column_names) <- NULL
      explain(x_train = x_train_no_column_names, x_explain = x_explain_numeric,
        model_lm_numeric, approach = "independence", prediction_zero = p0)
    Error <simpleError>
      x_train misses column names.

---

    Code
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL
      explain(x_train = x_train_numeric, x_explain = x_explain_no_column_names,
        model_lm_numeric, approach = "independence", prediction_zero = p0)
    Error <simpleError>
      x_explain misses column names.

---

    Code
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL
      explain(x_train = x_train_no_column_names, x_explain = x_explain_no_column_names,
        model_lm_numeric, approach = "independence", prediction_zero = p0)
    Error <simpleError>
      x_explain misses column names.

# erroneous input: `model/ignore_model`

    Code
      model_NULL <- NULL
      explain(x_train = x_train_numeric, x_explain = x_explain_numeric, model = model_NULL,
        approach = "independence", prediction_zero = p0)
    Error <simpleError>
      You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' function to explain().
      See ?shapr::explain or the vignette for more information on how to run shapr with custom models.

---

    Code
      ignore_model_TRUE <- TRUE
      explain(x_train = x_train_numeric, x_explain = x_explain_numeric, model = model_lm_numeric,
        approach = "independence", prediction_zero = p0, ignore_model = ignore_model_TRUE)
    Error <simpleError>
      object 'ignore_model' not found

# erroneous input: `approach`

    Code
      approach_non_character <- 1
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = approach_non_character, prediction_zero = p0)
    Error <simpleError>
      `approach` must be one of the following: 
       copula, ctree, empirical, gaussian, independence 
       or a vector of length equal to the number of features ( 5 ) with only the above strings.

---

    Code
      approach_incorrect_length <- c("empirical", "gaussian")
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = approach_incorrect_length, prediction_zero = p0)
    Error <simpleError>
      `approach` must be one of the following: 
       copula, ctree, empirical, gaussian, independence 
       or a vector of length equal to the number of features ( 5 ) with only the above strings.

---

    Code
      approach_incorrect_character <- "bla"
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = approach_incorrect_character, prediction_zero = p0)
    Error <simpleError>
      `approach` must be one of the following: 
       copula, ctree, empirical, gaussian, independence 
       or a vector of length equal to the number of features ( 5 ) with only the above strings.

# erroneous input: `prediction_zero`

    Code
      p0_non_numeric_1 <- "bla"
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_non_numeric_1)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

---

    Code
      p0_non_numeric_2 <- NULL
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_non_numeric_2)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

---

    Code
      p0_too_long <- c(1, 2)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_too_long)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

---

    Code
      p0_is_NA <- as.numeric(NA)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0_is_NA)
    Error <simpleError>
      `prediction_zero` must be a single numeric.

# erroneous input: `n_combinations`

    Code
      n_combinations_non_numeric_1 <- "bla"
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_combinations = n_combinations_non_numeric_1)
    Error <simpleError>
      `n_combinations` must be NULL or a single positive integer.

---

    Code
      n_combinations_non_numeric_2 <- TRUE
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_combinations = n_combinations_non_numeric_2)
    Error <simpleError>
      `n_combinations` must be NULL or a single positive integer.

---

    Code
      n_combinations_non_integer <- 10.5
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_combinations = n_combinations_non_integer)
    Error <simpleError>
      `n_combinations` must be NULL or a single positive integer.

---

    Code
      n_combinations_too_long <- c(1, 2)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_combinations = n_combinations_too_long)
    Error <simpleError>
      `n_combinations` must be NULL or a single positive integer.

---

    Code
      n_combinations_is_NA <- as.numeric(NA)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_combinations = n_combinations_is_NA)
    Error <simpleError>
      `n_combinations` must be NULL or a single positive integer.

---

    Code
      n_combinations_non_positive <- 0
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_combinations = n_combinations_non_positive)
    Error <simpleError>
      `n_combinations` must be NULL or a single positive integer.

# erroneous input: `group`

    Code
      group_non_list <- "bla"
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, group = group_non_list)
    Error <simpleError>
      `group` must be NULL or a list

---

    Code
      group_with_non_characters <- list(A = 1, B = 2)
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, group = group_with_non_characters)
    Error <simpleError>
      All components of group should be a character.

---

    Code
      group_with_non_data_features <- list(A = c("Solar.R", "Wind",
        "not_a_data_feature"), B = c("Temp", "Month", "Day"))
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, group = group_with_non_data_features)
    Error <simpleError>
      The group feature(s) not_a_data_feature are not
      among the features in the data: Solar.R, Wind, Temp, Month, Day. Delete from group.

---

    Code
      group_with_missing_data_features <- list(A = c("Solar.R"), B = c("Temp",
        "Month", "Day"))
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, group = group_with_missing_data_features)
    Error <simpleError>
      The data feature(s) Wind do not
      belong to one of the groups. Add to a group.

---

    Code
      group_with_duplicated_data_features <- list(A = c("Solar.R", "Solar.R", "Wind"),
      B = c("Temp", "Month", "Day"))
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, group = group_with_duplicated_data_features)
    Error <simpleError>
      Feature(s) Solar.R are found in more than one group or multiple times per group.
      Make sure each feature is only represented in one group, and only once.

# erroneous input: `n_samples`

    Code
      n_samples_non_numeric_1 <- "bla"
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_samples = n_samples_non_numeric_1)
    Error <simpleError>
      `n_samples` must be a single positive integer.

---

    Code
      n_samples_non_numeric_2 <- TRUE
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_samples = n_samples_non_numeric_2)
    Error <simpleError>
      `n_samples` must be a single positive integer.

---

    Code
      n_samples_non_integer <- 10.5
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_samples = n_samples_non_integer)
    Error <simpleError>
      `n_samples` must be a single positive integer.

---

    Code
      n_samples_too_long <- c(1, 2)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_samples = n_samples_too_long)
    Error <simpleError>
      `n_samples` must be a single positive integer.

---

    Code
      n_samples_is_NA <- as.numeric(NA)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_samples = n_samples_is_NA)
    Error <simpleError>
      `n_samples` must be a single positive integer.

---

    Code
      n_samples_non_positive <- 0
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_samples = n_samples_non_positive)
    Error <simpleError>
      `n_samples` must be a single positive integer.

# erroneous input: `n_batches`

    Code
      n_batches_non_numeric_1 <- "bla"
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_batches = n_batches_non_numeric_1)
    Error <simpleError>
      `n_batches` must be a single positive integer.

---

    Code
      n_batches_non_numeric_2 <- TRUE
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_batches = n_batches_non_numeric_2)
    Error <simpleError>
      `n_batches` must be a single positive integer.

---

    Code
      n_batches_non_integer <- 10.5
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_batches = n_batches_non_integer)
    Error <simpleError>
      `n_batches` must be a single positive integer.

---

    Code
      n_batches_too_long <- c(1, 2)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_batches = n_batches_too_long)
    Error <simpleError>
      `n_batches` must be a single positive integer.

---

    Code
      n_batches_is_NA <- as.numeric(NA)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_batches = n_batches_is_NA)
    Error <simpleError>
      `n_batches` must be a single positive integer.

---

    Code
      n_batches_non_positive <- 0
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_batches = n_batches_non_positive)
    Error <simpleError>
      `n_batches` must be a single positive integer.

---

    Code
      n_combinations <- 10
      n_batches_too_large <- 11
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, n_combinations = n_combinations, n_batches = n_batches_too_large)
    Error <simpleError>
      `n_batches` (11) is greater than the number feature combinations/`n_combinations` (10)

# erroneous input: `seed`

    Code
      seed_not_integer_interpretable <- "bla"
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, seed = seed_not_integer_interpretable)
    Warning <simpleWarning>
      NAs introduced by coercion
    Error <simpleError>
      supplied seed is not a valid integer

# erroneous input: `keep_samp_for_vS`

    Code
      keep_samp_for_vS_non_logical_1 <- "bla"
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, keep_samp_for_vS = keep_samp_for_vS_non_logical_1)
    Error <simpleError>
      `keep_samp_for_vS` must be single logical.

---

    Code
      keep_samp_for_vS_non_logical_2 <- NULL
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, keep_samp_for_vS = keep_samp_for_vS_non_logical_2)
    Error <simpleError>
      `keep_samp_for_vS` must be single logical.

---

    Code
      keep_samp_for_vS_too_long <- c(TRUE, FALSE)
      explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
        prediction_zero = p0, keep_samp_for_vS = keep_samp_for_vS_too_long)
    Error <simpleError>
      `keep_samp_for_vS` must be single logical.

---

    Code
      expect_snapshot({
        keep_samp_for_vS_too_long <- c(TRUE, FALSE)
        explain(x_train_numeric, x_explain_numeric, model_lm_numeric, approach = "independence",
          prediction_zero = p0, keep_samp_for_vS = keep_samp_for_vS_too_long)
      }, error = T)

# erroneous input: `predict_model`

    Code
      predict_model_nonfunction <- "bla"
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, predict_model = predict_model_nonfunction)
    Error <simpleError>
      `predict_model` must be NULL or a function.

---

    Code
      predict_model_non_numeric_output <- (function(model, x) {
        "bla"
      })
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, predict_model = predict_model_non_numeric_output)
    Error <simpleError>
      The predict_model function of class `lm` does not return a numeric output of the desired length.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      
      for more information on running shapr with custom models.

---

    Code
      predict_model_incorrect_output_length <- (function(model, x) {
        rep(1, nrow(x) + 1)
      })
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, predict_model = predict_model_incorrect_output_length)
    Error <simpleError>
      The predict_model function of class `lm` does not return a numeric output of the desired length.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      
      for more information on running shapr with custom models.

---

    Code
      predict_model_invalid_argument <- (function(model) {
        rep(1, nrow(x))
      })
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, predict_model = predict_model_invalid_argument)
    Error <simpleError>
      The predict_model function of class `lm` is invalid.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models.
      A basic function test threw the following error:
      Error in predict_model(model, x): unused argument (x)

---

    Code
      predict_model_error <- (function(model, x) {
        1 + "bla"
      })
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, predict_model = predict_model_error)
    Error <simpleError>
      The predict_model function of class `lm` is invalid.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models.
      A basic function test threw the following error:
      Error in 1 + "bla": non-numeric argument to binary operator

# erroneous input: `get_model_specs`

    Code
      get_model_specs_nonfunction <- "bla"
      explain(x_train = x_train_numeric, x_explain_numeric, model_lm_numeric,
        approach = "independence", prediction_zero = p0, get_model_specs = get_model_specs_nonfunction)
    Error <simpleError>
      `get_model_specs` must be NULL, NA or a function.

