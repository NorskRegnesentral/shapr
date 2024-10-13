# error with custom model without providing predict_model

    Code
      model_custom_lm_mixed <- model_lm_mixed
      class(model_custom_lm_mixed) <- "whatever"
      explain(testing = TRUE, model = model_custom_lm_mixed, x_train = x_train_mixed,
        x_explain = x_explain_mixed, approach = "independence", prediction_zero = p0)
    Message
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `get_predict_model()`:
      ! You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' function to explain().
      See ?shapr::explain or the vignette for more information on how to run shapr with custom models.

# messages with missing detail in get_model_specs

    Code
      explain(testing = TRUE, model = model_custom_lm_mixed, x_train = x_train_mixed,
        x_explain = x_explain_mixed, approach = "independence", prediction_zero = p0,
        predict_model = custom_predict_model, get_model_specs = NA)
    Message
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <whatever>
      * Approach: independence
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2:          2 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3:          3 42.44   3.857 -17.469 -1.466  1.099        3.379

---

    Code
      custom_get_model_specs_no_lab <- (function(x) {
        feature_specs <- list(labels = NA, classes = NA, factor_levels = NA)
      })
      explain(testing = TRUE, model = model_custom_lm_mixed, x_train = x_train_mixed,
        x_explain = x_explain_mixed, approach = "independence", prediction_zero = p0,
        predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_lab)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <whatever>
      * Approach: independence
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2:          2 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3:          3 42.44   3.857 -17.469 -1.466  1.099        3.379

---

    Code
      custom_gms_no_classes <- (function(x) {
        feature_specs <- list(labels = labels(x$terms), classes = NA, factor_levels = NA)
      })
      explain(testing = TRUE, model = model_custom_lm_mixed, x_train = x_train_mixed,
        x_explain = x_explain_mixed, approach = "independence", prediction_zero = p0,
        predict_model = custom_predict_model, get_model_specs = custom_gms_no_classes)
    Message
      Note: Feature classes extracted from the model contains NA.
      Assuming feature classes from the data are correct.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <whatever>
      * Approach: independence
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2:          2 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3:          3 42.44   3.857 -17.469 -1.466  1.099        3.379

---

    Code
      custom_gms_no_factor_levels <- (function(x) {
        feature_specs <- list(labels = labels(x$terms), classes = attr(x$terms,
        "dataClasses")[-1], factor_levels = NA)
      })
      explain(testing = TRUE, model = model_custom_lm_mixed, x_train = x_train_mixed,
        x_explain = x_explain_mixed, approach = "independence", prediction_zero = p0,
        predict_model = custom_predict_model, get_model_specs = custom_gms_no_factor_levels)
    Message
      Note: Feature factor levels extracted from the model contains NA.
      Assuming feature factor levels from the data are correct.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <whatever>
      * Approach: independence
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2:          2 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3:          3 42.44   3.857 -17.469 -1.466  1.099        3.379

# erroneous input: `x_train/x_explain`

    Code
      x_train_wrong_format <- c(a = 1, b = 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_wrong_format, approach = "independence", prediction_zero = p0)
    Condition
      Error in `get_data()`:
      ! x_train should be a matrix or a data.frame/data.table.

---

    Code
      x_explain_wrong_format <- c(a = 1, b = 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_wrong_format,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0)
    Condition
      Error in `get_data()`:
      ! x_explain should be a matrix or a data.frame/data.table.

---

    Code
      x_train_wrong_format <- c(a = 1, b = 2)
      x_explain_wrong_format <- c(a = 3, b = 4)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_wrong_format,
        x_train = x_train_wrong_format, approach = "independence", prediction_zero = p0)
    Condition
      Error in `get_data()`:
      ! x_train should be a matrix or a data.frame/data.table.
      x_explain should be a matrix or a data.frame/data.table.

---

    Code
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      names(x_train_no_column_names) <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_no_column_names, approach = "independence",
        prediction_zero = p0)
    Condition
      Error in `get_data()`:
      ! x_train misses column names.

---

    Code
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_no_column_names,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0)
    Condition
      Error in `get_data()`:
      ! x_explain misses column names.

---

    Code
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_no_column_names,
        x_train = x_train_no_column_names, approach = "independence",
        prediction_zero = p0)
    Condition
      Error in `get_data()`:
      ! x_explain misses column names.

# erroneous input: `model`

    Code
      explain(testing = TRUE, x_explain = x_explain_numeric, x_train = x_train_numeric,
        approach = "independence", prediction_zero = p0)
    Condition
      Error in `explain()`:
      ! argument "model" is missing, with no default

# erroneous input: `approach`

    Code
      approach_non_character <- 1
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = approach_non_character,
        prediction_zero = p0)
    Condition
      Error in `check_approach()`:
      ! `approach` must be one of the following: 'categorical', 'copula', 'ctree', 'empirical', 'gaussian', 'independence', 'regression_separate', 'regression_surrogate', 'timeseries', 'vaeac'.
      These can also be combined (except 'regression_surrogate' and 'regression_separate') by passing a vector of length one less than the number of features (4).

---

    Code
      approach_incorrect_length <- c("empirical", "gaussian")
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = approach_incorrect_length,
        prediction_zero = p0)
    Condition
      Error in `check_approach()`:
      ! `approach` must be one of the following: 'categorical', 'copula', 'ctree', 'empirical', 'gaussian', 'independence', 'regression_separate', 'regression_surrogate', 'timeseries', 'vaeac'.
      These can also be combined (except 'regression_surrogate' and 'regression_separate') by passing a vector of length one less than the number of features (4).

---

    Code
      approach_incorrect_character <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = approach_incorrect_character,
        prediction_zero = p0)
    Condition
      Error in `check_approach()`:
      ! `approach` must be one of the following: 'categorical', 'copula', 'ctree', 'empirical', 'gaussian', 'independence', 'regression_separate', 'regression_surrogate', 'timeseries', 'vaeac'.
      These can also be combined (except 'regression_surrogate' and 'regression_separate') by passing a vector of length one less than the number of features (4).

# erroneous input: `prediction_zero`

    Code
      p0_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0_non_numeric_1)
    Condition
      Error in `get_parameters()`:
      ! `prediction_zero` (bla) must be numeric and match the output size of the model (1).

---

    Code
      p0_non_numeric_2 <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0_non_numeric_2)
    Condition
      Error in `get_parameters()`:
      ! `prediction_zero` () must be numeric and match the output size of the model (1).

---

    Code
      p0_too_long <- c(1, 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0_too_long)
    Condition
      Error in `get_parameters()`:
      ! `prediction_zero` (1, 2) must be numeric and match the output size of the model (1).

---

    Code
      p0_is_NA <- as.numeric(NA)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0_is_NA)
    Condition
      Error in `get_parameters()`:
      ! `prediction_zero` (NA) must be numeric and match the output size of the model (1).

# erroneous input: `max_n_coalitions`

    Code
      max_n_comb_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        max_n_coalitions = max_n_comb_non_numeric_1)
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_comb_non_numeric_2 <- TRUE
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        max_n_coalitions = max_n_comb_non_numeric_2)
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions_non_integer <- 10.5
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        max_n_coalitions = max_n_coalitions_non_integer)
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions_too_long <- c(1, 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        max_n_coalitions = max_n_coalitions_too_long)
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions_is_NA <- as.numeric(NA)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        max_n_coalitions = max_n_coalitions_is_NA)
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_comb_non_positive <- 0
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        max_n_coalitions = max_n_comb_non_positive)
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions <- ncol(x_explain_numeric) - 1
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, prediction_zero = p0, approach = "gaussian",
        max_n_coalitions = max_n_coalitions)
    Message
      Success with message:
      max_n_coalitions is smaller than max(10, n_features + 1 = 6),which will result in unreliable results.
      It is therefore set to 10.
      
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 6 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44 -1.4276 -1.4276  15.1967  1.6879 -1.4276
      2:          2 42.44 -0.9143 -0.9143 -10.8152 -0.3212 -0.9143
      3:          3 42.44 -5.8068 -5.8068   0.1677 -0.3155 -5.8068

---

    Code
      groups <- list(A = c("Solar.R", "Wind"), B = c("Temp", "Month"), C = "Day")
      max_n_coalitions <- length(groups) - 1
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, prediction_zero = p0, approach = "gaussian",
        group = groups, max_n_coalitions = max_n_coalitions)
    Message
      Success with message:
      n_groups is smaller than or equal to 3, meaning there are so few unique coalitions (8) that we should use all to get reliable results.
      max_n_coalitions is therefore set to 2^n_groups = 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
      * Number of group-wise Shapley values: 3
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   0.2636  13.7991 -1.4606
      2:          2 42.44   0.1788 -13.1512 -0.9071
      3:          3 42.44 -18.4998  -0.1635  1.0951

# erroneous input: `group`

    Code
      group_non_list <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        group = group_non_list)
    Condition
      Error in `get_parameters()`:
      ! `group` must be NULL or a list

---

    Code
      group_with_non_characters <- list(A = 1, B = 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        group = group_with_non_characters)
    Condition
      Error in `check_groups()`:
      ! All components of group should be a character.

---

    Code
      group_with_non_data_features <- list(A = c("Solar.R", "Wind",
        "not_a_data_feature"), B = c("Temp", "Month", "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        group = group_with_non_data_features)
    Condition
      Error in `check_groups()`:
      ! The group feature(s) not_a_data_feature are not
      among the features in the data: Solar.R, Wind, Temp, Month, Day. Delete from group.

---

    Code
      group_missing_data_features <- list(A = c("Solar.R"), B = c("Temp", "Month",
        "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        group = group_missing_data_features)
    Condition
      Error in `check_groups()`:
      ! The data feature(s) Wind do not
      belong to one of the groups. Add to a group.

---

    Code
      group_dup_data_features <- list(A = c("Solar.R", "Solar.R", "Wind"), B = c(
        "Temp", "Month", "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        group = group_dup_data_features)
    Condition
      Error in `check_groups()`:
      ! Feature(s) Solar.R are found in more than one group or multiple times per group.
      Make sure each feature is only represented in one group, and only once.

---

    Code
      single_group <- list(A = c("Solar.R", "Wind", "Temp", "Month", "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        group = single_group)
    Condition
      Error in `check_groups()`:
      ! You have specified only a single group named A, containing the features: Solar.R, Wind, Temp, Month, Day.
       The predictions must be decomposed in at least two groups to be meaningful.

# erroneous input: `n_MC_samples`

    Code
      n_samples_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        n_MC_samples = n_samples_non_numeric_1)
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_non_numeric_2 <- TRUE
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        n_MC_samples = n_samples_non_numeric_2)
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_non_integer <- 10.5
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        n_MC_samples = n_samples_non_integer)
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_too_long <- c(1, 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        n_MC_samples = n_samples_too_long)
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_is_NA <- as.numeric(NA)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        n_MC_samples = n_samples_is_NA)
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_non_positive <- 0
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        n_MC_samples = n_samples_non_positive)
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

# erroneous input: `seed`

    Code
      seed_not_integer_interpretable <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        seed = seed_not_integer_interpretable)
    Condition
      Warning in `set.seed()`:
      NAs introduced by coercion
      Error in `set.seed()`:
      ! supplied seed is not a valid integer

# erroneous input: `keep_samp_for_vS`

    Code
      keep_samp_for_vS_non_logical_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        keep_samp_for_vS = keep_samp_for_vS_non_logical_1)
    Condition
      Error in `get_parameters()`:
      ! `keep_samp_for_vS` must be single logical.

---

    Code
      keep_samp_for_vS_non_logical_2 <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        keep_samp_for_vS = keep_samp_for_vS_non_logical_2)
    Condition
      Error in `get_parameters()`:
      ! `keep_samp_for_vS` must be single logical.

---

    Code
      keep_samp_for_vS_too_long <- c(TRUE, FALSE)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        keep_samp_for_vS = keep_samp_for_vS_too_long)
    Condition
      Error in `get_parameters()`:
      ! `keep_samp_for_vS` must be single logical.

# erroneous input: `MSEv_uniform_comb_weights`

    Code
      MSEv_uniform_comb_weights_nl_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_nl_1)
    Condition
      Error in `get_parameters()`:
      ! `MSEv_uniform_comb_weights` must be single logical.

---

    Code
      MSEv_uniform_comb_weights_nl_2 <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_nl_2)
    Condition
      Error in `get_parameters()`:
      ! `MSEv_uniform_comb_weights` must be single logical.

---

    Code
      MSEv_uniform_comb_weights_long <- c(TRUE, FALSE)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_long)
    Condition
      Error in `get_parameters()`:
      ! `MSEv_uniform_comb_weights` must be single logical.

# erroneous input: `predict_model`

    Code
      predict_model_nonfunction <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        predict_model = predict_model_nonfunction)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `get_predict_model()`:
      ! `predict_model` must be NULL or a function.

---

    Code
      predict_model_non_num_output <- (function(model, x) {
        "bla"
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        predict_model = predict_model_non_num_output)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` does not return a numeric output of the desired length
      for single output models or a data.table of the correct
      dimensions for a multiple output model.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      
      for more information on running shapr with custom models.

---

    Code
      predict_model_wrong_output_len <- (function(model, x) {
        rep(1, nrow(x) + 1)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        predict_model = predict_model_wrong_output_len)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` does not return a numeric output of the desired length
      for single output models or a data.table of the correct
      dimensions for a multiple output model.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      
      for more information on running shapr with custom models.

---

    Code
      predict_model_invalid_argument <- (function(model) {
        rep(1, nrow(x))
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        predict_model = predict_model_invalid_argument)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` is invalid.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models.
      A basic function test threw the following error:
      Error in predict_model(model, x_test): unused argument (x_test)

---

    Code
      predict_model_error <- (function(model, x) {
        1 + "bla"
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        predict_model = predict_model_error)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` is invalid.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models.
      A basic function test threw the following error:
      Error in 1 + "bla": non-numeric argument to binary operator

# erroneous input: `get_model_specs`

    Code
      get_model_specs_nonfunction <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        get_model_specs = get_model_specs_nonfunction)
    Condition
      Error in `get_feature_specs()`:
      ! `get_model_specs` must be NULL, NA or a function.

---

    Code
      get_ms_output_not_list <- (function(x) {
        "bla"
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        get_model_specs = get_ms_output_not_list)
    Condition
      Error in `get_feature_specs()`:
      ! The `get_model_specs` function of class `lm` does not return a list of length 3 with elements "labels","classes","factor_levels".
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models and the required output format of get_model_specs.

---

    Code
      get_ms_output_too_long <- (function(x) {
        list(1, 2, 3, 4)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        get_model_specs = get_ms_output_too_long)
    Condition
      Error in `get_feature_specs()`:
      ! The `get_model_specs` function of class `lm` does not return a list of length 3 with elements "labels","classes","factor_levels".
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models and the required output format of get_model_specs.

---

    Code
      get_ms_output_wrong_names <- (function(x) {
        list(labels = 1, classes = 2, not_a_name = 3)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        get_model_specs = get_ms_output_wrong_names)
    Condition
      Error in `get_feature_specs()`:
      ! The `get_model_specs` function of class `lm` does not return a list of length 3 with elements "labels","classes","factor_levels".
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models and the required output format of get_model_specs.

---

    Code
      get_model_specs_error <- (function(x) {
        1 + "bla"
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        get_model_specs = get_model_specs_error)
    Condition
      Error in `get_feature_specs()`:
      ! The get_model_specs function of class `lm` is invalid.
      See the 'Advanced usage' section of the vignette:
      vignette('understanding_shapr', package = 'shapr')
      for more information on running shapr with custom models.
      Note that `get_model_specs` is not required (can be set to NULL)
      unless you require consistency checks between model and data.
      A basic function test threw the following error:
      Error in 1 + "bla": non-numeric argument to binary operator

# incompatible input: `data/approach`

    Code
      non_factor_approach_1 <- "gaussian"
      explain(testing = TRUE, model = model_lm_mixed, x_explain = x_explain_mixed,
        x_train = x_explain_mixed, approach = non_factor_approach_1, prediction_zero = p0)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `setup_approach.gaussian()`:
      ! The following feature(s) are factor(s): Month_factor.
      approach = 'gaussian' does not support factor features.
      Please change approach to one of 'independence' (not recommended), 'ctree', 'vaeac', 'categorical'.

---

    Code
      non_factor_approach_2 <- "empirical"
      explain(testing = TRUE, model = model_lm_mixed, x_explain = x_explain_mixed,
        x_train = x_explain_mixed, approach = non_factor_approach_2, prediction_zero = p0)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `setup_approach.empirical()`:
      ! The following feature(s) are factor(s): Month_factor.
      approach = 'empirical' does not support factor features.
      Please change approach to one of 'independence' (not recommended), 'ctree', 'vaeac', 'categorical'.

---

    Code
      non_factor_approach_3 <- "copula"
      explain(testing = TRUE, model = model_lm_mixed, x_explain = x_explain_mixed,
        x_train = x_explain_mixed, approach = non_factor_approach_3, prediction_zero = p0)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `setup_approach.copula()`:
      ! The following feature(s) are factor(s): Month_factor.
      approach = 'copula' does not support factor features.
      Please change approach to one of 'independence' (not recommended), 'ctree', 'vaeac', 'categorical'.

# Message with too low `max_n_coalitions`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_explain_numeric, prediction_zero = p0, approach = "gaussian",
        max_n_coalitions = max_n_coalitions)
    Message
      Success with message:
      max_n_coalitions is smaller than max(10, n_features + 1 = 6),which will result in unreliable results.
      It is therefore set to 10.
      
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 6 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month     Day
              <int> <num>   <num>   <num>  <num>   <num>   <num>
      1:          1 42.44  2.3585  2.3585  5.900 -0.3739  2.3585
      2:          2 42.44 -1.5323 -1.5323 -8.909 -0.3739 -1.5323
      3:          3 42.44 -0.7635 -0.7635 -6.441 -8.8373 -0.7635

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_explain_numeric, prediction_zero = p0, approach = "gaussian",
        group = groups, max_n_coalitions = max_n_coalitions)
    Message
      Success with message:
      n_groups is smaller than or equal to 3, meaning there are so few unique coalitions (8) that we should use all to get reliable results.
      max_n_coalitions is therefore set to 2^n_groups = 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
      * Number of group-wise Shapley values: 3
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none      A      B       C
              <int> <num>  <num>  <num>   <num>
      1:          1 42.44  5.589  5.591  1.4213
      2:          2 42.44 -6.637 -6.636 -0.6071
      3:          3 42.44 -5.439 -5.436 -6.6932

# Shapr with `max_n_coalitions` >= 2^m uses exact Shapley kernel weights

    Code
      explanation_exact <- explain(testing = TRUE, model = model_lm_numeric,
        x_explain = x_explain_numeric, x_train = x_train_numeric, approach = "gaussian",
        prediction_zero = p0, n_MC_samples = 2, seed = 123, max_n_coalitions = NULL,
        adaptive = FALSE)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 

---

    Code
      explanation_equal <- explain(testing = TRUE, model = model_lm_numeric,
        x_explain = x_explain_numeric, x_train = x_train_numeric, approach = "gaussian",
        prediction_zero = p0, n_MC_samples = 2, seed = 123, adaptive_arguments = list(
          compute_sd = FALSE), max_n_coalitions = 2^ncol(x_explain_numeric),
        adaptive = FALSE)
    Message
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 

---

    Code
      explanation_larger <- explain(testing = TRUE, model = model_lm_numeric,
        x_explain = x_explain_numeric, x_train = x_train_numeric, approach = "gaussian",
        prediction_zero = p0, n_MC_samples = 2, seed = 123, adaptive_arguments = list(
          compute_sd = FALSE), max_n_coalitions = 2^ncol(x_explain_numeric) + 1,
        adaptive = FALSE)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 

