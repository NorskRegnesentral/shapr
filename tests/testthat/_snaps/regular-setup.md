# error with custom model without providing predict_model

    Code
      model_custom_lm_mixed <- model_lm_mixed
      class(model_custom_lm_mixed) <- "whatever"
      explain(testing = TRUE, model = model_custom_lm_mixed, x_train = x_train_mixed,
        x_explain = x_explain_mixed, approach = "independence", phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
        Consistency checks between model and data is therefore disabled.
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `get_predict_model()`:
      ! You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' function to explain(). See ?shapr::explain or the vignette for more information on how to run shapr with custom models.

# messages with missing detail in get_model_specs

    Code
      explain(testing = TRUE, model = model_custom_lm_mixed, x_train = x_train_mixed,
        x_explain = x_explain_mixed, approach = "independence", phi0 = p0, seed = 1,
        predict_model = custom_predict_model, get_model_specs = NA)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
        Consistency checks between model and data is therefore disabled.
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <whatever>
      * Approach: independence
      * Iterative estimation: FALSE
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
        x_explain = x_explain_mixed, approach = "independence", phi0 = p0, seed = 1,
        predict_model = custom_predict_model, get_model_specs = custom_get_model_specs_no_lab)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i Feature names extracted from the model contains NA.
        Consistency checks between model and data is therefore disabled.
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <whatever>
      * Approach: independence
      * Iterative estimation: FALSE
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
        x_explain = x_explain_mixed, approach = "independence", phi0 = p0, seed = 1,
        predict_model = custom_predict_model, get_model_specs = custom_gms_no_classes)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i Feature classes extracted from the model contains NA.
        Assuming feature classes from the data are correct.
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <whatever>
      * Approach: independence
      * Iterative estimation: FALSE
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
        x_explain = x_explain_mixed, approach = "independence", phi0 = p0, seed = 1,
        predict_model = custom_predict_model, get_model_specs = custom_gms_no_factor_levels)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i Feature factor levels extracted from the model contains NA.
        Assuming feature factor levels from the data are correct.
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <whatever>
      * Approach: independence
      * Iterative estimation: FALSE
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
        x_train = x_train_wrong_format, approach = "independence", phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_data()`:
      ! x_train should be a matrix or a data.frame/data.table.

---

    Code
      x_explain_wrong_format <- c(a = 1, b = 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_wrong_format,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_data()`:
      ! x_explain should be a matrix or a data.frame/data.table.

---

    Code
      x_train_wrong_format <- c(a = 1, b = 2)
      x_explain_wrong_format <- c(a = 3, b = 4)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_wrong_format,
        x_train = x_train_wrong_format, approach = "independence", phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_data()`:
      ! x_train should be a matrix or a data.frame/data.table.
      ! x_explain should be a matrix or a data.frame/data.table.

---

    Code
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      names(x_train_no_column_names) <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_no_column_names, approach = "independence", phi0 = p0,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_data()`:
      ! x_train misses column names.

---

    Code
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_no_column_names,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_data()`:
      ! x_explain misses column names.

---

    Code
      x_train_no_column_names <- as.data.frame(x_train_numeric)
      x_explain_no_column_names <- as.data.frame(x_explain_numeric)
      names(x_explain_no_column_names) <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_no_column_names,
        x_train = x_train_no_column_names, approach = "independence", phi0 = p0,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_data()`:
      ! x_explain misses column names.

# erroneous input: `model`

    Code
      explain(testing = TRUE, x_explain = x_explain_numeric, x_train = x_train_numeric,
        approach = "independence", phi0 = p0, seed = 1)
    Condition
      Error in `explain()`:
      ! argument "model" is missing, with no default

# erroneous input: `approach`

    Code
      approach_non_character <- 1
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = approach_non_character, phi0 = p0,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_approach()`:
      ! `approach` must be one of the following: 'categorical', 'copula', 'ctree', 'empirical', 'gaussian', 'independence', 'regression_separate', 'regression_surrogate', 'timeseries', 'vaeac'. These can also be combined (except 'regression_surrogate' and 'regression_separate') by passing a vector of length one less than the number of features (4).

---

    Code
      approach_incorrect_length <- c("empirical", "gaussian")
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = approach_incorrect_length, phi0 = p0,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_approach()`:
      ! `approach` must be one of the following: 'categorical', 'copula', 'ctree', 'empirical', 'gaussian', 'independence', 'regression_separate', 'regression_surrogate', 'timeseries', 'vaeac'. These can also be combined (except 'regression_surrogate' and 'regression_separate') by passing a vector of length one less than the number of features (4).

---

    Code
      approach_incorrect_character <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = approach_incorrect_character, phi0 = p0,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_approach()`:
      ! `approach` must be one of the following: 'categorical', 'copula', 'ctree', 'empirical', 'gaussian', 'independence', 'regression_separate', 'regression_surrogate', 'timeseries', 'vaeac'. These can also be combined (except 'regression_surrogate' and 'regression_separate') by passing a vector of length one less than the number of features (4).

# erroneous input: `phi0`

    Code
      p0_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0_non_numeric_1,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `phi0` (bla) must be numeric and match the output size of the model (1).

---

    Code
      p0_non_numeric_2 <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0_non_numeric_2,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `phi0` () must be numeric and match the output size of the model (1).

---

    Code
      p0_too_long <- c(1, 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0_too_long,
        seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `phi0` (1, 2) must be numeric and match the output size of the model (1).

---

    Code
      p0_is_NA <- as.numeric(NA)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0_is_NA, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `phi0` (NA) must be numeric and match the output size of the model (1).

# erroneous input: `max_n_coalitions`

    Code
      max_n_comb_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        max_n_coalitions = max_n_comb_non_numeric_1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_comb_non_numeric_2 <- TRUE
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        max_n_coalitions = max_n_comb_non_numeric_2)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions_non_integer <- 10.5
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        max_n_coalitions = max_n_coalitions_non_integer)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions_too_long <- c(1, 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        max_n_coalitions = max_n_coalitions_too_long)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions_is_NA <- as.numeric(NA)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        max_n_coalitions = max_n_coalitions_is_NA)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_comb_non_positive <- 0
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        max_n_coalitions = max_n_comb_non_positive)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `max_n_coalitions` must be NULL or a single positive integer.

---

    Code
      max_n_coalitions <- ncol(x_explain_numeric) - 1
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "gaussian",
        max_n_coalitions = max_n_coalitions)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is smaller than max(10, n_features + 1 = 6), which will result in unreliable results.
        It is therefore set to 10.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 6 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44 -0.4778 -0.4778  15.3062 -0.4778 -1.2708
      2:          2 42.44 -0.7900 -0.7900 -10.7058 -0.7900 -0.8036
      3:          3 42.44 -6.2514 -6.2514   0.2771 -6.2514  0.9088

---

    Code
      groups <- list(A = c("Solar.R", "Wind"), B = c("Temp", "Month"), C = "Day")
      max_n_coalitions <- length(groups) - 1
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "gaussian", group = groups,
        max_n_coalitions = max_n_coalitions)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i n_groups is smaller than or equal to 3, meaning there are so few unique coalitions (8) that we should use all to get reliable results.
        max_n_coalitions is therefore set to 2^n_groups = 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        group = group_non_list)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `group` must be NULL or a list

---

    Code
      group_with_non_characters <- list(A = 1, B = 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        group = group_with_non_characters)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_groups()`:
      ! All components of group should be a character.

---

    Code
      group_with_non_data_features <- list(A = c("Solar.R", "Wind",
        "not_a_data_feature"), B = c("Temp", "Month", "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        group = group_with_non_data_features)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_groups()`:
      ! The group feature(s) not_a_data_feature are not among the features in the data: Solar.R, Wind, Temp, Month, Day. Delete from group.

---

    Code
      group_missing_data_features <- list(A = c("Solar.R"), B = c("Temp", "Month",
        "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        group = group_missing_data_features)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_groups()`:
      ! The data feature(s) Wind do not belong to one of the groups. Add to a group.

---

    Code
      group_dup_data_features <- list(A = c("Solar.R", "Solar.R", "Wind"), B = c(
        "Temp", "Month", "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        group = group_dup_data_features)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_groups()`:
      ! Feature(s) Solar.R are found in more than one group or multiple times per group. Make sure each feature is only represented in one group, and only once.

---

    Code
      single_group <- list(A = c("Solar.R", "Wind", "Temp", "Month", "Day"))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        group = single_group)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_groups()`:
      ! You have specified only a single group named A, containing the features: Solar.R, Wind, Temp, Month, Day. The predictions must be decomposed in at least two groups to be meaningful.

# erroneous input: `n_MC_samples`

    Code
      n_samples_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        n_MC_samples = n_samples_non_numeric_1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_non_numeric_2 <- TRUE
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        n_MC_samples = n_samples_non_numeric_2)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_non_integer <- 10.5
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        n_MC_samples = n_samples_non_integer)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_too_long <- c(1, 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        n_MC_samples = n_samples_too_long)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_is_NA <- as.numeric(NA)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        n_MC_samples = n_samples_is_NA)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

---

    Code
      n_samples_non_positive <- 0
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        n_MC_samples = n_samples_non_positive)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `n_MC_samples` must be a single positive integer.

# erroneous input: `seed`

    Code
      seed_not_integer_interpretable <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = seed_not_integer_interpretable)
    Condition
      Warning in `set.seed()`:
      NAs introduced by coercion
      Error in `set.seed()`:
      ! supplied seed is not a valid integer

# erroneous input: `keep_samp_for_vS`

    Code
      keep_samp_for_vS_non_logical_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        output_args = list(keep_samp_for_vS = keep_samp_for_vS_non_logical_1))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `check_output_args()`:
      ! `output_args$keep_samp_for_vS` must be single logical.

---

    Code
      keep_samp_for_vS_non_logical_2 <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        output_args = list(keep_samp_for_vS = keep_samp_for_vS_non_logical_2))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `check_output_args()`:
      ! `output_args$keep_samp_for_vS` must be single logical.

---

    Code
      keep_samp_for_vS_too_long <- c(TRUE, FALSE)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        output_args = list(keep_samp_for_vS = keep_samp_for_vS_too_long))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `check_output_args()`:
      ! `output_args$keep_samp_for_vS` must be single logical.

# erroneous input: `MSEv_uniform_comb_weights`

    Code
      MSEv_uniform_comb_weights_nl_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        output_args = list(MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_nl_1))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `check_output_args()`:
      ! `output_args$MSEv_uniform_comb_weights` must be single logical.

---

    Code
      MSEv_uniform_comb_weights_nl_2 <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        output_args = list(MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_nl_2))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `check_output_args()`:
      ! `output_args$MSEv_uniform_comb_weights` must be single logical.

---

    Code
      MSEv_uniform_comb_weights_long <- c(TRUE, FALSE)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        output_args = list(MSEv_uniform_comb_weights = MSEv_uniform_comb_weights_long))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `check_output_args()`:
      ! `output_args$MSEv_uniform_comb_weights` must be single logical.

# erroneous input: `predict_model`

    Code
      predict_model_nonfunction <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        predict_model = predict_model_nonfunction)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `get_predict_model()`:
      ! `predict_model` must be NULL or a function.

---

    Code
      predict_model_non_num_output <- (function(model, x) {
        "bla"
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        predict_model = predict_model_non_num_output)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` does not return a numeric output of the desired length for single output models or a data.table of the correct dimensions for a multiple output model. See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models.

---

    Code
      predict_model_wrong_output_len <- (function(model, x) {
        rep(1, nrow(x) + 1)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        predict_model = predict_model_wrong_output_len)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` does not return a numeric output of the desired length for single output models or a data.table of the correct dimensions for a multiple output model. See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models.

---

    Code
      predict_model_invalid_argument <- (function(model) {
        rep(1, nrow(x))
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        predict_model = predict_model_invalid_argument)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` is invalid. See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models. A basic function test threw the following error: Error in predict_model(model, x_test): unused argument (x_test)

---

    Code
      predict_model_error <- (function(model, x) {
        1 + "bla"
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        predict_model = predict_model_error)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `test_predict_model()`:
      ! The predict_model function of class `lm` is invalid. See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models. A basic function test threw the following error: Error in 1 + "bla": non-numeric argument to binary operator

# erroneous input: `get_model_specs`

    Code
      get_model_specs_nonfunction <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
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
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        get_model_specs = get_ms_output_not_list)
    Condition
      Error in `get_feature_specs()`:
      ! The `get_model_specs` function of class `lm` does not return a list of length 3 with elements "labels","classes","factor_levels". See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models and the required output format of get_model_specs.

---

    Code
      get_ms_output_too_long <- (function(x) {
        list(1, 2, 3, 4)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        get_model_specs = get_ms_output_too_long)
    Condition
      Error in `get_feature_specs()`:
      ! The `get_model_specs` function of class `lm` does not return a list of length 3 with elements "labels","classes","factor_levels". See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models and the required output format of get_model_specs.

---

    Code
      get_ms_output_wrong_names <- (function(x) {
        list(labels = 1, classes = 2, not_a_name = 3)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        get_model_specs = get_ms_output_wrong_names)
    Condition
      Error in `get_feature_specs()`:
      ! The `get_model_specs` function of class `lm` does not return a list of length 3 with elements "labels","classes","factor_levels". See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models and the required output format of get_model_specs.

---

    Code
      get_model_specs_error <- (function(x) {
        1 + "bla"
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        get_model_specs = get_model_specs_error)
    Condition
      Error in `get_feature_specs()`:
      ! The get_model_specs function of class `lm` is invalid. See the 'Advanced usage' section of the vignette: vignette('general_usage', package = 'shapr') for more information on running shapr with custom models. Note that `get_model_specs` is not required (can be set to NULL) unless you require consistency checks between model and data. A basic function test threw the following error: Error in 1 + "bla": non-numeric argument to binary operator

# incompatible input: `data/approach`

    Code
      non_factor_approach_1 <- "gaussian"
      explain(testing = TRUE, model = model_lm_mixed, x_explain = x_explain_mixed,
        x_train = x_explain_mixed, approach = non_factor_approach_1, phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `setup_approach()`:
      ! The following feature(s) are factor(s): Month_factor. approach = 'gaussian' does not support factor features. Please change approach to one of 'independence' (not recommended), 'ctree', 'vaeac', 'categorical', 'regression_separate', 'regression_surrogate'.

---

    Code
      non_factor_approach_2 <- "empirical"
      explain(testing = TRUE, model = model_lm_mixed, x_explain = x_explain_mixed,
        x_train = x_explain_mixed, approach = non_factor_approach_2, phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `setup_approach()`:
      ! The following feature(s) are factor(s): Month_factor. approach = 'empirical' does not support factor features. Please change approach to one of 'independence' (not recommended), 'ctree', 'vaeac', 'categorical', 'regression_separate', 'regression_surrogate'.

---

    Code
      non_factor_approach_3 <- "copula"
      explain(testing = TRUE, model = model_lm_mixed, x_explain = x_explain_mixed,
        x_train = x_explain_mixed, approach = non_factor_approach_3, phi0 = p0, seed = 1)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
    Condition
      Error in `setup_approach()`:
      ! The following feature(s) are factor(s): Month_factor. approach = 'copula' does not support factor features.Please change approach to one of 'independence' (not recommended), 'ctree', 'vaeac', 'categorical', 'regression_separate', 'regression_surrogate'.

# Message with too low `max_n_coalitions`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_explain_numeric, phi0 = p0, seed = 1, approach = "gaussian",
        max_n_coalitions = max_n_coalitions)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is smaller than max(10, n_features + 1 = 6), which will result in unreliable results.
        It is therefore set to 10.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 6 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month     Day
              <int> <num>   <num>   <num>  <num>   <num>   <num>
      1:          1 42.44  1.6821  1.6821  5.891  1.6821   1.665
      2:          2 42.44 -1.1944 -1.1944 -8.918 -1.1944  -1.378
      3:          3 42.44 -0.2038 -0.2038 -6.450 -0.2038 -10.507

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_explain_numeric, phi0 = p0, seed = 1, approach = "gaussian",
        group = groups, max_n_coalitions = max_n_coalitions)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i n_groups is smaller than or equal to 3, meaning there are so few unique coalitions (8) that we should use all to get reliable results.
        max_n_coalitions is therefore set to 2^n_groups = 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
        phi0 = p0, n_MC_samples = 2, seed = 123, max_n_coalitions = NULL, iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 

---

    Code
      explanation_equal <- explain(testing = TRUE, model = model_lm_numeric,
        x_explain = x_explain_numeric, x_train = x_train_numeric, approach = "gaussian",
        phi0 = p0, n_MC_samples = 2, seed = 123, extra_computation_args = list(
          compute_sd = FALSE), max_n_coalitions = 2^ncol(x_explain_numeric),
        iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 

---

    Code
      explanation_larger <- explain(testing = TRUE, model = model_lm_numeric,
        x_explain = x_explain_numeric, x_train = x_train_numeric, approach = "gaussian",
        phi0 = p0, n_MC_samples = 2, seed = 123, extra_computation_args = list(
          compute_sd = FALSE), max_n_coalitions = 2^ncol(x_explain_numeric) + 1,
        iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 

# output_custom_lm_numeric_independence_2

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
        Consistency checks between model and data is therefore disabled.
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <whatever>
      * Approach: independence
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_independence_keep_samp_for_vS

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: independence
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_vS_batching_method

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: ctree
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 24 of 32 coalitions, 6 new. 
      
      -- Iteration 6 -----------------------------------------------------------------
      i Using 28 of 32 coalitions, 4 new. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -8.187   8.808 16.651 -1.355 -3.315
      2:          2 42.44   5.551  -6.142 -8.670 -2.622 -1.997
      3:          3 42.44   7.009 -21.407 -4.878  0.418  1.290

