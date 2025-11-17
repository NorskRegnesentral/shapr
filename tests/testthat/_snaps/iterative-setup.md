# erroneous input: `min_n_batches`

    Code
      n_batches_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_non_numeric_1))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

---

    Code
      n_batches_non_numeric_2 <- TRUE
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_non_numeric_2))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

---

    Code
      n_batches_non_integer <- 10.5
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_non_integer))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

---

    Code
      n_batches_too_long <- c(1, 2)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_too_long))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

---

    Code
      n_batches_is_NA <- as.numeric(NA)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_is_NA))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

---

    Code
      n_batches_non_positive <- 0
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_non_positive))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

# output_lm_numeric_independence_saving_and_cont_est

    Code
      print({
        out <- code
      }, digits = digits)
    Output
         explain_id  none Solar.R    Wind   Temp Month    Day
              <int> <num>   <num>   <num>  <num> <num>  <num>
      1:          1 42.44  -4.578   8.290 17.504 -5.54 -3.073
      2:          2 42.44   2.209  -3.325 -5.246 -5.54 -1.978
      3:          3 42.44   3.667 -18.590 -1.454 -2.50  1.309

---

    Code
      print({
        out <- code
      }, digits = digits)
    Output
         explain_id  none Solar.R    Wind   Temp Month    Day
              <int> <num>   <num>   <num>  <num> <num>  <num>
      1:          1 42.44  -4.578   8.290 17.504 -5.54 -3.073
      2:          2 42.44   2.209  -3.325 -5.246 -5.54 -1.978
      3:          3 42.44   3.667 -18.590 -1.454 -2.50  1.309

# output_lm_numeric_independence_keep_samp_for_vS

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Iterative computation started --
      
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
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.536   8.304 17.505 -5.593 -3.078
      2:          2 42.44   2.251  -3.311 -5.244 -5.593 -1.982
      3:          3 42.44   3.709 -18.576 -1.453 -2.553  1.304

