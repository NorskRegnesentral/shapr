# erroneous input: `min_n_batches`

    Code
      n_batches_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(min_n_batches = n_batches_non_numeric_1))
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

# output_lm_numeric_independence_saving_and_cont_est

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
         explain_id  none Solar.R    Wind   Temp Month    Day
              <int> <num>   <num>   <num>  <num> <num>  <num>
      1:          1 42.44  -4.578   8.290 17.503 -5.54 -3.072
      2:          2 42.44   2.209  -3.325 -5.246 -5.54 -1.977
      3:          3 42.44   3.667 -18.590 -1.455 -2.50  1.310

---

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
         explain_id  none Solar.R    Wind   Temp Month    Day
              <int> <num>   <num>   <num>  <num> <num>  <num>
      1:          1 42.44  -4.578   8.290 17.503 -5.54 -3.072
      2:          2 42.44   2.209  -3.325 -5.246 -5.54 -1.977
      3:          3 42.44   3.667 -18.590 -1.455 -2.50  1.310

# output_lm_numeric_independence_keep_samp_for_vS

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.532   8.262 17.522 -5.582 -3.068
      2:          2 42.44   2.255  -3.352 -5.227 -5.582 -1.972
      3:          3 42.44   3.713 -18.617 -1.436 -2.542  1.314

