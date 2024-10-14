# erroneous input: `min_n_batches`

    Code
      n_batches_non_numeric_1 <- "bla"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
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
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
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
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
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
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
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
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
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
        x_train = x_train_numeric, approach = "independence", prediction_zero = p0,
        extra_computation_args = list(min_n_batches = n_batches_non_positive))
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$min_n_batches` must be NULL or a single positive integer.

