# erroneous input: `response`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must be a numeric vector for computation of SAGE-values.

---

    Code
      y_non_numeric <- rep(c("A", "B", "C"), length.out = nrow(data_train))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_non_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must be a numeric vector for computation of SAGE-values.

---

    Code
      y_NA <- new_vector <- rep(NA, nrow(data_train))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_NA)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must be a numeric vector for computation of SAGE-values.

---

    Code
      y_single_int <- 2
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_single_int)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must have same number of observations as x_explain.

---

    Code
      y_short <- data_train[[y_var_numeric]][-1]
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_short)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must have same number of observations as x_explain.

# erroneous input: `sage`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = 3,
        response = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `sage` must be a single logical.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = c(
          TRUE, TRUE), response = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `sage` must be a single logical.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = NULL,
        response = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `sage` must be a single logical.

