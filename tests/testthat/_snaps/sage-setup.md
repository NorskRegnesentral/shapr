# erroneous input: `response`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must be a numeric vector with the same number of elements as rows in `x_explain`.

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
      ! `response` must be a numeric vector with the same number of elements as rows in `x_explain`.

---

    Code
      y_NA <- rep(NA, nrow(data_train))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_NA)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must be a numeric vector with the same number of elements as rows in `x_explain`.

---

    Code
      y_short <- y_train_numeric[-1]
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_short)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `response` must be a numeric vector with the same number of elements as rows in `x_explain`.

# erroneous input: `sage`

    Code
      sage_numeric <- 3
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = sage_numeric,
        response = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `sage` must be a single logical.

---

    Code
      sage_vec <- c(TRUE, TRUE)
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = sage_vec,
        response = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `sage` must be a single logical.

---

    Code
      sage_null <- NULL
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = sage_null,
        response = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `sage` must be a single logical.

# erroneous input: `loss_func`

    Code
      loss_non_func <- 3
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_train_numeric, loss_func = loss_non_func)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `setup()`:
      ! `loss_func` must be a function of two parameters.

---

    Code
      loss_wrong_n_param <- (function(y, pred, loss) {
        expected <- (y - pred) * loss
        return(expected)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        response = y_train_numeric, loss_func = loss_wrong_n_param)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `setup()`:
      ! `loss_func` must be a function of two parameters.

