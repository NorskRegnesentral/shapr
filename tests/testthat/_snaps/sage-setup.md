# erroneous input: `y_explain`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `sage = TRUE`.

---

    Code
      y_non_numeric <- rep(c("A", "B", "C"), length.out = nrow(x_train_numeric))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        y_explain = y_non_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `sage = TRUE`.

---

    Code
      y_NA <- rep(NA_real_, nrow(x_train_numeric))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        y_explain = y_NA)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `sage = TRUE`.

---

    Code
      y_short <- y_train_numeric[-1]
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        y_explain = y_short)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `sage = TRUE`.

# erroneous input: `sage`

    Code
      sage_numeric <- 3
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = sage_numeric,
        y_explain = y_train_numeric)
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
        y_explain = y_train_numeric)
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
        y_explain = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `sage` must be a single logical.

# erroneous input: `sage_args$loss_func`

    Code
      loss_non_func <- 3
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        y_explain = y_train_numeric, sage_args = list(loss_func = loss_non_func))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_sage_args()`:
      ! `sage_args$loss_func` must be `NULL` or a function of exactly two arguments.

---

    Code
      loss_wrong_n_param <- (function(y, pred, loss) {
        expected <- (y - pred) * loss
        return(expected)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, sage = TRUE,
        y_explain = y_train_numeric, sage_args = list(loss_func = loss_wrong_n_param))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_sage_args()`:
      ! `sage_args$loss_func` must be `NULL` or a function of exactly two arguments.

