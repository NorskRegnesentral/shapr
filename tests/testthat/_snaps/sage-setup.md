# erroneous input: `y_explain`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = "global")
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `scope = "global"`.

---

    Code
      y_non_numeric <- rep(c("A", "B", "C"), length.out = nrow(x_train_numeric))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = "global",
        y_explain = y_non_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `scope = "global"`.

---

    Code
      y_NA <- rep(NA_real_, nrow(x_train_numeric))
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = "global",
        y_explain = y_NA)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `scope = "global"`.

---

    Code
      y_short <- y_train_numeric[-1]
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = "global",
        y_explain = y_short)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_data()`:
      ! `y_explain` must be a numeric vector without `NA`s and with the same number of elements as there are rows in `x_explain` when `scope = "global"`.

# erroneous input: `scope`

    Code
      scope_numeric <- 3
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = scope_numeric,
        y_explain = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `scope` must be either `local` or `global`.

---

    Code
      scope_invalid <- "both"
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = scope_invalid,
        y_explain = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `scope` must be either `local` or `global`.

---

    Code
      scope_vec <- c("local", "global")
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = scope_vec,
        y_explain = y_train_numeric)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `scope` must be either `local` or `global`.

# erroneous input: `extra_computation_args$global_loss_func`

    Code
      loss_non_func <- 3
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = "global",
        y_explain = y_train_numeric, extra_computation_args = list(global_loss_func = loss_non_func))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$global_loss_func` must be `NULL` or a function of exactly two arguments.

---

    Code
      loss_wrong_n_param <- (function(y, pred, loss) {
        expected <- (y - pred) * loss
        return(expected)
      })
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_train_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1, scope = "global",
        y_explain = y_train_numeric, extra_computation_args = list(global_loss_func = loss_wrong_n_param))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_extra_computation_args()`:
      ! `extra_computation_args$global_loss_func` must be `NULL` or a function of exactly two arguments.

