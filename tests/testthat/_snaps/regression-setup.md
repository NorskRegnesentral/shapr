# regression erroneous input: `approach`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = c(
          "regression_surrogate", "gaussian", "independence", "empirical"),
        iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_approach()`:
      ! The `regression_separate` and `regression_surrogate` approaches cannot be combined with other approaches.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = c(
          "regression_separate", "gaussian", "independence", "empirical"), iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
    Condition
      Error in `check_approach()`:
      ! The `regression_separate` and `regression_surrogate` approaches cannot be combined with other approaches.

# regression erroneous input: `regression.model`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = NULL)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! `regression.model` must be a tidymodels object with class 'model_spec'. See documentation.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = lm)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! `regression.model` must be a tidymodels object with class 'model_spec'. See documentation.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! `regression.tune_values` must be provided when `regression.model` contains hyperparameters to tune.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"), regression.tune_values = data.frame(
          num_terms = c(1, 2, 3)))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! The tunable parameters in `regression.model` ('tree_depth') and `regression.tune_values` ('num_terms') must match.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"), regression.tune_values = data.frame(
          tree_depth = c(1, 2, 3), num_terms = c(1, 2, 3)))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! The tunable parameters in `regression.model` ('tree_depth') and `regression.tune_values` ('tree_depth', 'num_terms') must match.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = 2, engine = "rpart",
          mode = "regression"), regression.tune_values = data.frame(tree_depth = c(1,
          2, 3)))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! The tunable parameters in `regression.model` ('') and `regression.tune_values` ('tree_depth') must match.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_surrogate",
        regression.tune_values = data.frame(tree_depth = c(1, 2, 3)), iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Condition
      Error in `regression.get_tune()`:
      ! The tunable parameters in `regression.model` ('') and `regression.tune_values` ('tree_depth') must match.

# regression erroneous input: `regression.tune_values`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = 2, engine = "rpart",
          mode = "regression"), regression.tune_values = as.matrix(data.frame(
          tree_depth = c(1, 2, 3))))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! `regression.tune_values` must be of either class `data.frame` or `function`. See documentation.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"), regression.tune_values = function(x)
          c(1, 2, 3))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! The output of the user provided `regression.tune_values` function must be of class `data.frame`.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"), regression.tune_values = function(x)
          data.frame(wrong_name = c(1, 2, 3)))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.get_tune()`:
      ! The tunable parameters in `regression.model` ('tree_depth') and `regression.tune_values` ('wrong_name') must match.

# regression erroneous input: `regression.vfold_cv_para`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"), regression.tune_values = data.frame(
          tree_depth = c(1, 2, 3)), regression.vfold_cv_para = 10)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.check_vfold_cv_para()`:
      ! `regression.vfold_cv_para` must be a named list. See the documentation of `shapr::explain()`.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"), regression.tune_values = data.frame(
          tree_depth = c(1, 2, 3)), regression.vfold_cv_para = list(10))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.check_vfold_cv_para()`:
      ! `regression.vfold_cv_para` must be a named list. See the documentation of `shapr::explain()`.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.model = parsnip::decision_tree(tree_depth = parsnip::tune(),
        engine = "rpart", mode = "regression"), regression.tune_values = data.frame(
          tree_depth = c(1, 2, 3)), regression.vfold_cv_para = list(hey = 10))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.check_vfold_cv_para()`:
      ! The following parameters in `regression.vfold_cv_para` are not supported by `rsample::vfold_cv()`: 'hey'.

# regression erroneous input: `regression.recipe_func`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_separate",
        regression.recipe_func = 3)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `regression.check_recipe_func()`:
      ! `regression.recipe_func` must be a function. See documentation.

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_surrogate",
        regression.recipe_func = function(x) {
          return(2)
        }, iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Condition
      Error in `regression.check_recipe_func()`:
      ! The output of the `regression.recipe_func` must be of class `recipe`.

# regression erroneous input: `regression.surrogate_n_comb`

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_surrogate",
        regression.surrogate_n_comb = 2^ncol(x_explain_numeric) - 1, iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Condition
      Error in `regression.check_sur_n_comb()`:
      ! `regression.surrogate_n_comb` (31) must be a positive integer less than or equal to `n_coalitions` minus two (30).

---

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, phi0 = p0, seed = 1, approach = "regression_surrogate",
        regression.surrogate_n_comb = 0, iterative = FALSE)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Condition
      Error in `regression.check_sur_n_comb()`:
      ! `regression.surrogate_n_comb` (0) must be a positive integer less than or equal to `n_coalitions` minus two (30).

