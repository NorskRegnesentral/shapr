# regression erroneous input: `approach`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = c(
          "regression_surrogate", "gaussian", "independence", "empirical"), )
    Condition
      Error in `check_approach()`:
      ! The `regression_separate` and `regression_surrogate` approaches cannot be combined with other approaches.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = c(
          "regression_separate", "gaussian", "independence", "empirical"), )
    Condition
      Error in `check_approach()`:
      ! The `regression_separate` and `regression_surrogate` approaches cannot be combined with other approaches.

# regression erroneous input: `regression_model`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = NULL)
    Condition
      Error in `get_regression_tune()`:
      ! `regression_model` must be a tidymodels object with class 'model_spec'. See documentation.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = lm)
    Condition
      Error in `get_regression_tune()`:
      ! `regression_model` must be a tidymodels object with class 'model_spec'. See documentation.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"))
    Condition
      Error in `get_regression_tune()`:
      ! `regression_tune_values` must be provided when `regression_model` contains hyperparameters to tune.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"), regression_tune_values = data.frame(num_terms = c(1, 2,
          3)))
    Condition
      Error in `get_regression_tune()`:
      ! The tunable parameters in `regression_model` ('tree_depth') and `regression_tune_values` ('num_terms') must match.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"), regression_tune_values = data.frame(tree_depth = c(1, 2,
          3), num_terms = c(1, 2, 3)))
    Condition
      Error in `get_regression_tune()`:
      ! The tunable parameters in `regression_model` ('tree_depth') and `regression_tune_values` ('tree_depth', 'num_terms') must match.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = 2, engine = "rpart",
          mode = "regression"), regression_tune_values = data.frame(tree_depth = c(1,
          2, 3)))
    Condition
      Error in `get_regression_tune()`:
      ! The tunable parameters in `regression_model` ('') and `regression_tune_values` ('tree_depth') must match.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_surrogate",
        regression_tune_values = data.frame(tree_depth = c(1, 2, 3)))
    Condition
      Error in `get_regression_tune()`:
      ! The tunable parameters in `regression_model` ('') and `regression_tune_values` ('tree_depth') must match.

# regression erroneous input: `regression_tune_values`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = 2, engine = "rpart",
          mode = "regression"), regression_tune_values = as.matrix(data.frame(
          tree_depth = c(1, 2, 3))))
    Condition
      Error in `get_regression_tune()`:
      ! `regression_tune_values` must be of either class `data.frame` or `function`. See documentation.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"), regression_tune_values = function(x) c(1, 2, 3))
    Condition
      Error in `get_regression_tune()`:
      ! The output of the user provided `regression_tune_values` function must be of class `data.frame`.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"), regression_tune_values = function(x) data.frame(
          wrong_name = c(1, 2, 3)))
    Condition
      Error in `get_regression_tune()`:
      ! The tunable parameters in `regression_model` ('tree_depth') and `regression_tune_values` ('wrong_name') must match.

# regression erroneous input: `regression_vfold_cv_para`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"), regression_tune_values = data.frame(tree_depth = c(1, 2,
          3)), regression_vfold_cv_para = 10)
    Condition
      Error in `check_regression_vfold_cv_para()`:
      ! `regression_vfold_cv_para` must be a named list. See documentation using '?shapr::explain()'.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"), regression_tune_values = data.frame(tree_depth = c(1, 2,
          3)), regression_vfold_cv_para = list(10))
    Condition
      Error in `check_regression_vfold_cv_para()`:
      ! `regression_vfold_cv_para` must be a named list. See documentation using '?shapr::explain()'.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_model = parsnip::decision_tree(tree_depth = tune(), engine = "rpart",
        mode = "regression"), regression_tune_values = data.frame(tree_depth = c(1, 2,
          3)), regression_vfold_cv_para = list(hey = 10))
    Condition
      Error in `check_regression_vfold_cv_para()`:
      ! The following parameters in `regression_vfold_cv_para` are not supported by `rsample::vfold_cv()`: 'hey'.

# regression erroneous input: `regression_recipe_func`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_separate",
        regression_recipe_func = 3)
    Condition
      Error in `check_regression_recipe_func()`:
      ! `regression_recipe_func` must be a function. See documentation.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_surrogate",
        regression_recipe_func = function(x) {
          return(2)
        })
    Condition
      Error in `check_regression_recipe_func()`:
      ! The output of the `regression_recipe_func` must be of class `recipe`.

# regression erroneous input: `regression_surr_n_comb`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_surrogate",
        regression_surr_n_comb = 2^ncol(x_explain_numeric) - 1)
    Condition
      Error in `check_regression_n_comb()`:
      ! `regression_surr_n_comb` (31) must be a positive integer less than or equal to `used_n_combinations` minus two (30).

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, approach = "regression_surrogate",
        regression_surr_n_comb = 0)
    Condition
      Error in `check_regression_n_comb()`:
      ! `regression_surr_n_comb` (0) must be a positive integer less than or equal to `used_n_combinations` minus two (30).

