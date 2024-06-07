# asymmetric erroneous input: `causal_ordering`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list(1:6), confounding = NULL, approach = "gaussian", )
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list(1:5, 5), confounding = NULL, approach = "gaussian", )
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list(2:5, 5), confounding = NULL, approach = "gaussian", )
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list(1:2, 4), confounding = NULL, approach = "gaussian", )
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Month", "Day", "FAKE"),
        confounding = NULL, approach = "gaussian", )
    Condition
      Error in `convert_feature_name_to_idx()`:
      ! `causal_ordering` contains feature names that are not in the data (`FAKE`).

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Month", "Day", "Day"),
        confounding = NULL, approach = "gaussian", )
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind", "Temp", "Day", "Day"), confounding = NULL,
        approach = "gaussian", )
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list("Solar.R", "Wind"), confounding = NULL, approach = "gaussian",
        )
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list(c("Solar.R", "Wind", "Temp", "Month"), "Day"),
        confounding = NULL, approach = "gaussian", group = list(A = c("Solar.R",
          "Wind"), B = "Temp", C = c("Month", "Day")))
    Condition
      Error in `convert_feature_name_to_idx()`:
      ! `causal_ordering` contains feature names that are not in the data (`Solar.R`, `Wind`, `Temp`, `Month`, `Day`).

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 1, timing = FALSE, asymmetric = TRUE,
        causal_ordering = list(c("A"), "B"), confounding = NULL, approach = "gaussian",
        group = list(A = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day")))
    Condition
      Error in `check_and_set_causal_ordering()`:
      ! `causal_ordering` is incomplete/incorrect. It must contain all features or feature indices exactly once.

# asymmetric erroneous input: `approach`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 3, timing = FALSE, asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5), confounding = TRUE, approach = c(
          "gaussian", "independence", "empirical", "gaussian"), )
    Condition
      Error in `check_and_set_causal_sampling()`:
      ! Causal Shapley values is not applicable for combined approaches.

# asymmetric erroneous input: `asymmetric`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 3, timing = FALSE, asymmetric = c(FALSE,
          FALSE), causal_ordering = list(1:2, 3:4, 5), confounding = TRUE, approach = "gaussian")
    Condition
      Error in `get_parameters()`:
      ! `asymmetric` must be a single logical.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 3, timing = FALSE, asymmetric = "Must be logical",
        causal_ordering = list(1:2, 3:4, 5), confounding = TRUE, approach = "gaussian")
    Condition
      Error in `get_parameters()`:
      ! `asymmetric` must be a single logical.

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 3, timing = FALSE, asymmetric = 1L,
        causal_ordering = list(1:2, 3:4, 5), confounding = TRUE, approach = "gaussian")
    Condition
      Error in `get_parameters()`:
      ! `asymmetric` must be a single logical.

# asymmetric erroneous input: `confounding`

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 3, timing = FALSE, asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5), confounding = c("A", "B", "C"),
        approach = "gaussian", )
    Condition
      Error in `get_parameters()`:
      ! `confounding` must be a logical (vector).

---

    Code
      explain(model = model_lm_numeric, x_explain = x_explain_numeric, x_train = x_train_numeric,
        prediction_zero = p0, n_batches = 3, timing = FALSE, asymmetric = FALSE,
        causal_ordering = list(1:2, 3:4, 5), confounding = c(TRUE, FALSE), approach = "gaussian",
        )
    Condition
      Error in `check_and_set_confounding()`:
      ! `confounding` must either be a single logical or a vector of logicals of the same length as the number of components in `causal_ordering` (3).

