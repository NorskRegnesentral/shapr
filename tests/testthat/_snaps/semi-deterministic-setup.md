# semi_deterministic_sampling: not paired sampling

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "independence", phi0 = p0, seed = 1,
        extra_computation_args = list(paired_shap_sampling = FALSE,
          semi_deterministic_sampling = TRUE))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Error in `check_and_set_sampling_info()`:
      ! `paired_shap_sampling` cannot be FALSE when `semi_deterministic_sampling` is TRUE.

# semi_deterministic_sampling: not regular sampling

    Code
      explain_forecast(testing = TRUE, model = model_ar_temp, y = data_arima[, "Temp"],
      train_idx = 2:151, explain_idx = 152:153, explain_y_lags = 2, horizon = 3,
      approach = "empirical", phi0 = p0_ar, seed = 1, group_lags = FALSE,
      extra_computation_args = list(paired_shap_sampling = TRUE,
        semi_deterministic_sampling = TRUE))
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 4`, and is therefore set to `2^n_features = 4`.
    Condition
      Error in `check_and_set_sampling_info()`:
      ! `semi_deterministic_sampling` is not supported for explain_forecast().

# semi_deterministic_sampling: not symmetric sampling

    Code
      explain(testing = TRUE, model = model_lm_numeric, x_explain = x_explain_numeric,
        x_train = x_train_numeric, approach = "gaussian", phi0 = p0, seed = 1,
        asymmetric = TRUE, causal_ordering = list(1:2, 3, 4:5), confounding = NULL,
        extra_computation_args = list(paired_shap_sampling = TRUE,
          semi_deterministic_sampling = TRUE))
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
    Condition
      Error in `set_extra_comp_params()`:
      ! Set `paired_shap_sampling = FALSE` to compute asymmetric Shapley values. Asymmetric Shapley values do not support paired sampling as the paired coalitions will not necessarily respect the causal ordering.

# setup_semi_determ_n_determ_sample_coal_18

    Code
      print({
        out <- code
      }, digits = digits)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -3.389   7.949 14.864 -4.626 -2.196
      2:          2 42.44   3.083  -3.561 -4.635 -6.028 -2.738
      3:          3 42.44   3.732 -18.903 -1.043 -3.556  2.202

# setup_semi_determ_n_determ_sample_coal_20

    Code
      print({
        out <- code
      }, digits = digits)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.331   7.521 17.475 -5.006 -3.057
      2:          2 42.44   2.873  -4.405 -4.707 -4.967 -2.673
      3:          3 42.44   3.354 -18.354 -1.828 -2.822  2.082

