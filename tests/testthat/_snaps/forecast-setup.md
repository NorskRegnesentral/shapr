# error with custom model without providing predict_model

    Code
      model_custom_arima_temp <- model_arima_temp
      class(model_custom_arima_temp) <- "whatever"
      explain_forecast(testing = TRUE, model = model_custom_arima_temp, y = data_arima[
        1:150, "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:
        150, explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i You passed a model to `shapr::explain()` which is not natively supported, and did not supply a `get_model_specs` function to `shapr::explain()`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
    Condition
      Error in `get_predict_model()`:
      ! You passed a model to `shapr::explain()` that is not natively supported and did not supply a 'predict_model' function to `shapr::explain()`. See the documentation of `shapr::explain()` or the `vignette(shapr::general_usage)` vignette for more information on how to run shapr with custom models.

# erroneous input: `x_train/x_explain`

    Code
      y_wrong_format <- data_arima[, c("Temp", "Wind")]
      explain_forecast(testing = TRUE, model = model_arima_temp, y = y_wrong_format,
        xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
        explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
        phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! `y` has 2 columns (Temp,Wind). `explain_y_lags` has length 1. These two should match.

---

    Code
      xreg_wrong_format <- data_arima[, c("Temp", "Wind")]
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = xreg_wrong_format, train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! `xreg` has 2 columns (Temp,Wind). `explain_xreg_lags` has length 1. These two should match.

---

    Code
      xreg_no_column_names <- data_arima[, "Wind"]
      names(xreg_no_column_names) <- NULL
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = xreg_no_column_names, train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! `xreg` is missing column names.

# erroneous input: `model`

    Code
      explain_forecast(testing = TRUE, y = data_arima[1:150, "Temp"], xreg = data_arima[
        , "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", phi0 = p0_ar,
      seed = 1)
    Condition
      Error in `explain_forecast()`:
      ! argument "model" is missing, with no default

# erroneous input: `phi0`

    Code
      p0_wrong_length <- p0_ar[1:2]
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      phi0 = p0_wrong_length, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `phi0` (77.8823529411765, 77.8823529411765) must be numeric and match the output size of the model (3).

# erroneous input: `max_n_coalitions`

    Code
      horizon <- 3
      explain_y_lags <- 2
      explain_xreg_lags <- 2
      n_coalitions <- horizon + explain_y_lags + explain_xreg_lags - 1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = explain_y_lags, explain_xreg_lags = explain_xreg_lags,
      horizon = horizon, approach = "independence", phi0 = p0_ar, seed = 1,
      max_n_coalitions = n_coalitions, group_lags = FALSE, iterative_args = list(
        initial_n_coalitions = 20))
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is smaller than `max(10, n_features + 1 = 8)`, which will result in unreliable results.
        It is therefore set to 8.
    Condition
      Error in `check_iterative_args()`:
      ! `iterative_args$initial_n_coalitions` must be a single integer between 2 and `max_n_coalitions`.

---

    Code
      horizon <- 3
      explain_y_lags <- 2
      explain_xreg_lags <- 2
      n_coalitions <- 1 + 1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = explain_y_lags, explain_xreg_lags = explain_xreg_lags,
      horizon = horizon, approach = "independence", phi0 = p0_ar, seed = 1,
      max_n_coalitions = n_coalitions, group_lags = TRUE)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `n_groups` is smaller than or equal to 3, meaning there are so few unique coalitions (4) that we should use all to get reliable results.
        `max_n_coalitions` is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 2
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 4 of 4 coalitions. 
    Output
         explain_idx horizon  none  Temp   Wind
               <int>   <int> <num> <num>  <num>
      1:         149       1  77.9 -8.25 -2.256
      2:         150       1  77.9 -2.98 -2.659
      3:         149       2  77.9 -8.25 -2.632
      4:         150       2  77.9 -2.98 -0.499
      5:         149       3  77.9 -8.26 -0.470
      6:         150       3  77.9 -2.98 -1.695

# erroneous input: `train_idx`

    Code
      train_idx_too_short <- 2
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = train_idx_too_short,
      explain_idx = 149:150, explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `train_idx` must be a vector of positive finite integers and length > 1.

---

    Code
      train_idx_not_integer <- c(3:5) + 0.1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = train_idx_not_integer,
      explain_idx = 149:150, explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `train_idx` must be a vector of positive finite integers and length > 1.

---

    Code
      train_idx_out_of_range <- 1:5
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = train_idx_out_of_range,
      explain_idx = 149:150, explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! The train (`train_idx`) and explain (`explain_idx`) indices must fit in the lagged data. The lagged data begins at index 2 and ends at index 150.

# erroneous input: `explain_idx`

    Code
      explain_idx_not_integer <- c(3:5) + 0.1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = explain_idx_not_integer,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `explain_idx` must be a vector of positive finite integers.

---

    Code
      explain_idx_out_of_range <- 1:5
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = explain_idx_out_of_range,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! The train (`train_idx`) and explain (`explain_idx`) indices must fit in the lagged data. The lagged data begins at index 2 and ends at index 150.

# erroneous input: `explain_y_lags`

    Code
      explain_y_lags_negative <- -1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = explain_y_lags_negative, explain_xreg_lags = 2, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `explain_y_lags` must be a vector of positive finite integers.

---

    Code
      explain_y_lags_not_integer <- 2.1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = explain_y_lags_not_integer, explain_xreg_lags = 2, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `explain_y_lags` must be a vector of positive finite integers.

---

    Code
      explain_y_lags_more_than_one <- c(1, 2)
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = explain_y_lags_more_than_one, explain_xreg_lags = 2, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! `y` has 1 columns (Temp). `explain_y_lags` has length 2. These two should match.

---

    Code
      explain_y_lags_zero <- 0
      explain_forecast(testing = TRUE, model = model_arima_temp_noxreg, y = data_arima[
        1:150, "Temp"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 0,
      horizon = 3, approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! `explain_y_lags=0` is not allowed for models without exogeneous variables.

# erroneous input: `explain_x_lags`

    Code
      explain_xreg_lags_negative <- -2
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = explain_xreg_lags_negative, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `explain_xreg_lags` must be a vector of positive finite integers.

---

    Code
      explain_xreg_lags_not_integer <- 2.1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = explain_xreg_lags_not_integer, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `explain_xreg_lags` must be a vector of positive finite integers.

---

    Code
      explain_x_lags_wrong_length <- c(1, 2)
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = explain_x_lags_wrong_length, horizon = 3,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_data_forecast()`:
      ! `xreg` has 1 columns (Wind). `explain_xreg_lags` has length 2. These two should match.

# erroneous input: `horizon`

    Code
      horizon_negative <- -2
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = horizon_negative,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `horizon` must be a vector (or scalar) of positive integers.

---

    Code
      horizon_not_integer <- 2.1
      explain_forecast(testing = TRUE, model = model_arima_temp, y = data_arima[1:150,
      "Temp"], xreg = data_arima[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = horizon_not_integer,
      approach = "independence", phi0 = p0_ar, seed = 1)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
    Condition
      Error in `get_parameters()`:
      ! `horizon` must be a vector (or scalar) of positive integers.

