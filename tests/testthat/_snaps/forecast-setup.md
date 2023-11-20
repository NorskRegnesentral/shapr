# error with custom model without providing predict_model

    Code
      model_custom_arima_temp <- model_arima_temp
      class(model_custom_arima_temp) <- "whatever"
      explain_forecast(model = model_custom_arima_temp, y = data[1:150, "Temp"],
      xreg = data[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Message
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Condition
      Error in `get_predict_model()`:
      ! You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' function to explain().
      See ?shapr::explain or the vignette for more information on how to run shapr with custom models.

# erroneous input: `x_train/x_explain`

    Code
      y_wrong_format <- data[, c("Temp", "Wind")]
      explain_forecast(model = model_arima_temp, y = y_wrong_format, xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! `y` has 2 columns (Temp,Wind).
      `explain_y_lags` has length 1.
      These two should match.

---

    Code
      xreg_wrong_format <- data[, c("Temp", "Wind")]
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = xreg_wrong_format,
      train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! `xreg` has 2 columns (Temp,Wind).
      `explain_xreg_lags` has length 1.
      These two should match.

---

    Code
      xreg_no_column_names <- data[, "Wind"]
      names(xreg_no_column_names) <- NULL
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = xreg_no_column_names,
      train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! `xreg` misses column names.

# erroneous input: `model`

    Code
      explain_forecast(y = data[1:150, "Temp"], xreg = data[, "Wind"], train_idx = 2:
        148, explain_idx = 149:150, explain_y_lags = 2, explain_xreg_lags = 2,
      horizon = 3, approach = "independence", prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `explain_forecast()`:
      ! argument "model" is missing, with no default

# erroneous input: `prediction_zero`

    Code
      p0_wrong_length <- p0_ar[1:2]
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_wrong_length,
      n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `prediction_zero` (77.8823529411765, 77.8823529411765) must be numeric and match the output size of the model (3).

# erroneous input: `n_combinations`

    Code
      horizon <- 3
      explain_y_lags <- 2
      explain_xreg_lags <- 2
      n_combinations <- horizon + explain_y_lags + explain_xreg_lags - 1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = explain_y_lags,
      explain_xreg_lags = explain_xreg_lags, horizon = horizon, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1, n_combinations = n_combinations,
      group_lags = FALSE)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Condition
      Error in `check_n_combinations()`:
      ! `n_combinations` (6) has to be greater than the number of components to decompose  the forecast onto:
      `horizon` (3) + `explain_y_lags` (2) + sum(`explain_xreg_lags`) (2).

---

    Code
      horizon <- 3
      explain_y_lags <- 2
      explain_xreg_lags <- 2
      n_combinations <- 1 + 1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = explain_y_lags,
      explain_xreg_lags = explain_xreg_lags, horizon = horizon, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1, n_combinations = n_combinations,
      group_lags = TRUE)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Condition
      Error in `check_n_combinations()`:
      ! `n_combinations` (2) has to be greater than the number of components to decompose the forecast onto:
      ncol(`xreg`) (1) + 1

# erroneous input: `train_idx`

    Code
      train_idx_too_short <- 2
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = train_idx_too_short, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `train_idx` must be a vector of positive finite integers and length > 1.

---

    Code
      train_idx_not_integer <- c(3:5) + 0.1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = train_idx_not_integer, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `train_idx` must be a vector of positive finite integers and length > 1.

---

    Code
      train_idx_out_of_range <- 1:5
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = train_idx_out_of_range, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! The train (`train_idx`) and explain (`explain_idx`) indices must fit in the lagged data.
      The lagged data begins at index 2 and ends at index 150.

# erroneous input: `explain_idx`

    Code
      explain_idx_not_integer <- c(3:5) + 0.1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = explain_idx_not_integer,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `explain_idx` must be a vector of positive finite integers.

---

    Code
      explain_idx_out_of_range <- 1:5
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = explain_idx_out_of_range,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! The train (`train_idx`) and explain (`explain_idx`) indices must fit in the lagged data.
      The lagged data begins at index 2 and ends at index 150.

# erroneous input: `explain_y_lags`

    Code
      explain_y_lags_negative <- -1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = explain_y_lags_negative,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `explain_y_lags` must be a vector of positive finite integers.

---

    Code
      explain_y_lags_not_integer <- 2.1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = explain_y_lags_not_integer,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `explain_y_lags` must be a vector of positive finite integers.

---

    Code
      explain_y_lags_more_than_one <- c(1, 2)
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = explain_y_lags_more_than_one,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! `y` has 1 columns (Temp).
      `explain_y_lags` has length 2.
      These two should match.

---

    Code
      explain_y_lags_zero <- 0
      explain_forecast(model = model_arima_temp_noxreg, y = data[1:150, "Temp"],
      train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 0, horizon = 3,
      approach = "independence", prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! `explain_y_lags=0` is not allowed for models without exogeneous variables

# erroneous input: `explain_x_lags`

    Code
      explain_xreg_lags_negative <- -2
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = explain_xreg_lags_negative, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `explain_xreg_lags` must be a vector of positive finite integers.

---

    Code
      explain_xreg_lags_not_integer <- 2.1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = explain_xreg_lags_not_integer, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `explain_xreg_lags` must be a vector of positive finite integers.

---

    Code
      explain_x_lags_wrong_length <- c(1, 2)
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = explain_x_lags_wrong_length, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_data_forecast()`:
      ! `xreg` has 1 columns (Wind).
      `explain_xreg_lags` has length 2.
      These two should match.

# erroneous input: `horizon`

    Code
      horizon_negative <- -2
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = horizon_negative, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `horizon` must be a vector (or scalar) of positive integers.

---

    Code
      horizon_not_integer <- 2.1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = horizon_not_integer, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Condition
      Error in `get_parameters()`:
      ! `horizon` must be a vector (or scalar) of positive integers.

