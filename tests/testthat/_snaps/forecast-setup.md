# error with custom model without providing predict_model

    Code
      model_custom_arima_temp <- model_arima_temp
      class(model_custom_arima_temp) <- "whatever"
      explain_forecast(model = model_custom_arima_temp, y = data[1:150, "Temp"],
      xreg = data[, "Wind"], train_idx = 2:148, explain_idx = 149:150,
      explain_y_lags = 2, explain_xreg_lags = 2, horizon = 3, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Error <simpleError>
      You passed a model to explain() which is not natively supported, and did not supply the 'predict_model' function to explain().
      See ?shapr::explain or the vignette for more information on how to run shapr with custom models.

# erroneous input: `x_train/x_explain`

    Code
      y_wrong_format <- data[, c("Temp", "Wind")]
      explain_forecast(model = model_arima_temp, y = y_wrong_format, xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Error <simpleError>
      Each data column must have a lag order set in lags$data.

---

    Code
      xreg_wrong_format <- data[, c("Temp", "Wind")]
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = xreg_wrong_format,
      train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Error <simpleError>
      Each reg column must have a lag order set in lags$reg.

---

    Code
      xreg_no_column_names <- data[, "Wind"]
      names(xreg_no_column_names) <- NULL
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = xreg_no_column_names,
      train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_ar,
      n_batches = 1)
    Error <simpleError>
      attempt to select less than one element in get1index

# erroneous input: `model`

    Code
      explain_forecast(y = data[1:150, "Temp"], xreg = data[, "Wind"], train_idx = 2:
        148, explain_idx = 149:150, explain_y_lags = 2, explain_xreg_lags = 2,
      horizon = 3, approach = "independence", prediction_zero = p0_ar, n_batches = 1)
    Error <simpleError>
      argument "model" is missing, with no default

# erroneous input: `prediction_zero`

    Code
      p0_wrong_length <- p0_ar[1:2]
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = 2,
      explain_xreg_lags = 2, horizon = 3, approach = "independence", prediction_zero = p0_wrong_length,
      n_batches = 1)
    Error <simpleError>
      `prediction_zero` must match the output size of the model (3).

# erroneous input: `n_combinations`

    Code
      horizon = 3
      explain_y_lags = 2
      explain_xreg_lags = 2
      n_combinations = horizon + explain_y_lags + explain_xreg_lags - 1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = explain_y_lags,
      explain_xreg_lags = explain_xreg_lags, horizon = horizon, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1, n_combinations = n_combinations,
      group_lags = FALSE)
    Message <simpleMessage>
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Error <simpleError>
      `n_combinations` has to be greater than the number of features.

---

    Code
      horizon = 3
      explain_y_lags = 2
      explain_xreg_lags = 2
      n_combinations = 1 + 1
      explain_forecast(model = model_arima_temp, y = data[1:150, "Temp"], xreg = data[,
        "Wind"], train_idx = 2:148, explain_idx = 149:150, explain_y_lags = explain_y_lags,
      explain_xreg_lags = explain_xreg_lags, horizon = horizon, approach = "independence",
      prediction_zero = p0_ar, n_batches = 1, n_combinations = n_combinations,
      group_lags = TRUE)
    Message <simpleMessage>
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Error <simpleError>
      `n_combinations` has to be greater than the number of groups.

