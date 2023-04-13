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

